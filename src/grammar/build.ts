import {GrammarDeclaration, RuleDeclaration, PrecDeclaration, Expression, NamedExpression, exprsEq} from "./node"
import {Term, TermSet, Precedence, Rule, Grammar} from "./grammar"
import {Input} from "./parse"

// FIXME add inlining other other grammar simplifications?

const none: ReadonlyArray<any> = []

class Arg {
  constructor(readonly expr: NamedExpression | null,
              readonly value: Term[] | null) {}
}

class Context {
  constructor(readonly b: Builder,
              readonly rule: RuleDeclaration,
              readonly precedence: Precedence | null = null,
              readonly params: ReadonlyArray<string> = none,
              readonly args: ReadonlyArray<Arg> = none) {}

  newName() {
    return this.b.newName(this.rule.id.name)
  }

  defineRule(name: Term, choices: Term[][]) {
    for (let choice of choices)
      this.b.rules.push(new Rule(name, choice, this.precedence))
    return [name]
  }

  resolve(expr: NamedExpression): Term[] {
    let param
    if (expr.namespace) {
      let ns = this.b.namespaces[expr.namespace.name]
      if (!ns)
        this.raise(`Reference to undefined namespace '${expr.namespace.name}'`, expr.start)
      return ns.resolve(expr, this)
    } else if ((param = this.params.indexOf(expr.id.name)) > -1) {
      let arg = this.args[param]
      if (arg.expr)
        return this.resolve(new NamedExpression(expr.start, expr.end, arg.expr.namespace, arg.expr.id, expr.args))
      if (expr.args.length)
        this.raise(`Passing arguments to a by-value rule parameter`, expr.args[0].start)
      return arg.value!
    } else {
      let innerPrec = expr.args.length ? this.precedence : null
      if (!expr.args.some(e => e.containsNames(this.params))) for (let built of this.b.built) {
        if (built.id == expr.id.name &&
            (built.precedence ? innerPrec && built.precedence.eq(innerPrec) : !innerPrec) &&
            exprsEq(expr.args, built.args))
          return [built.name]
      }

      let known = this.b.ast.rules.find(r => r.id.name == expr.id.name)
      if (!known)
        return this.raise(`Reference to undefined rule '${expr.id.name}'`, expr.start)
      if (known.params.length != expr.args.length)
        this.raise(`Wrong number or arguments for '${expr.id.name}'`, expr.start)
      return this.buildRule(known, expr.args)
    }
  }

  resolveArg(e: Expression) {
    if (e.type == "NamedExpression" && e.args.length == 0) {
      let param = e.namespace ? -1 : this.params.indexOf(e.id.name)
      return param > -1 ? this.args[param] : new Arg(e, null)
    }
    return new Arg(null, this.normalizeExpr(e))
  }

  normalizeTopExpr(expr: Expression, self: Term): Term[][] {
    if (expr.type == "RepeatExpression" && expr.kind == "?")
      return [[], ...this.normalizeTopExpr(expr.expr, self)]
    else if (expr.type == "RepeatExpression" && expr.kind == "*")
      return [[], [self, ...this.normalizeExpr(expr.expr)]]
    else if (expr.type == "ChoiceExpression")
      return expr.exprs.map(e => this.normalizeExpr(e))
    else
      return [this.normalizeExpr(expr)]
  }

  normalizeExpr(expr: Expression): Term[] {
    if (expr.type == "RepeatExpression") {
      if (expr.kind == "?") {
        let name = this.newName()
        return this.defineRule(this.newName(), [[] as Term[]].concat(this.normalizeTopExpr(expr.expr, name)))
      } else {
        // FIXME is the duplication for + a good idea? Could also
        // factor expr into a rule.
        let inner = this.normalizeExpr(expr.expr), name = this.newName()
        let result = this.defineRule(name, [[name].concat(inner), []])
        return expr.kind == "*" ? result : inner.concat(result)
      }
    } else if (expr.type == "ChoiceExpression") {
      return this.defineRule(this.newName(), expr.exprs.map(e => this.normalizeExpr(e)))
    } else if (expr.type == "SequenceExpression") {
      return expr.exprs.reduce((a, e) => a.concat(this.normalizeExpr(e)), [] as Term[])
    } else if (expr.type == "LiteralExpression") {
      return expr.value ? [this.b.terms.getTerminal(expr.value)] : []
    } else if (expr.type == "NamedExpression") {
      return this.resolve(expr)
    } else {
      return this.raise("Unhandled expression type " + expr.type, expr.start)
    }
  }

  raise(message: string, pos: number = -1): never {
    return this.b.input.raise(message, pos)
  }

  withPrecedence(prec: Precedence | null) {
    return new Context(this.b, this.rule, prec, this.params, this.args)
  }

  buildRule(rule: RuleDeclaration, args: ReadonlyArray<Expression>): Term[] {
    let cx = new Context(this.b, rule, args.length ? this.precedence : null,
                         rule.params.map(p => p.name), args.map(a => this.resolveArg(a)))
    let name = this.b.newName(rule.id.name, isTag(rule.id.name) || true)
    if (!args.some(a => a.containsNames(this.params)))
      this.b.built.push(new BuiltRule(rule.id.name, args, cx.precedence, name))
    return cx.defineRule(name, cx.normalizeTopExpr(rule.expr, name))
  }
}

function isTag(name: string) {
  let ch0 = name[0]
  return ch0.toUpperCase() == ch0 && ch0 != "_" ? name : null
}

class BuiltRule {
  constructor(readonly id: string,
              readonly args: ReadonlyArray<Expression>,
              readonly precedence: Precedence | null,
              readonly name: Term) {}
}

class Builder {
  ast: GrammarDeclaration
  input: Input
  terms = new TermSet()
  rules: Rule[] = []
  built: BuiltRule[] = []
  namespaces: {[name: string]: Namespace} = Object.create(null)

  constructor(text: string, fileName: string | null = null) {
    this.input = new Input(text, fileName)
    this.ast = this.input.parse()

    this.defineNamespace("conflict", new ConflictNamespace)
    this.defineNamespace("tag", new TagNamespace)
    for (let prec of this.ast.precedences)
      this.defineNamespace(prec.id.name, new PrecNamespace(prec), prec.id.start)

    for (let rule of this.ast.rules) {
      if (this.ast.rules.find(r => r != rule && r.id.name == rule.id.name))
        this.input.raise(`Duplicate rule definition for '${rule.id.name}'`, rule.id.start)
      if (this.namespaces[rule.id.name])
        this.input.raise(`Rule name '${rule.id.name}' conflicts with a defined namespace`, rule.id.start)
      if (rule.id.name == "program") {
        if (rule.params.length) this.input.raise(`'program' rules should not take parameters`, rule.id.start)
        new Context(this, rule).buildRule(rule, [])
      }
    }
    if (!this.rules.length)
      this.input.raise(`Missing 'program' rule declaration`)
    for (let rule of this.ast.rules) {
      if (!this.rules.some(r => r.name.name == rule.id.name))
        this.input.raise(`Unused rule '${rule.id.name}'`, rule.start)
    }
  }

  defineNamespace(name: string, value: Namespace, pos: number = 0) {
    if (this.namespaces[name]) this.input.raise(`Duplicate definition of namespace '${name}'`, pos)
    this.namespaces[name] = value
  }

  newName(base: string, tag: string | null | true = null): Term {
    for (let i = tag ? 0 : 1;; i++) {
      let name = i ? `${base}-${i}` : base
      if (!this.terms.nonTerminals.some(t => t.name == name))
        return this.terms.makeNonTerminal(name, tag === true ? null : tag)
    }
  }
}

let precID = 1

interface Namespace {
  resolve(expr: NamedExpression, cx: Context): Term[]
}

class PrecNamespace implements Namespace {
  id: number = precID++

  constructor(readonly ast: PrecDeclaration) {}

  resolve(expr: NamedExpression, cx: Context): Term[] {
    if (expr.args.length != 1)
      cx.raise(`Precedence specifiers take a single argument`, expr.start)
    let found = this.ast.names.findIndex(n => n.name == expr.id.name)
    if (found < 0)
      cx.raise(`Precedence group '${this.ast.id.name}' has no precedence named '${expr.id.name}'`, expr.id.start)
    let name = cx.b.newName(`${expr.namespace!.name}-${expr.id.name}`, true)
    cx = cx.withPrecedence(new Precedence(this.ast.assoc[found], this.id, found))
    return cx.defineRule(name, [cx.normalizeExpr(expr.args[0])])
  }
}

class ConflictNamespace implements Namespace {
  groups: {[name: string]: number} = Object.create(null)

  resolve(expr: NamedExpression, cx: Context): Term[] {
    if (expr.args.length != 1)
      cx.raise(`Conflict specifiers take a single argument`, expr.start)
    let group = this.groups[expr.id.name] || (this.groups[expr.id.name] = precID++)
    let name = cx.b.newName(`conflict-${expr.id.name}`, true)
    cx = cx.withPrecedence(new Precedence(null, group, -1))
    return cx.defineRule(name, cx.normalizeTopExpr(expr.args[0], name))
  }
}

class TagNamespace implements Namespace {
  resolve(expr: NamedExpression, cx: Context): Term[] {
    if (expr.args.length != 1)
      cx.raise(`Tag wrappers take a single argument`, expr.start)
    let tag = expr.id.name
    let name = cx.b.newName(`tag.${tag}`, tag)
    return cx.defineRule(name, cx.normalizeTopExpr(expr.args[0], name))
  }
}

export function buildGrammar(text: string, fileName: string | null = null) {
  let builder = new Builder(text, fileName)
  return new Grammar(builder.rules, builder.terms)
}
