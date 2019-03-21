import {GrammarDeclaration, RuleDeclaration, PrecDeclaration, Expression, NamedExpression} from "./node"
import {Term, TermSet, Precedence, Rule, Grammar} from "./grammar"
import {Input} from "./parse"

// FIXME add inlining other other grammar simplifications?

const none: ReadonlyArray<any> = []

// Union type for arguments—precisely one of the fields is null in any
// instance.
class Arg {
  constructor(readonly operator: NamedExpression | null, // Always has zero arguments
              readonly value: Term[] | null) {}

  static make(expr: Expression, cx: Context): Arg {
    if (expr.type == "NamedExpression" && expr.args.length == 0) {
      let param = expr.namespace ? -1 : cx.params.indexOf(expr.id.name)
      return param > -1 ? cx.args[param] : new Arg(expr, null)
    }
    return new Arg(null, cx.normalizeExpr(expr))
  }
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

  defineRule(name: Term, parts: Term[]) {
    this.b.rules.push(new Rule(name, parts, this.precedence))
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
      if (arg.operator)
        return this.resolve(new NamedExpression(expr.start, expr.end, arg.operator.namespace,
                                                arg.operator.id, expr.args))
      if (expr.args.length)
        this.raise(`Passing arguments to a by-value rule parameter`, expr.args[0].start)
      return arg.value!
    } else {
      let known = this.b.ast.rules.find(r => r.id.name == expr.id.name)
      if (!known)
        return this.raise(`Reference to undefined rule '${expr.id.name}'`, expr.start)
      if (known.params.length != expr.args.length)
        this.raise(`Wrong number or arguments for '${expr.id.name}'`, expr.start)
      if (expr.args.length == 0) return [this.b.terms.getNonTerminal(expr.id.name)]

      // FIXME avoid generating the same rules multiple times
      let name = this.b.newName(expr.id.name, true)
      let paramCx = new Context(this.b, known, this.precedence,
                                known.params.map(p => p.name),
                                expr.args.map(a => Arg.make(a, this)))
      for (let choice of paramCx.normalizeTopExpr(known.expr))
        this.defineRule(name, choice)
      return [name]
    }
  }

  normalizeTopExpr(expr: Expression): Term[][] {
    if (expr.type == "RepeatExpression" && expr.kind == "?") {
      return [[], ...this.normalizeTopExpr(expr.expr)]
    } else if (expr.type == "RepeatExpression" && expr.kind == "*") {
      return [[], [this.b.terms.getNonTerminal(this.rule.id.name), ...this.normalizeExpr(expr.expr)]]
    } else if (expr.type == "ChoiceExpression") {
      return expr.exprs.map(e => this.normalizeExpr(e))
    } else {
      return [this.normalizeExpr(expr)]
    }
  }

  normalizeExpr(expr: Expression): Term[] {
    if (expr.type == "RepeatExpression") {
      if (expr.kind == "?") {
        let name = this.newName()
        this.defineRule(name, this.normalizeExpr(expr.expr))
        this.defineRule(name, [])
        return [name]
      } else {
        // FIXME is the duplication for + a good idea? Could also
        // factor expr into a rule.
        let name = this.newName()
        let inner = this.normalizeExpr(expr.expr)
        this.defineRule(name, inner)
        this.defineRule(name, [])
        return expr.kind == "*" ? [name] : inner.concat(name)
      }
    } else if (expr.type == "ChoiceExpression") {
      let name = this.newName()
      for (let choice of expr.exprs)
        this.defineRule(name, this.normalizeExpr(choice))
      return [name]
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

  withPrecedence(prec: Precedence) {
    return new Context(this.b, this.rule, prec, this.params, this.args)
  }
}

class Builder {
  ast: GrammarDeclaration
  input: Input
  terms = new TermSet()
  rules: Rule[] = []
  namespaces: {[name: string]: Namespace} = Object.create(null)

  constructor(text: string, fileName: string | null = null) {
    this.input = new Input(text, fileName)
    this.ast = this.input.parse()

    this.defineNamespace("Conflict", new ConflictNamespace)
    for (let prec of this.ast.precedences)
      this.defineNamespace(prec.id.name, new PrecNamespace(prec), prec.id.start)

    if (!this.ast.rules.some(r => r.id.name == "program" && r.params.length == 0))
      this.input.raise(`Missing 'program' rule declaration`)
    for (let rule of this.ast.rules) {
      if (this.ast.rules.find(r => r != rule && r.id.name == rule.id.name))
        this.input.raise(`Duplicate rule definition for '${rule.id.name}'`, rule.id.start)
      if (this.namespaces[rule.id.name])
        this.input.raise(`Rule name '${rule.id.name}' conflicts with a defined namespace`, rule.id.start)
      if (rule.params.length == 0) {
        let name = this.terms.getNonTerminal(rule.id.name)
        let cx = new Context(this, rule)
        for (let choice of cx.normalizeTopExpr(rule.expr)) cx.defineRule(name, choice)
      }
    }
  }

  defineNamespace(name: string, value: Namespace, pos: number = 0) {
    if (this.namespaces[name]) this.input.raise(`Duplicate definition of namespace '${name}'`, pos)
    this.namespaces[name] = value
  }

  newName(base: string, forceID = true): Term {
    for (let i = forceID ? 1 : 0;; i++) {
      let name = i ? `${base}-${i}` : base
      if (!this.terms.nonTerminals.some(t => t.name == name))
        return this.terms.getNonTerminal(name)
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
    let name = cx.b.newName(`${expr.namespace!.name}-${expr.id.name}`, false)
    cx = cx.withPrecedence(new Precedence(this.ast.assoc[found], this.id, found))
    cx.defineRule(name, cx.normalizeExpr(expr.args[0]))
    return [name]
  }
}

class ConflictNamespace implements Namespace {
  groups: {[name: string]: number} = Object.create(null)

  resolve(expr: NamedExpression, cx: Context): Term[] {
    if (expr.args.length != 1)
      cx.raise(`Conflict specifiers take a single argument`, expr.start)
    let group = this.groups[expr.id.name] || (this.groups[expr.id.name] = precID++)
    let name = cx.b.newName(`Conflict-${expr.id.name}`, false)
    cx = cx.withPrecedence(new Precedence(null, group, -1))
    cx.defineRule(name, cx.normalizeExpr(expr.args[0]))
    return [name]
  }
}

export function buildGrammar(text: string, fileName: string | null = null) {
  let builder = new Builder(text, fileName)
  return new Grammar(builder.rules, builder.terms)
}
