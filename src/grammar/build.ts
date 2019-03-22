import {GrammarDeclaration, RuleDeclaration, PrecDeclaration, TokenGroupDeclaration,
        Expression, Identifier, NamedExpression, exprsEq} from "./node"
import {Term, TermSet, Precedence, Rule, Grammar} from "./grammar"
import {Edge, State} from "./token"
import {Input} from "./parse"

// FIXME add inlining other other grammar simplifications?

const none: ReadonlyArray<any> = []

class Arg {
  constructor(readonly name: string,
              readonly expr: NamedExpression | null,
              readonly value: Term[] | null) {}
}

class Context {
  constructor(readonly b: Builder,
              readonly rule: RuleDeclaration,
              readonly precedence: Precedence | null = null,
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
    let arg
    if (expr.namespace) {
      let ns = this.b.namespaces[expr.namespace.name]
      if (!ns)
        this.raise(`Reference to undefined namespace '${expr.namespace.name}'`, expr.start)
      return ns.resolve(expr, this)
    } else if (arg = this.args.find(a => a.name == expr.id.name)) {
      if (arg.expr)
        return this.resolve(new NamedExpression(expr.start, expr.end, arg.expr.namespace, arg.expr.id, expr.args))
      if (expr.args.length)
        this.raise(`Passing arguments to a by-value rule parameter`, expr.args[0].start)
      return arg.value!
    } else {
      let innerPrec = expr.args.length ? this.precedence : null
      if (!expr.args.some(e => e.containsNames(this.args.map(p => p.name)))) for (let built of this.b.built)
        if (built.matches(expr, innerPrec)) return [built.term]

      for (let tokens of this.b.tokenGroups) {
        let found = tokens.getToken(expr)
        if (found) return [found]
      }

      let known = this.b.ast.rules.find(r => r.id.name == expr.id.name)
      if (!known)
        return this.raise(`Reference to undefined rule '${expr.id.name}'`, expr.start)
      if (known.params.length != expr.args.length)
        this.raise(`Wrong number or arguments for '${expr.id.name}'`, expr.start)
      return this.buildRule(known, expr.args)
    }
  }

  resolveArg(name: string, e: Expression) {
    if (e.type == "NamedExpression" && e.args.length == 0) {
      let found = this.args.find(a => a.name == e.id.name)
      return found ? new Arg(name, found.expr, found.value) : new Arg(name, e, null)
    }
    return new Arg(name, null, this.normalizeExpr(e))
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
    } else if (expr.type == "LiteralExpression") { // FIXME compile as token?
      return expr.value ? [this.b.terms.getTerminal(expr.value)] : []
    } else if (expr.type == "NamedExpression") {
      return this.resolve(expr)
    } else {
      return this.raise("This type of expression may not occur in non-token rules", expr.start)
    }
  }

  raise(message: string, pos: number = -1): never {
    return this.b.input.raise(message, pos)
  }

  withPrecedence(prec: Precedence | null) {
    return new Context(this.b, this.rule, prec, this.args)
  }

  buildRule(rule: RuleDeclaration, args: ReadonlyArray<Expression>): Term[] {
    let cx = new Context(this.b, rule, args.length ? this.precedence : null,
                         args.map((a, i) => this.resolveArg(rule.params[i].name, a)))
    let name = this.b.newName(rule.id.name, isTag(rule.id.name) || true)
    if (!args.some(a => a.containsNames(this.args.map(a => a.name)))) // FIXME don't allocate this array
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
              readonly term: Term) {}

  matches(expr: NamedExpression, prec: Precedence | null = null) {
    return this.id == expr.id.name &&
      (this.precedence ? prec && this.precedence.eq(prec) : !prec) &&
      exprsEq(expr.args, this.args)
  }
}

class Builder {
  ast: GrammarDeclaration
  input: Input
  terms = new TermSet()
  tokenGroups: TokenGroup[] = []
  rules: Rule[] = []
  built: BuiltRule[] = []
  ruleNames: {[name: string]: boolean} = Object.create(null)
  namespaces: {[name: string]: Namespace} = Object.create(null)

  constructor(text: string, fileName: string | null = null) {
    this.input = new Input(text, fileName)
    this.ast = this.input.parse()

    this.defineNamespace("conflict", new ConflictNamespace)
    this.defineNamespace("tag", new TagNamespace)
    for (let prec of this.ast.precedences)
      this.defineNamespace(prec.id.name, new PrecNamespace(prec), prec.id.start)

    for (let tokens of this.ast.tokenGroups)
      this.tokenGroups.push(new TokenGroup(this, tokens))

    for (let rule of this.ast.rules) {
      this.unique(rule.id)
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
        // FIXME should probably be a warning
        this.input.raise(`Unused rule '${rule.id.name}'`, rule.start)
    }
    for (let tokens of this.tokenGroups) tokens.checkUnused()
  }

  unique(id: Identifier) {
    if (this.ruleNames[id.name])
      this.input.raise(`Duplicate definition of rule '${id.name}'`, id.start)
    this.ruleNames[id.name] = true
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

class TokenArg {
  constructor(readonly name: string, readonly expr: Expression, readonly scope: ReadonlyArray<TokenArg>) {}
}

class TokenGroup {
  startState: State = new State
  built: BuiltRule[] = []
  used: {[name: string]: boolean} = Object.create(null)
  building: string[] = [] // Used for recursion check

  constructor(readonly b: Builder, readonly ast: TokenGroupDeclaration) {
    for (let rule of ast.rules) this.b.unique(rule.id)
  }

  getToken(expr: NamedExpression) {
    for (let built of this.built) if (built.matches(expr)) return built.term
    let name = expr.id.name
    let rule = this.ast.rules.find(r => r.id.name == name)
    if (!rule) return null
    let term = this.b.terms.getTerminal(name) // FIXME make sure this is new/unique!
    let end = new State
    end.connect(this.buildRule(rule, expr, this.startState))
    end.accepting.push(term)
    this.built.push(new BuiltRule(name, expr.args, null, term))
    return term
  }

  raise(msg: string, pos: number = -1): never {
    return this.b.input.raise(msg, pos)
  }

  buildRule(rule: RuleDeclaration, expr: NamedExpression, from: State, args: ReadonlyArray<TokenArg> = none): Edge[] {
    if (rule.params.length != expr.args.length)
      this.raise(`Incorrect number of arguments for token '${name}'`, expr.start)
    this.used[rule.id.name] = true
    if (this.building.includes(rule.id.name))
      this.raise(`Recursive token rules: ${this.building.slice(this.building.lastIndexOf(name)).join(" -> ")}`, expr.start)
    this.building.push(rule.id.name)
    let result = this.build(rule.expr, from,
                            expr.args.map((e, i) => new TokenArg(rule!.params[i].name, e, args)))
    this.building.pop()
    return result
  }

  build(expr: Expression, from: State, args: ReadonlyArray<TokenArg>): Edge[] {
    if (expr.type == "NamedExpression") {
      if (expr.namespace) this.b.input.raise(`Unknown namespace '${expr.namespace.name}'`, expr.start)
      let name = expr.id.name, arg = args.find(a => a.name == name)
      if (arg) return this.build(arg.expr, from, arg.scope)
      let rule = this.ast.rules.find(r => r.id.name == name)
      if (!rule) return this.raise(`Reference to rule '${expr.id.name}', which isn't in this token group`, expr.start)
      return this.buildRule(rule, expr, from, args)
    } else if (expr.type == "ChoiceExpression") {
      return expr.exprs.reduce((out, expr) => out.concat(this.build(expr, from, args)), [] as Edge[])
    } else if (expr.type == "SequenceExpression") {
      for (let i = 0;; i++) {
        let next = this.build(expr.exprs[i], from, args)
        if (i == expr.exprs.length - 1) return next
        ;(from = new State).connect(next)
      }
    } else if (expr.type == "RepeatExpression") {
      if (expr.kind == "*") {
        let loop = new State
        from.nullEdge(loop)
        loop.connect(this.build(expr.expr, loop, args))
        return [loop.nullEdge()]
      } else if (expr.kind == "+") {
        let loop = new State
        loop.connect(this.build(expr.expr, from, args))
        loop.connect(this.build(expr.expr, loop, args))
        return [loop.nullEdge()]
      } else { // expr.kind == "?"
        return [from.nullEdge()].concat(this.build(expr.expr, from, args))
      }
    } else if (expr.type == "CharacterRangeExpression") {
      return [from.edge(expr.from.codePointAt(0)!, expr.to.codePointAt(0)! + 1)]
    } else if (expr.type == "LiteralExpression") {
      for (let i = 0;;) {
        let code = expr.value.codePointAt(i)!
        i += code <= 0xffff ? 1 : 2
        if (i < expr.value.length) {
          let next = new State
          from.edge(code, code + 1, next)
          from = next
        } else {
          return [from.edge(code, code + 1)]
        }
      }
    } else if (expr.type == "AnyExpression") {
      return [from.edge(0, 2e8)] // FIXME optimize out comparison in automaton
    } else {
      return this.raise(`Unrecognized expression type in token`, (expr as any).start)
    }
  }

  checkUnused() {
    for (let rule of this.ast.rules) if (!this.used[rule.id.name])
      // FIXME should probably be a warning
      this.raise(`Unused token rule '${rule.id.name}'`, rule.start)
  }
}

export function buildGrammar(text: string, fileName: string | null = null) {
  let builder = new Builder(text, fileName)
  return new Grammar(builder.rules, builder.terms)
}
