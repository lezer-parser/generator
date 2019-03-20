import {GrammarDeclaration, RuleDeclaration, PrecDeclaration, Expression, NamedExpression} from "./node"
import {Term, TermSet, Precedence, Rule, Grammar} from "./grammar"
import {Input} from "./parse"

class Context {
  constructor(readonly b: Builder,
              readonly rule: RuleDeclaration,
              readonly precedence: Precedence | null) {}

  newName() {
    return this.b.newName(this.rule.id.name)
  }

  defineRule(name: Term, parts: Term[]) {
    this.b.rules.push(new Rule(name, parts, this.precedence))
  }

  resolve(expr: NamedExpression) {
    if (expr.namespace) {
      let ns = this.b.namespaces[expr.namespace.name]
      if (!ns)
        this.raise(`Reference to undefined namespace '${expr.namespace.name}'`, expr.start)
      return ns.resolve(expr, this)
    } else {
      let known = this.b.ast.rules.find(r => r.id.name == expr.id.name)
      if (!known)
        this.raise(`Reference to undefined rule '${expr.id.name}'`, expr.start)
      if (known!.params.length != expr.args.length)
        this.raise(`Wrong number or arguments for '${expr.id.name}'`, expr.start)
      // FIXME instantiate parameterized rules
      return [this.b.terms.getNonTerminal(expr.id.name)]
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

  raise(message: string, pos: number): never {
    return this.b.input.raise(message, pos)
  }

  withPrecedence(prec: Precedence) {
    return new Context(this.b, this.rule, prec)
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
    this.rules.push(new Rule(this.terms.getNonTerminal("^"), [this.terms.getNonTerminal("Program"),
                                                              this.terms.getTerminal("#")]))

    this.defineNamespace("Conflict", new ConflictNamespace)
    for (let prec of this.ast.precedences)
      this.defineNamespace(prec.id.name, new PrecNamespace(prec), prec.id.start)

    for (let rule of this.ast.rules) {
      if (this.ast.rules.find(r => r != rule && r.id.name == rule.id.name))
        this.input.raise(`Duplicate rule definition for '${rule.id.name}'`, rule.id.start)
      if (this.namespaces[rule.id.name])
        this.input.raise(`Rule name '${rule.id.name}' conflicts with a defined namespace`, rule.id.start)
      let name = this.terms.getNonTerminal(rule.id.name)
      let cx = new Context(this, rule, null)
      for (let choice of cx.normalizeTopExpr(rule.expr)) cx.defineRule(name, choice)
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
