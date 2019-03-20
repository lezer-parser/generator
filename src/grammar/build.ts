import {Expression, GrammarDeclaration, NamedExpression, PrecDeclaration, Identifier} from "./node"
import {Term, TermSet, Precedence, Rule, Grammar} from "./grammar"
import {Input} from "./parse"

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
    for (let prec of this.ast.precedences) {
      let name = prec.id.name
      if (this.namespaces[name]) this.input.raise(`Duplicate definition of namespace '${name}'`, prec.id.start)
      this.namespaces[name] = new PrecNamespace(prec)
    }
    for (let rule of this.ast.rules) {
      if (this.ast.rules.find(r => r != rule && r.id.name == rule.id.name))
        this.input.raise(`Duplicate rule definition for '${rule.id.name}'`, rule.id.start)
      let name = this.terms.getNonTerminal(rule.id.name)
      for (let choice of this.normalizeTopExpr(rule.expr, rule.id))
        this.rules.push(new Rule(name, choice))
    }
  }

  resolve(expr: NamedExpression, ruleName: Identifier): Term[] {
    if (expr.namespace) {
      let ns = this.namespaces[expr.namespace.name]
      if (!ns)
        this.input.raise(`Reference to undefined namespace '${expr.namespace.name}'`, expr.start)
      return ns.resolve(expr, this, ruleName)
    } else {
      let known = this.ast.rules.find(r => r.id.name == expr.id.name)
      if (!known)
        this.input.raise(`Reference to undefined rule '${expr.id.name}'`, expr.start)
      if (known!.params.length != expr.args.length)
        this.input.raise(`Wrong number or arguments for '${expr.id.name}'`, expr.start)
      // FIXME instantiate parameterized rules
      return [this.terms.getNonTerminal(expr.id.name)]
    }
  }

  normalizeTopExpr(expr: Expression, ruleName: Identifier): Term[][] {
    if (expr.type == "RepeatExpression" && expr.kind == "?") {
      return [[], ...this.normalizeTopExpr(expr.expr, ruleName)]
    } else if (expr.type == "RepeatExpression" && expr.kind == "*") {
      return [[], [this.terms.getNonTerminal(ruleName.name), ...this.normalizeExpr(expr.expr, ruleName)]]
    } else if (expr.type == "ChoiceExpression") {
      return expr.exprs.map(e => this.normalizeExpr(e, ruleName))
    } else {
      return [this.normalizeExpr(expr, ruleName)]
    }
  }

  normalizeExpr(expr: Expression, ruleName: Identifier): Term[] {
    if (expr.type == "RepeatExpression") {
      if (expr.kind == "?") {
        let name = this.newName(ruleName.name)
        this.rules.push(new Rule(name, this.normalizeExpr(expr.expr, ruleName)),
                        new Rule(name, []))
        return [name]
      } else {
        let name = this.newName(ruleName.name)
        let inner = this.normalizeExpr(expr.expr, ruleName)
        this.rules.push(new Rule(name, inner), new Rule(name, []))
        return expr.kind == "*" ? [name] : inner.concat(name)
      }
    } else if (expr.type == "ChoiceExpression") {
      let name = this.newName(ruleName.name)
      for (let choice of expr.exprs)
        this.rules.push(new Rule(name, this.normalizeExpr(choice, ruleName)))
      return [name]
    } else if (expr.type == "SequenceExpression") {
      return expr.exprs.reduce((a, e) => a.concat(this.normalizeExpr(e, ruleName)), [] as Term[])
    } else if (expr.type == "LiteralExpression") {
      return expr.value ? [this.terms.getTerminal(expr.value)] : []
    } else if (expr.type == "NamedExpression") {
      return this.resolve(expr, ruleName)
    } else {
      return this.input.raise("Unhandled expression type " + expr.type, expr.start)
    }
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
  resolve(expr: NamedExpression, b: Builder, ruleName: Identifier): Term[]
}

class PrecNamespace implements Namespace {
  id: number = precID++

  constructor(readonly ast: PrecDeclaration) {}

  resolve(expr: NamedExpression, b: Builder, ruleName: Identifier): Term[] {
    if (expr.args.length != 1)
      b.input.raise(`Precedence specifiers take a single argument`, expr.start)
    let found = this.ast.names.findIndex(n => n.name == expr.id.name)
    if (found < 0)
      b.input.raise(`Precedence group '${this.ast.id.name}' has no precedence named '${expr.id.name}'`, expr.id.start)
    let name = b.newName(`${expr.namespace!.name}-${expr.id.name}`, false)
    // FIXME make sure all sub-rules created by normalizing share the precedence
    b.rules.push(new Rule(name, b.normalizeExpr(expr.args[0], ruleName),
                          new Precedence(this.ast.assoc[found], this.id, found)))
    return [name]
  }
}

export function buildGrammar(text: string, fileName: string | null = null) {
  let builder = new Builder(text, fileName)
  return new Grammar(builder.rules, builder.terms)
}
