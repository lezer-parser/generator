import {Identifier, Expression} from "./node"
import {parseGrammar} from "./parse"
import {normalizeGrammar} from "./normalize"

function flatten(expr: Expression, type: string): Expression[] {
  let result: Expression[] = []
  ;(function explore(expr: Expression) {
    if (expr.type == type) (expr as any).exprs.forEach(explore)
    else result.push(expr)
  })(expr)
  return result
}

export class Term {
  constructor(readonly name: string, readonly terminal: boolean) {}
  toString() { return this.terminal ? JSON.stringify(this.name) : this.name }
  cmp(other: Term) { return this == other ? 0 : (this.name < other.name ? -1 : 1) || this.terminal ? -1 : 1 }
}

export class Rule {
  constructor(readonly name: Term, readonly assoc: null | "left" | "right", precedence: number, readonly parts: Term[]) {}

  static build(id: Identifier, expr: Expression, assoc: null | "left" | "right", prec: number, grammar: Grammar) {
    let name = grammar.getNonTerminal(id.name)
    let parts = flatten(expr, "SequenceExpression").map(expr => {
      if (expr.type == "NamedExpression") return grammar.getNonTerminal(expr.id.name)
      else if (expr.type == "LiteralExpression") return grammar.getTerminal(expr.value)
      else throw new Error("Unsupported expr type " + expr.type)
    })
    return new Rule(name, assoc, prec, parts)
  }

  cmp(rule: Rule) {
    return this.name.cmp(rule.name) ||
      this.parts.length - rule.parts.length ||
      this.parts.reduce((r, s, i) => r || s.cmp(rule.parts[i]), 0)
  }

  toString() {
    return this.name + " -> " + this.parts.join(" ")
  }
}

const enum Prec { None = -1, Ambiguous = -2 }

export class Grammar {
  rules: Rule[]
  nonTerminals: Term[] = []
  terminals: Term[] = []
  first: {[name: string]: Term[]}
  follows: {[name: string]: Term[]}

  constructor(grammar: string, fileName?: string) {
    let parsed = normalizeGrammar(parseGrammar(grammar, fileName))
    this.rules = [new Rule(this.getNonTerminal("S'"), null, -1, [this.getNonTerminal("S"), this.getTerminal("#")])] // FIXME
    for (let rule of Object.values(parsed.rules)) {
      let choices = flatten(rule.expr, "ChoiceExpression")
      let prec = rule.expr.type != "ChoiceExpression" || (rule as any).kind == null
        ? Prec.None : (rule as any).kind == "prec" ? choices.length : Prec.Ambiguous
      for (let expr of choices) {
        this.rules.push(Rule.build(rule.id, expr, rule.assoc, prec, this))
        if (prec > 0) prec--
      }
    }
    this.first = computeFirst(this.rules, this.nonTerminals)
    this.follows = computeFollows(this.rules, this.nonTerminals, this.first)
  }

  getTerminal(name: string) {
    for (let term of this.terminals) if (term.name == name) return term
    let result = new Term(name, true)
    this.terminals.push(result)
    return result
  }

  getNonTerminal(name: string) {
    for (let term of this.nonTerminals) if (term.name == name) return term
    let result = new Term(name, false)
    this.nonTerminals.push(result)
    return result
  }
}

function add<T>(value: T, array: T[]) {
  if (!array.includes(value)) array.push(value)
}

function computeFirst(rules: Rule[], nonTerminals: Term[]) {
  let table: {[term: string]: Term[]} = {}
  for (let t of nonTerminals) table[t.name] = []
  for (;;) {
    let change = false
    for (let rule of rules) {
      let set = table[rule.name.name]
      let found = false, startLen = set.length
      for (let part of rule.parts) {
        found = true
        if (part.terminal) {
          add(part, set)
        } else {
          for (let t of table[part.name]) {
            if (t == null) found = false
            else add(t, set)
          }
        }
        if (found) break
      }
      if (!found) add(null, set)
      if (set.length > startLen) change = true
    }
    if (!change) return table
  }
}

function computeFollows(rules: Rule[], nonTerminals: Term[], first: {[name: string]: Term[]}) {
  let table: {[name: string]: Term[]} = {}
  for (let t of nonTerminals) table[t.name] = []
  for (;;) {
    let change = false
    for (let rule of rules) {
      for (let i = 0; i < rule.parts.length; i++) {
        let part = rule.parts[i], toEnd = true
        if (part.terminal) continue
        let set = table[part.name], startLen = set.length
        for (let j = i + 1; j < rule.parts.length; j++) {
          let next = rule.parts[j]
          toEnd = false
          if (next.terminal) {
            add(next, set)
          } else {
            for (let f of first[next.name]) {
              if (f == null) toEnd = true
              else add(f, set)
            }
          }
          if (!toEnd) break
        }
        if (toEnd) for (let follow of table[rule.name.name]) add(follow, set)
        if (set.length > startLen) change = true
      }
    }
    if (!change) return table
  }
}
