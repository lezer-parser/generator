export class Term {
  readonly terminal: boolean
  constructor(readonly name: string) {
    this.terminal = !/^[A-Z]/.test(name)
  }
  toString() { return this.name }
  cmp(other: Term) { return this == other ? 0 : this.name < other.name ? -1 : 1 }
}

class TermSet {
  terms: Term[]
  constructor() { this.terms = [] }
  get(name: string) {
    let found = this.terms.find(t => t.name == name)
    if (!found) this.terms.push(found = new Term(name))
    return found
  }
}

export class Rule {
  name: Term
  parts: ReadonlyArray<Term>
  constructor(str: string, terms: TermSet) {
    let [, name, expr] = /(\w+)\s*->\s*(.*)/.exec(str)!
    this.name = terms.get(name)
    this.parts = expr.split(/\s+/).filter(x => x).map(n => terms.get(n))
  }

  cmp(rule: Rule) {
    return this.name.cmp(rule.name) ||
      this.parts.length - rule.parts.length ||
      this.parts.reduce((r, s, i) => r || s.cmp(rule.parts[i]), 0)
  }

  toString() {
    return this.name + "->" + this.parts.join("")
  }
}

export class Grammar {
  terms = new TermSet
  rules: Rule[]
  nonTerminals: Term[]
  terminals: Term[]
  first: {[name: string]: Term[]}
  follows: {[name: string]: Term[]}

  constructor(rules: string[]) {
    this.rules = rules.map(rule => new Rule(rule, this.terms))
    this.nonTerminals = this.terms.terms.filter(t => !t.terminal)
    this.terminals = this.terms.terms.filter(t => t.terminal)
    this.first = computeFirst(this.rules, this.nonTerminals)
    this.follows = computeFollows(this.rules, this.nonTerminals, this.first)
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

