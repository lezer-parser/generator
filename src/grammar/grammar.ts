export class Term {
  constructor(readonly name: string, readonly terminal: boolean) {}
  toString() { return this.terminal ? JSON.stringify(this.name) : this.name }
  cmp(other: Term) { return this == other ? 0 : (this.name < other.name ? -1 : 1) || this.terminal ? -1 : 1 }
}

export class TermSet {
  nonTerminals: Term[] = []
  terminals: Term[] = []

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

export class Precedence {
  constructor(readonly associativity: "left" | "right" | null,
              readonly group: number,
              readonly precedence: number) {}

  cmp(other: Precedence): number {
    return this.precedence - other.precedence || this.group - other.group ||
      (this.associativity == other.associativity ? 0 : (this.associativity || "null") < (other.associativity || "null") ? -1 : 1)
  }
}

export class Rule {
  constructor(readonly name: Term,
              readonly parts: Term[],
              readonly precedence: Precedence | null = null) {}

  cmp(rule: Rule) {
    return this.name.cmp(rule.name) ||
      this.parts.length - rule.parts.length ||
      this.parts.reduce((r, s, i) => r || s.cmp(rule.parts[i]), 0) ||
      cmpPrec(this.precedence, rule.precedence)
  }

  toString() {
    return this.name + " -> " + this.parts.join(" ")
  }
}

function cmpPrec(a: Precedence | null, b: Precedence | null) {
  return a == b ? 0 : a && b ? a.cmp(b) : a ? 1 : -1
}

export class Grammar {
  first: {[name: string]: Term[]}
  follows: {[name: string]: Term[]}

  constructor(readonly rules: Rule[], readonly terms: TermSet) {
    this.first = computeFirst(this.rules, this.terms.nonTerminals)
    this.follows = computeFollows(this.rules, this.terms.nonTerminals, this.first)
  }

  toString() { return this.rules.join("\n") }
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
