export class Term {
  constructor(readonly name: string, readonly terminal: boolean, readonly tag: string | null) {}
  toString() { return this.terminal ? JSON.stringify(this.name) : this.name }
  cmp(other: Term) { return this == other ? 0 : (this.name < other.name ? -1 : 1) || this.terminal ? -1 : 1 }
}

export class TermSet {
  nonTerminals: Term[] = []
  terminals: Term[] = []
  eof: Term

  constructor() {
    this.eof = this.makeTerminal("␄", null)
  }

  makeTerminal(name: string, tag: string | null) {
    let result = new Term(name, true, tag)
    this.terminals.push(result)
    return result
  }

  makeNonTerminal(name: string, tag: string | null) {
    let result = new Term(name, false, tag)
    this.nonTerminals.push(result)
    return result
  }
}

export class Precedence {
  constructor(readonly associativity: "left" | "right" | null,
              readonly group: number,
              readonly precedence: number) {}

  eq(other: Precedence) {
    return this.associativity == other.associativity && this.group == other.group && this.precedence == other.precedence
  }
}

export class Rule {
  constructor(readonly name: Term,
              readonly parts: Term[],
              readonly precedence: Precedence | null = null) {}

  cmp(rule: Rule) {
    return this.name.cmp(rule.name) ||
      this.parts.length - rule.parts.length ||
      this.parts.reduce((r, s, i) => r || s.cmp(rule.parts[i]), 0)
  }

  toString() {
    return this.name + " -> " + this.parts.join(" ")
  }
}

export class Grammar {
  first: {[name: string]: Term[]}

  constructor(readonly rules: Rule[], readonly terms: TermSet) {
    this.first = computeFirst(this.rules, this.terms.nonTerminals)
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
