import {State} from "./token"
import {buildAutomaton, State as TableState} from "./automaton"

const TERMINAL = 1, EOF = 2, PROGRAM = 4

export class Term {
  constructor(readonly name: string, readonly flags: number, readonly tag: string | null) {}
  toString() { return this.name }
  get terminal() { return (this.flags & TERMINAL) > 0 }
  get eof() { return (this.flags & EOF) > 0 }
  get program() { return (this.flags & PROGRAM) > 0 }
  cmp(other: Term) { return this == other ? 0 : (this.name < other.name ? -1 : 1) || this.terminal ? -1 : 1 }
}

export class TermSet {
  nonTerminals: Term[] = []
  terminals: Term[] = []
  eof: Term

  constructor() {
    this.eof = new Term("␄", TERMINAL | EOF, null)
    this.terminals.push(this.eof)
  }

  makeTerminal(name: string, tag: string | null) {
    let result = new Term(name, TERMINAL, tag)
    this.terminals.push(result)
    return result
  }

  makeNonTerminal(name: string, tag: string | null) {
    let result = new Term(name, name == "program" ? PROGRAM : 0, tag)
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
  readonly table: ReadonlyArray<TableState>

  constructor(readonly rules: Rule[],
              readonly terms: TermSet,
              readonly tokens: State,
              readonly skip: State | null) {
    this.first = computeFirst(this.rules, this.terms.nonTerminals)
    this.table = buildAutomaton(this)
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
