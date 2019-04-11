import {Tokenizer} from "./token"
import {State as TableState} from "./automaton"

const TERMINAL = 1, EOF = 2, ERROR = 4, PROGRAM = 8

export const termTable: Term[] = []
// FIXME termIDs must wrap on 2^16 to fit in tree buffers. Store only names in the table, and start appending them when overflowing?
let termID = 0, taglessTermID = 1e9

export class Term {
  id: number
  public repeats: Term | null = null
  constructor(readonly name: string, private flags: number, readonly tag: string | null) {
    this.id = tag ? termID++ : taglessTermID--
    if (tag) termTable[this.id] = this
  }
  toString() { return this.name }
  get terminal() { return (this.flags & TERMINAL) > 0 }
  get eof() { return (this.flags & EOF) > 0 }
  get error() { return (this.flags & ERROR) > 0 }
  get program() { return (this.flags & PROGRAM) > 0 }
  cmp(other: Term) { return this == other ? 0 : (this.name < other.name ? -1 : 1) || this.flags - other.flags }
}

export class TermSet {
  nonTerminals: Term[] = []
  terminals: Term[] = []
  eof: Term
  error: Term

  constructor() {
    this.terminals.push(this.eof = new Term("␄", TERMINAL | EOF, null))
    this.terminals.push(this.error = new Term("⚠", TERMINAL | ERROR, "⚠"))
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
              readonly group: string,
              readonly precedence: number) {}

  cmp(other: Precedence) {
    return cmpStr(this.associativity || "", other.associativity || "") || cmpStr(this.group, other.group) ||
      this.precedence - other.precedence
  }

  eq(other: Precedence) {
    return this.associativity == other.associativity && this.group == other.group && this.precedence == other.precedence
  }

  static join(a: ReadonlyArray<Precedence>, b: ReadonlyArray<Precedence>): ReadonlyArray<Precedence> {
    if (a.length == 0) return b
    if (b.length == 0) return a
    return a.filter(p => !b.some(x => x.group == p.group)).concat(b)
  }

  static NON_FRAGILE = 1e9
}

function cmpStr(a: string, b: string) {
  return a == b ? 0 : a < b ? -1 : 1
}

const none: ReadonlyArray<any> = []

export class Rule {
  constructor(readonly name: Term,
              readonly parts: Term[],
              readonly precedence: ReadonlyArray<ReadonlyArray<Precedence>>) {}

  cmp(rule: Rule) {
    return this.name.cmp(rule.name) ||
      this.parts.length - rule.parts.length ||
      this.parts.reduce((r, s, i) => r || s.cmp(rule.parts[i]), 0) ||
      this.precedence.length - rule.precedence.length // FIXME flaky
  }

  precAt(pos: number): ReadonlyArray<Precedence> {
    return pos >= this.precedence.length ? none : this.precedence[pos] || none
  }

  rulePrec(): ReadonlyArray<Precedence> {
    let result = none as Precedence[]
    for (let value of this.precedence) {
      if (value) for (let prec of value) {
        if (result == none) result = []
        if (!result.some(p => prec.eq(p))) result.push(prec)
      }
    }
    return result
  }

  toString() {
    return this.name + " -> " + this.parts.join(" ")
  }
}

export class Grammar {
  constructor(readonly rules: Rule[],
              readonly terms: TermSet,
              readonly table: ReadonlyArray<TableState>,
              readonly tokenTable: ReadonlyArray<ReadonlyArray<Tokenizer>>) {}

  toString() { return this.rules.join("\n") }
}
