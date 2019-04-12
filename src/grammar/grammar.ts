import {Tokenizer} from "./token"
import {State as TableState} from "./automaton"

const TERMINAL = 1, EOF = 2, ERROR = 4, PROGRAM = 8, REPEATED = 16

export const termTable: Term[] = []
// FIXME termIDs must wrap on 2^16 to fit in tree buffers. Store only names in the table, and start appending them when overflowing?
let termID = 0, taglessTermID = 1e9

export class Term {
  id: number
  public repeats: Term | null = null
  constructor(readonly name: string, private flags: number, readonly tag: string | null, readonly grammarID: number) {
    this.id = tag ? termID++ : taglessTermID--
    if (tag) termTable[this.id] = this
  }
  toString() { return this.name }
  get terminal() { return (this.flags & TERMINAL) > 0 }
  get eof() { return (this.flags & EOF) > 0 }
  get error() { return (this.flags & ERROR) > 0 }
  get program() { return (this.flags & PROGRAM) > 0 }
  get interesting() { return this.flags > 0 || this.tag != null || this.repeats != null }
  set repeated(value: boolean) { this.flags = value ? this.flags | REPEATED : this.flags & ~REPEATED }
  cmp(other: Term) { return this == other ? 0 : (this.name < other.name ? -1 : 1) || this.flags - other.flags }
}

export class TermSet {
  nonTerminals: Term[] = []
  terminals: Term[] = []
  eof: Term
  error: Term

  constructor(public grammarID: number) {
    this.terminals.push(this.eof = new Term("␄", TERMINAL | EOF, null, grammarID))
    this.terminals.push(this.error = new Term("⚠", TERMINAL | ERROR, "⚠", grammarID))
  }

  makeTerminal(name: string, tag: string | null) {
    let result = new Term(name, TERMINAL, tag, this.grammarID)
    this.terminals.push(result)
    return result
  }

  makeNonTerminal(name: string, tag: string | null) {
    let result = new Term(name, name == "program" ? PROGRAM : 0, tag, this.grammarID)
    this.nonTerminals.push(result)
    return result
  }
}

export class Precedence {
  constructor(readonly isAmbig: boolean,
              readonly value: number,
              readonly associativity: "left" | "right" | null,
              readonly group: string | null) {}

  cmp(other: Precedence) {
    return +this.isAmbig - +other.isAmbig || this.value - other.value || cmpStr(this.associativity || "", other.associativity || "") ||
      cmpStr(this.group || "", other.group || "")
  }

  eq(other: Precedence) {
    return this.cmp(other) == 0
  }

  static join(a: ReadonlyArray<Precedence>, b: ReadonlyArray<Precedence>): ReadonlyArray<Precedence> {
    if (a.length == 0) return b
    if (b.length == 0) return a
    let result = a.slice()
    for (let p of b) {
      if (p.isAmbig) {
        if (!result.some(x => x.isAmbig && x.group == p.group)) result.push(p)
      } else {
        let found = result.findIndex(x => !x.isAmbig)
        if (found < 0) result.push(p)
        else if (result[found].value < p.value) result[found] = p
      }
    }
    return result
  }

  static REPEAT = 1e9
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
    return this.name.cmp(rule.name) || this.cmpNoName(rule)
  }

  cmpNoName(rule: Rule) {
    return this.parts.length - rule.parts.length ||
      this.parts.reduce((r, s, i) => r || s.cmp(rule.parts[i]), 0) ||
      this.precedence.length - rule.precedence.length ||
      this.precedence.reduce((d, ps, i) => d || ps.reduce((d, p, j) => d || p.cmp(rule.precedence[i][j]), 0), 0)
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
  constructor(readonly id: number,
              readonly rules: ReadonlyArray<Rule>,
              readonly terms: TermSet,
              readonly table: ReadonlyArray<TableState>,
              readonly tokenTable: ReadonlyArray<ReadonlyArray<Tokenizer>>) {}

  toString() { return this.rules.join("\n") }
}
