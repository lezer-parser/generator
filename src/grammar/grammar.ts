import {State} from "./token"
import {State as TableState} from "./automaton"

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

export class TokenContext {
  constructor(readonly skip: State | null,
              readonly tokens: State) {}
}

export class Grammar {
  constructor(readonly rules: Rule[],
              readonly terms: TermSet,
              readonly table: ReadonlyArray<TableState>,
              readonly tokenTable: ReadonlyArray<ReadonlyArray<TokenContext>>) {}

  toString() { return this.rules.join("\n") }
}
