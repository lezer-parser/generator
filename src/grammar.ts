import {TERM_EOF} from "lezer"

const TERMINAL = 1, REPEATED = 2, PROGRAM = 4, ERROR = 8, EOF = 16

let termHash = 0

export class Term {
  hash = ++termHash // Used for sorting and hashing during parser generation
  id = -1 // Assigned in a later stage, used in actual output

  constructor(readonly name: string,
              private flags: number,
              readonly tag: string | null,
              readonly repeats: Term | null = null) {}
  toString() { return this.name }
  get terminal() { return (this.flags & TERMINAL) > 0 }
  get eof() { return (this.flags & EOF) > 0 }
  get error() { return (this.flags & ERROR) > 0 }
  get program() { return (this.flags & PROGRAM) > 0 }
  get interesting() { return this.flags > 0 || this.tag != null || this.repeats != null }
  set repeated(value: boolean) { this.flags = value ? this.flags | REPEATED : this.flags & ~REPEATED }
  cmp(other: Term) { return this.hash - other.hash }
}

export class TermSet {
  nonTerminals: Term[] = []
  terminals: Term[] = []
  eof: Term
  error: Term

  constructor() {
    this.eof = this.term("␄", null, TERMINAL | EOF)
    this.error = this.term("⚠", "⚠", ERROR)
  }

  term(name: string, tag: string | null, flags: number = 0, repeats?: Term) {
    let term = new Term(name, flags, tag, repeats)
    ;(term.terminal ? this.terminals : this.nonTerminals).push(term)
    return term
  }

  makeTerminal(name: string, tag: string | null) {
    return this.term(name, tag, TERMINAL)
  }

  makeNonTerminal(name: string, tag: string | null, repeats?: Term) {
    // FIXME maybe don't hard-code the start symbol name—some grammars don't even parse "programs" (JSON, Markdown)
    return this.term(name, tag, name == "program" ? PROGRAM : 0, repeats)
  }

  finish(rules: readonly Rule[]) {
    let tags: string[] = []
    let names: {[id: number]: string} = {}
    let repeatInfo: number[] = []

    let taggedID = -1, untaggedID = -2
    for (let term of this.nonTerminals) if (term.repeats && rules.some(r => r.name == term))
      term.id = (untaggedID += 2)
    for (let term of this.nonTerminals) if (term.id < 0 && (term.error || rules.some(r => r.name == term)))
      term.id = term.tag ? (taggedID += 2) : (untaggedID += 2)
    for (let term of this.terminals)
      term.id = term.eof ? TERM_EOF : term.tag ? (taggedID += 2) : (untaggedID += 2)

    for (let term of this.terminals.concat(this.nonTerminals)) if (term.id > -1) {
      if (term.tag) tags[term.id >> 1] = term.tag
      if (term.repeats) repeatInfo.push(term.repeats.id)
      names[term.id] = term.name
    }

    return {tags, names, repeatInfo}
  }
}

export function cmpSet<T>(a: readonly T[], b: readonly T[], cmp: (a: T, b: T) => number) {
  if (a.length != b.length) return a.length - b.length
  for (let i = 0; i < a.length; i++) {
    let diff = cmp(a[i], b[i])
    if (diff) return diff
  }
  return 0
}

export const PREC_REPEAT = 2e8

const none: readonly any[] = []

export class Conflicts {
  constructor(readonly precedence: number, readonly ambigGroups: readonly string[]) {}

  join(other: Conflicts) {
    if (this == Conflicts.none || this == other) return other
    if (other == Conflicts.none) return this
    return new Conflicts(Math.max(this.precedence, other.precedence), union(this.ambigGroups, other.ambigGroups))
  }

  cmp(other: Conflicts) {
    return this.precedence - other.precedence || cmpSet(this.ambigGroups, other.ambigGroups, (a, b) => a < b ? -1 : a > b ? 1 : 0)
  }

  static none = new Conflicts(0, none)
}

export function union(a: readonly string[], b: readonly string[]): readonly string[] {
  if (a.length == 0 || a == b) return b
  if (b.length == 0) return a
  let result = a.slice()
  for (let value of b) if (!a.includes(value)) result.push(value)
  return result.sort()
}

let ruleID = 0

export class Rule {
  id = ruleID++

  constructor(readonly name: Term,
              readonly parts: readonly Term[],
              readonly conflicts: readonly Conflicts[]) {}

  cmp(rule: Rule) {
    return this.id - rule.id
  }

  cmpNoName(rule: Rule) {
    return this.parts.length - rule.parts.length ||
      this.parts.reduce((r, s, i) => r || s.cmp(rule.parts[i]), 0) ||
      cmpSet(this.conflicts, rule.conflicts, (a, b) => a.cmp(b))
  }

  toString() {
    return this.name + " -> " + this.parts.join(" ")
  }
}
