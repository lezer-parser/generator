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

export class Precedence {
  hash: number

  constructor(readonly isAmbig: boolean,
              readonly value: number,
              readonly associativity: "left" | "right" | null,
              readonly group: string | null) {
    this.hash = +isAmbig + (value << 1) + (associativity == "left" ? 387 : associativity ? 812 : 0) +
      (!group ? 0 : group.charCodeAt(0) << 6 + (group.length > 1 ? group.charCodeAt(1) << 8 : 0))
  }

  cmp(other: Precedence) {
    return +this.isAmbig - +other.isAmbig || this.value - other.value || cmpStr(this.associativity || "", other.associativity || "") ||
      cmpStr(this.group || "", other.group || "")
  }

  eq(other: Precedence) {
    return this.cmp(other) == 0
  }

  static join(a: ReadonlyArray<Precedence>, b: ReadonlyArray<Precedence>): ReadonlyArray<Precedence> {
    if (a.length == 0 || a == b) return b
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

let ruleID = 0

export class Rule {
  id = ruleID++

  constructor(readonly name: Term,
              readonly parts: Term[],
              readonly precedence: ReadonlyArray<ReadonlyArray<Precedence>>) {}

  cmp(rule: Rule) {
    return this.id - rule.id
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
