import {FIRST_REPEAT_TERM, FIRST_ANON_TERM, TERM_ERR, TERM_EOF} from "../parse/parser"

const TERMINAL = 1, REPEATED = 2, REPEATS = 4, PROGRAM = 8

export class Term {
  constructor(readonly id: number,
              readonly name: string,
              private flags: number,
              readonly tag: string | null) {}
  toString() { return this.name }
  get terminal() { return (this.flags & TERMINAL) > 0 }
  get eof() { return this.id == TERM_EOF }
  get error() { return this.id == TERM_ERR }
  get program() { return (this.flags & PROGRAM) > 0 }
  get interesting() { return this.flags > 0 || this.tag != null }
  set repeated(value: boolean) { this.flags = value ? this.flags | REPEATED : this.flags & ~REPEATED }
  cmp(other: Term) { return this.id - other.id }
}

export class TermSet {
  nonTerminals: Term[] = []
  terminals: Term[] = []
  tags: string[] = []
  repeatInfo: number[] = []
  eof: Term
  error: Term
  anonID = FIRST_ANON_TERM
  names: {[id: number]: string} = Object.create(null)

  constructor() {
    this.eof = this.term("␄", null, TERMINAL)
    this.error = this.term("⚠", "⚠", TERMINAL)
  }

  term(name: string, tag: string | null, flags: number = 0, repeats?: Term) {
    let id
    if (tag) {
      id = this.tags.length
      this.tags.push(tag)
    } else if (repeats) {
      flags |= REPEATS
      id = this.repeatInfo.length + FIRST_REPEAT_TERM
      this.repeatInfo.push(repeats.id)
    } else {
      id = this.anonID++
    }
    let term = new Term(id, name, flags, tag)
    this.names[id] = name
    ;(term.terminal ? this.terminals : this.nonTerminals).push(term)
    return term
  }

  makeTerminal(name: string, tag: string | null) {
    return this.term(name, tag, TERMINAL)
  }

  makeNonTerminal(name: string, tag: string | null, repeats?: Term) {
    // FIXME maybe don't hard-code the start symbol name—some grammars don't even parse "programs" (JSON, Markdown)
    return this.term(name, tag, (name == "program" ? PROGRAM : 0))
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
