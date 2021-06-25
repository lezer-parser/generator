import {Term as T} from "@lezer/lr/dist/constants"
import {GenError} from "./error"

const enum TermFlag {
  // This term is a terminal
  Terminal = 1,
  // This is the top production
  Top = 2,
  // This represents end-of-file
  Eof = 4,
  // This should be preserved, even if it doesn't occur in any rule
  Preserve = 8,
  // Rules used for * and + constructs
  Repeated = 16,
  // Rules explicitly marked as [inline]
  Inline = 32
}

export type Props = {[name: string]: string}

export function hasProps(props: Props) {
  for (let _p in props) return true
  return false
}

let termHash = 0

export class Term {
  hash = ++termHash // Used for sorting and hashing during parser generation
  id = -1 // Assigned in a later stage, used in actual output
  // Filled in only after the rules are simplified, used in automaton.ts
  rules: Rule[] = []

  constructor(readonly name: string,
              private flags: number,
              readonly nodeName: string | null,
              readonly props: Props = {}) {}

  toString() { return this.name }
  get nodeType() { return this.top || this.nodeName != null || hasProps(this.props) || this.repeated }
  get terminal() { return (this.flags & TermFlag.Terminal) > 0 }
  get eof() { return (this.flags & TermFlag.Eof) > 0 }
  get error() { return "error" in this.props }
  get top() { return (this.flags & TermFlag.Top) > 0 }
  get interesting() { return this.flags > 0 || this.nodeName != null }
  get repeated() { return (this.flags & TermFlag.Repeated) > 0 }
  set preserve(value: boolean) { this.flags = value ? this.flags | TermFlag.Preserve : this.flags & ~TermFlag.Preserve }
  get preserve() { return (this.flags & TermFlag.Preserve) > 0 }
  set inline(value: boolean) { this.flags = value ? this.flags | TermFlag.Inline : this.flags & ~TermFlag.Inline }
  get inline() { return (this.flags & TermFlag.Inline) > 0 }
  cmp(other: Term) { return this.hash - other.hash }
}

export class TermSet {
  terms: Term[] = []
  // Map from term names to Term instances
  names: {[name: string]: Term} = Object.create(null)
  eof: Term
  error: Term
  tops: Term[] = []

  constructor() {
    this.eof = this.term("␄", null, TermFlag.Terminal | TermFlag.Eof)
    this.error = this.term("⚠", "⚠", TermFlag.Preserve)
  }

  term(name: string, nodeName: string | null, flags: number = 0, props: Props = {}) {
    let term = new Term(name, flags, nodeName, props)
    this.terms.push(term)
    this.names[name] = term
    return term
  }

  makeTop(nodeName: string | null, props: Props) : Term {
    const term = this.term("@top", nodeName, TermFlag.Top, props)
    this.tops.push(term)
    return term
  }

  makeTerminal(name: string, nodeName: string | null, props = {}) {
    return this.term(name, nodeName, TermFlag.Terminal, props)
  }

  makeNonTerminal(name: string, nodeName: string | null, props = {}) {
    return this.term(name, nodeName, 0, props)
  }

  makeRepeat(name: string) {
    return this.term(name, null, TermFlag.Repeated)
  }

  uniqueName(name: string) {
    for (let i = 0;; i++) {
      let cur = i ? `${name}-${i}` : name
      if (!this.names[cur]) return cur
    }
  }

  finish(rules: readonly Rule[]) {
    for (let rule of rules) rule.name.rules.push(rule)

    this.terms = this.terms.filter(t => t.terminal || t.preserve || rules.some(r => r.name == t || r.parts.includes(t)))

    let names: {[id: number]: string} = {}
    let nodeTypes = [this.error]

    this.error.id = T.Err
    let nextID = T.Err + 1

    // Assign ids to terms that represent node types
    for (let term of this.terms) if (term.id < 0 && term.nodeType && !term.repeated) {
      term.id = nextID++
      nodeTypes.push(term)
    }
    // Put all repeated terms after the regular node types
    let minRepeatTerm = nextID
    for (let term of this.terms) if (term.repeated) {
      term.id = nextID++
      nodeTypes.push(term)
    }
    // Then comes the EOF term
    this.eof.id = nextID++
    // And then the remaining (non-node, non-repeat) terms.
    for (let term of this.terms) {
      if (term.id < 0) term.id = nextID++
      if (term.name) names[term.id] = term.name
    }
    if (nextID >= 0xfffe) throw new GenError("Too many terms")

    return {nodeTypes, names, minRepeatTerm, maxTerm: nextID - 1}
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

const none: readonly any[] = []

export class Conflicts {
  constructor(readonly precedence: number, readonly ambigGroups: readonly string[] = none, readonly cut = 0) {}

  join(other: Conflicts) {
    if (this == Conflicts.none || this == other) return other
    if (other == Conflicts.none) return this
    return new Conflicts(Math.max(this.precedence, other.precedence), union(this.ambigGroups, other.ambigGroups),
                         Math.max(this.cut, other.cut))
  }

  cmp(other: Conflicts) {
    return this.precedence - other.precedence || cmpSet(this.ambigGroups, other.ambigGroups, (a, b) => a < b ? -1 : a > b ? 1 : 0) ||
      this.cut - other.cut
  }

  static none = new Conflicts(0)
}

export function union<T>(a: readonly T[], b: readonly T[]): readonly T[] {
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
              readonly conflicts: readonly Conflicts[],
              readonly skip: Term) {}

  cmp(rule: Rule) {
    return this.id - rule.id
  }

  cmpNoName(rule: Rule) {
    return this.parts.length - rule.parts.length ||
      this.skip.hash - rule.skip.hash ||
      this.parts.reduce((r, s, i) => r || s.cmp(rule.parts[i]), 0) ||
      cmpSet(this.conflicts, rule.conflicts, (a, b) => a.cmp(b))
  }

  toString() {
    return this.name + " -> " + this.parts.join(" ")
  }

  get isRepeatWrap() {
    return this.name.repeated && this.parts.length == 2 && this.parts[0] == this.name
  }

  sameReduce(other: Rule) {
    return this.name == other.name && this.parts.length == other.parts.length && this.isRepeatWrap == other.isRepeatWrap
  }
}
