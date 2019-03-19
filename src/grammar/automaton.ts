import {Term, Rule, Grammar} from "./grammar"

export class Pos {
  constructor(readonly rule: Rule, readonly pos: number) {}

  get next() {
    return this.pos < this.rule.parts.length ? this.rule.parts[this.pos] : null
  }

  advance() {
    return new Pos(this.rule, this.pos + 1)
  }

  cmp(pos: Pos) {
    return this.rule.cmp(pos.rule) || this.pos - pos.pos
  }

  toString() {
    let parts = this.rule.parts.map(t => t.name)
    parts.splice(this.pos, 0, "·")
    return this.rule.name + " -> " + parts.join(" ")
  }
}

function advance(set: Pos[], expr: Term): Pos[] {
  let result = []
  for (let pos of set) if (pos.next == expr) result.push(pos.advance())
  return result
}

function cmpSet<T extends {cmp(other: T): number}>(a: ReadonlyArray<T>, b: ReadonlyArray<T>) {
  if (a.length != b.length) return a.length - b.length
  for (let i = 0; i < a.length; i++) {
    let diff = a[i].cmp(b[i])
    if (diff) return diff
  }
  return 0
}

export interface Action {
  term: Term
  eq(other: Action): boolean
}

export class Goto implements Action {
  constructor(readonly term: Term, readonly target: State) {}

  eq(other: Action): boolean { return other instanceof Goto && other.target == this.target }

  toString() { return "goto " + this.target.id }
}

export const Shift = Goto

export class Accept implements Action {
  constructor(readonly term: Term) {}

  eq(other: Action) { return other instanceof Accept }

  toString() { return "accept" }
}

export class Reduce implements Action {
  constructor(readonly term: Term, readonly rule: Rule) {}

  eq(other: Action): boolean { return other instanceof Reduce && other.rule == this.rule }

  toString() { return "reduce " + this.rule }
}

export class State {
  terminals: Action[] = []
  goto: Goto[] = []
  ambiguous = false // FIXME maybe store per terminal

  constructor(readonly id: number, readonly set: ReadonlyArray<Pos>) {}

  toString() {
    return this.id + "=" + this.set.join() +  ": " +
      this.terminals.map(t => t.term + "=" + t).join(",") + "|" + this.goto.map(g => g.term + "=" + g).join(",")
  }

  addAction(value: Action, pos?: Pos) {
    for (let i = 0; i < this.terminals.length; i++) {
      let action = this.terminals[i]
      if (action.term == value.term) {
        if (action.eq(value)) return
        this.ambiguous = true
        // FIXME resolve precedence
        throw new Error((action instanceof Shift ? "shift" : "reduce") + "/reduce conflict at " + pos + " for " + action.term)
      }
    }
    this.terminals.push(value)
  }

  forEachAction(term: Term, f: (action: Action) => void) {
    for (let a of this.terminals) if (a.term == term) f(a)
  }

  getGoto(term: Term) {
    return this.goto.find(a => a.term == term)
  }
}

function closure(set: ReadonlyArray<Pos>, grammar: Grammar) {
  let result = set.slice()
  for (let pos of result) {
    let next = pos.next
    if (!next || next.terminal) continue
    for (let rule of grammar.rules) if (rule.name == next) {
      if (!result.some(p => p.rule == rule && p.pos == 0)) result.push(new Pos(rule, 0))
    }
  }
  return result.sort((a, b) => a.cmp(b))
}

export function buildAutomaton(grammar: Grammar) {
  let states: State[] = []
  function explore(set: Pos[]) {
    if (set.length == 0) return null
    set = closure(set, grammar)
    let state = states.find(s => cmpSet(s.set, set) == 0)
    if (!state) {
      states.push(state = new State(states.length, set))
      for (let term of grammar.terminals) {
        if (term.name == "#") continue
        let newSet = advance(set, term), shift = explore(newSet)
        if (shift) state.addAction(new Shift(term, shift))
      }
      for (let nt of grammar.nonTerminals) {
        let goto = explore(advance(set, nt))
        if (goto) state.goto.push(new Goto(nt, goto))
      }
      for (let pos of set) {
        let next = pos.next
        if (next == null) {
          for (let follow of grammar.follows[pos.rule.name.name])
            state.addAction(new Reduce(follow, pos.rule), pos)
        } else if (next.name == "#") { // FIXME robust EOF representation
          state.addAction(new Accept(next), pos)
        }
      }
    }
    return state
  }

  explore(grammar.rules.filter(rule => rule.name.name == "S'").map(rule => new Pos(rule, 0)))
  return states
}
