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
    return this.rule.cmp(pos.rule) ||
      this.pos - pos.pos
  }

  toString() {
    let parts = this.rule.parts.map(t => t.name)
    parts.splice(this.pos, 0, "·")
    return this.rule.name + "->" + parts.join("")
  }
}

function advance(set: Pos[], expr: Term) {
  let result = []
  for (let pos of set) if (pos.next == expr)
    result.push(pos.advance())
  return result
}

function sameSet(a: ReadonlyArray<Pos>, b: ReadonlyArray<Pos>) {
  if (a.length != b.length) return false
  for (let i = 0; i < a.length; i++) if (a[i].cmp(b[i]) != 0) return false
  return true
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
  terminalSets: Pos[][] = []
  goto: Goto[] = []
  ambiguous = false

  constructor(readonly id: number, readonly set: ReadonlyArray<Pos>) {}

  toString() {
    return this.id + "=" + this.set.join() +  ": " +
      this.terminals.map(t => t.term + "=" + t).join(",") + "|" + this.goto.map(g => g.term + "=" + g).join(",")
  }

  addAction(value: Action, set: Pos[]) {
    for (let i = 0; i < this.terminals.length; i++) {
      let action = this.terminals[i]
      if (action.term == value.term) {
        if (action.eq(value)) return
        // Shift-reduce conflict that may be solved with associativity
        if (value instanceof Reduce && action instanceof Goto && value.rule.source.assoc &&
            this.terminalSets[i].some(p => p.rule == value.rule)) {
          if (value.rule.source.assoc == "left") { // Reduce wins in left-associative rules
            this.terminals[i] = value
            this.terminalSets[i] = set
          } // Else the existing shift wins, `value` is discarded
          return
        }
        // FIXME only allow duplicates when choices are explicitly marked
        // as ambiguous
        // throw new Error("Conflict at " + pos + ": " + action + " vs " + value)
        this.ambiguous = true
        console.log("duplicate rule added in " + this.id + " for", value.term + " " + value + " / " + action)
      }
    }
    this.terminals.push(value)
    this.terminalSets.push(set)
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
      if (!result.some(p => p.pos == 0 && p.rule == rule))
        result.push(new Pos(rule, 0))
    }
  }
  return result.sort((a, b) => a.cmp(b))
}

export function buildAutomaton(grammar: Grammar) {
  let states: State[] = []
  function explore(set: Pos[]) {
    if (set.length == 0) return null
    set = closure(set, grammar)
    let state = states.find(s => sameSet(s.set, set))
    if (!state) {
      states.push(state = new State(states.length, set))
      for (let term of grammar.terminals) {
        if (term.name == "#") continue
        let shift = explore(advance(set, term))
        if (shift) state.addAction(new Goto(term, shift), set)
      }
      for (let nt of grammar.nonTerminals) {
        let goto = explore(advance(set, nt))
        if (goto) state.goto.push(new Goto(nt, goto))
      }
      for (let pos of set) {
        let next = pos.next
        if (next == null) {
          for (let follow of grammar.follows[pos.rule.name.name])
            state.addAction(new Reduce(follow, pos.rule), [pos])
        } else if (next.name == "#") { // FIXME robust EOF representation
          state.addAction(new Accept(next), [pos])
        }
      }
    }
    return state
  }

  explore(grammar.rules.filter(rule => rule.name.name == "S'").map(rule => new Pos(rule, 0)))
  return states
}
