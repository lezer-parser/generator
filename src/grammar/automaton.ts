import {Term, Precedence, Rule, Grammar} from "./grammar"

export class Pos {
  constructor(readonly rule: Rule, readonly pos: number, public ahead: Term) {}

  get next() {
    return this.pos < this.rule.parts.length ? this.rule.parts[this.pos] : null
  }

  advance() {
    return new Pos(this.rule, this.pos + 1, this.ahead)
  }

  cmp(pos: Pos) {
    return this.cmpSimple(pos) || this.ahead.cmp(pos.ahead)
  }

  cmpSimple(pos: Pos) {
    return this.rule.cmp(pos.rule) || this.pos - pos.pos
  }

  toString() {
    let parts = this.rule.parts.map(t => t.name)
    parts.splice(this.pos, 0, "·")
    return `${this.rule.name} -> ${parts.join(" ")} [${this.ahead}]`
  }

  termsAhead(grammar: Grammar): Term[] {
    let found: Term[] = []
    for (let pos = this.pos + 1; pos < this.rule.parts.length; pos++) {
      let next = this.rule.parts[pos], cont = false
      for (let term of next.terminal ? [next] : grammar.first[next.name]) {
        if (term == null) cont = true
        else if (!found.includes(term)) found.push(term)
      }
      if (!cont) return found
    }
    if (!found.includes(this.ahead)) found.push(this.ahead)
    return found
  }
}

function advance(set: Pos[], expr: Term): Pos[] {
  let result = []
  for (let pos of set) if (pos.next == expr) result.push(pos.advance())
  return result
}

function advanceWithPrec(set: Pos[], expr: Term): {set: Pos[], prec: Precedence | null} {
  let result = [], prec = null
  for (let pos of set) if (pos.next == expr) {
    if (pos.rule.precedence) {
      if (prec && !prec.eq(pos.rule.precedence))
        throw new Error(`Conflicting precedences for terminal ${JSON.stringify(expr.name)} at ${set.join()}`)
      prec = pos.rule.precedence
    }
    result.push(pos.advance())
  }
  return {set: result, prec}
}

function cmpSet<T extends {cmp(other: T): number}>(a: ReadonlyArray<T>, b: ReadonlyArray<T>,
                                                   cmp: (a: T, b: T) => number = (a, b) => a.cmp(b)) {
  if (a.length != b.length) return a.length - b.length
  for (let i = 0; i < a.length; i++) {
    let diff = cmp(a[i], b[i])
    if (diff) return diff
  }
  return 0
}

export interface Action {
  term: Term
  eq(other: Action): boolean
  map(mapping: number[], states: State[]): Action
}

export class Goto implements Action {
  constructor(readonly term: Term, readonly target: State) {}

  eq(other: Action): boolean { return other instanceof Goto && other.target == this.target }

  toString() { return "goto " + this.target.id }

  map(mapping: number[], states: State[]) { return new Goto(this.term, states[mapping[this.target.id]]) }
}

export const Shift = Goto

export class Accept implements Action {
  constructor(readonly term: Term) {}

  eq(other: Action) { return other instanceof Accept }

  toString() { return "accept" }

  map() { return this }
}

export class Reduce implements Action {
  constructor(readonly term: Term, readonly rule: Rule) {}

  eq(other: Action): boolean { return other instanceof Reduce && other.rule == this.rule }

  toString() { return "reduce " + this.rule }

  map() { return this }
}

export class State {
  terminals: Action[] = []
  terminalPrec: (Precedence | null)[] = []
  goto: Goto[] = []
  ambiguous = false // FIXME maybe store per terminal

  constructor(readonly id: number, readonly set: ReadonlyArray<Pos>) {}

  toString() {
    return this.id + "=" + this.set.join() +  ": " +
      this.terminals.map(t => t.term + "=" + t).join(",") + "|" + this.goto.map(g => g.term + "=" + g).join(",")
  }

  addAction(value: Action, prec: Precedence | null, pos?: Pos) {
    for (let i = 0; i < this.terminals.length; i++) {
      let action = this.terminals[i]
      if (action.term == value.term) {
        if (action.eq(value)) return
        this.ambiguous = true
        let prev = this.terminalPrec[i]
        if (prev && prec && prev.group == prec.group) {
          if (prec.precedence < 0) continue // This is marked as a conflict
          let override = prev.precedence - prec.precedence // Positive means we override, 0 means conflict
          if (override == 0 && action instanceof Shift && prec.associativity)
            override = prec.associativity == "left" ? 1 : -1
          if (override > 0) {
            this.terminals.splice(i, 1)
            this.terminalPrec.splice(i, 1)
            i--
            continue
          } else if (override < 0) {
            return
          }
        }
        console.log(action, "vs", value)
        console.log("state A is " + (action as any).target)
        console.log("state B is " + (value as any).target)
        throw new Error((action instanceof Shift ? "shift" : "reduce") + "/reduce conflict at " + pos + " for " + action.term)
      }
    }
    this.terminals.push(value)
    this.terminalPrec.push(prec)
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
    let ahead = pos.termsAhead(grammar)
    for (let rule of grammar.rules) if (rule.name == next) {
      for (let a of ahead) {
        if (!result.some(p => p.rule == rule && p.pos == 0 && p.ahead == a))
          result.push(new Pos(rule, 0, a))
      }
    }
  }
  return result.sort((a, b) => a.cmp(b))
}

// Builds a full LR(1) automaton
export function buildFullAutomaton(grammar: Grammar) {
  let states: State[] = []
  function explore(set: Pos[]) {
    if (set.length == 0) return null
    set = closure(set, grammar)
    let state = states.find(s => cmpSet(s.set, set) == 0)
    if (!state) {
      states.push(state = new State(states.length, set))
      for (let term of grammar.terms.terminals) {
        if (term == grammar.terms.eof) continue
        let {set: newSet, prec} = advanceWithPrec(set, term), shift = explore(newSet)
        if (shift) state.addAction(new Shift(term, shift), prec)
      }
      for (let nt of grammar.terms.nonTerminals) {
        let goto = explore(advance(set, nt))
        if (goto) state.goto.push(new Goto(nt, goto))
      }
      for (let pos of set) {
        let next = pos.next
        if (next != null) continue
        if (pos.ahead == grammar.terms.eof && pos.rule.name.name == "Program")
          state.addAction(new Accept(grammar.terms.eof), pos.rule.precedence, pos)
        else
          state.addAction(new Reduce(pos.ahead, pos.rule), pos.rule.precedence, pos)
      }
    }
    return state
  }

  explore(grammar.rules.filter(rule => rule.name.name == "Program").map(rule => new Pos(rule, 0, grammar.terms.eof)))
  return states
}

// Collapse an LR(1) automaton to an LALR automaton
function collapseAutomaton(states: State[]): State[] {
  let newStates: State[] = [], mapping: number[] = []
  for (let i = 0; i < states.length; i++) {
    let state = states[i], set: Pos[] = []
    for (let pos of state.set) if (!set.some(p => p.cmpSimple(pos) == 0)) set.push(pos)
    let newID = newStates.findIndex(s => cmpSet(s.set, set, (a, b) => a.cmpSimple(b)) == 0)
    if (newID < 0) {
      newID = newStates.length
      newStates.push(new State(newID, set))
    }
    mapping.push(newID)
  }
  for (let i = 0; i < states.length; i++) {
    let state = states[i], newID = mapping[i], target = newStates[newID]
    for (let j = 0; j < state.terminals.length; j++)
      target.addAction(state.terminals[j].map(mapping, newStates), state.terminalPrec[j])
    for (let goto of state.goto) {
      if (!target.goto.find(a => a.term == goto.term))
        target.goto.push(goto.map(mapping, newStates))
    }
  }
  return newStates
}

export function buildAutomaton(grammar: Grammar) {
  return collapseAutomaton(buildFullAutomaton(grammar))
}
