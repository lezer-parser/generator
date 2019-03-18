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
    return this.rule.name + " -> " + parts.join(" ")
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
        this.ambiguous = true
        // We know value instanceof Reduce, because Shift actions are
        // added first, and won't ever conflict with each other
        let reduce = value as Reduce, source = reduce.rule.source
        let match = this.terminalSets[i].find(pos => pos.rule.source == source)
        // Shift-reduce conflict that may be solved with associativity
        if (action instanceof Shift && match && source.assoc) {
          if (source.assoc == "left") { // Reduce wins in left-associative rules
            this.terminals[i] = reduce
            this.terminalSets[i] = set
          } // Else the existing shift wins, `value` is discarded
          return
        } else if (match && source.expr.type == "ChoiceExpression" && source.expr.kind) {
          if (source.expr.kind == "ambig") { // Marked ambiguous, allow conflicting actions
            continue
          } else if (match.rule.position != reduce.rule.position) { // Precedence choice with differing precedences
            if (match.rule.position > reduce.rule.position) { // New action has higher precedence
              this.terminals[i] = reduce
              this.terminalSets[i] = set
            } // Otherwise, old action has higher precedence
            return
          }
        }
        throw new Error((action instanceof Shift ? "shift" : "reduce") + "/reduce conflict at " + set[0] + " for " + action.term)
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
        if (shift) state.addAction(new Shift(term, shift), set)
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
