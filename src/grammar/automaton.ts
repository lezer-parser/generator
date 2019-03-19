import {Term, Rule, Grammar} from "./grammar"

function choicePrec(rule: Rule): null | "ambig" | "prec" {
  return rule.source.expr && rule.source.expr.type == "ChoiceExpression" ? rule.source.expr.kind : null
}

class Precedence {
  constructor(readonly rules: Rule[]) {}

  addRule(rule: Rule) {
    if ((!rule.source.assoc && !choicePrec(rule)) || this.rules.includes(rule)) return this
    let rules = this.rules.filter(r => r.source != rule.source)
    rules.push(rule)
    return new Precedence(rules.sort((a, b) => a.cmp(b)))
  }

  cmp(other: Precedence) {
    return cmpSet(this.rules, other.rules)
  }

  combine(other: Precedence) {
    if (this == other || other.rules.length == 0) return this
    if (this.rules.length == 0) return other
    let rules = this.rules.slice(), updated = false
    for (let rule of other.rules) {
      let found = rules.findIndex(r => r.source == rule.source)
      if (found < 0) { rules.push(rule); updated = true }
      else if (rules[found].position > rule.position) { rules[found] = rule; updated = true }
    }
    return updated ? new Precedence(rules.sort((a, b) => a.cmp(b))) : this
  }

  compare(other: Precedence, isShift: boolean): "override" | "overridden" | "both" | "conflict" {
    let result: "override" | "overridden" | "both" | "conflict" | null = null
    for (let rule of this.rules) {
      let match = other.rules.find(r => r.source == rule.source)
      if (!match) continue
      if (isShift && rule.source.assoc && match == rule) {
        let value: "override" | "overridden" = rule.source.assoc == "left" ? "override" : "overridden"
        console.log('set to ', value, "by associativity for " + rule)
        if (result && result != value) throw new Error("Conflicting precedences") // FIXME allow?
        result = value
      }
      let prec = choicePrec(rule)
      if (prec == "prec" && match.position != rule.position) {
        let value: "override" | "overridden" = match.position > rule.position ? "override" : "overridden"
        console.log('set to ', value, "by precedence for " + rule + " vs " + match)
        if (result && result != value) throw new Error("Conflicting precedences")
        result = value
      } else if (prec == "ambig") {
        return "both"
      }
    }
    return result || "conflict"
  }

  static null = new Precedence([])
}

export class Pos {
  constructor(readonly rule: Rule, readonly pos: number, readonly prec: Precedence) {}

  get next() {
    return this.pos < this.rule.parts.length ? this.rule.parts[this.pos] : null
  }

  advance() {
    return new Pos(this.rule, this.pos + 1, this.prec)
  }

  cmp(pos: Pos) {
    return this.rule.cmp(pos.rule) || this.pos - pos.pos || cmpSet(this.prec.rules, pos.prec.rules)
  }

  toString() {
    let parts = this.rule.parts.map(t => t.name)
    parts.splice(this.pos, 0, "·")
    return this.rule.name + " -> " + parts.join(" ")
  }
}

function advance(set: Pos[], expr: Term): {set: Pos[], prec: Precedence} {
  let result = [], prec = Precedence.null
  for (let pos of set) if (pos.next == expr) {
    result.push(pos.advance())
    prec = prec.combine(pos.prec)
  }
  return {set: result, prec}
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
  terminalPrec: Precedence[] = []
  goto: Goto[] = []
  ambiguous = false // FIXME maybe store per terminal

  constructor(readonly id: number, readonly set: ReadonlyArray<Pos>) {}

  toString() {
    return this.id + "=" + this.set.join() +  ": " +
      this.terminals.map(t => t.term + "=" + t).join(",") + "|" + this.goto.map(g => g.term + "=" + g).join(",")
  }

  addAction(value: Action, prec: Precedence, pos?: Pos) {
    for (let i = 0; i < this.terminals.length; i++) {
      let action = this.terminals[i]
      if (action.term == value.term) {
        if (action.eq(value)) {
          this.terminalPrec[i] = this.terminalPrec[i].combine(prec)
          return
        }
        this.ambiguous = true
        let resolve = prec.compare(this.terminalPrec[i], action instanceof Shift)
        if (resolve == "both") continue
        if (resolve == "conflict")
          throw new Error((action instanceof Shift ? "shift" : "reduce") + "/reduce conflict at " + pos + " for " + action.term)
        if (resolve == "override") {
          this.terminals[i] = value
          this.terminalPrec[i] = prec
        }
        return // Either overridden or overriding, we're done
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
    for (let rule of grammar.rules) if (rule.name == next) {
      let found = result.findIndex(p => p.pos == 0 && p.rule == rule)
      if (found > -1) {
        let prec = result[found].prec.addRule(rule)
        if (prec != result[found].prec) result[found] = new Pos(rule, 0, prec)
      } else {
        result.push(new Pos(rule, 0, pos.prec.addRule(rule)))
      }
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
        let {set: newSet, prec} = advance(set, term), shift = explore(newSet)
        if (shift) state.addAction(new Shift(term, shift), prec)
      }
      for (let nt of grammar.nonTerminals) {
        let goto = explore(advance(set, nt).set)
        if (goto) state.goto.push(new Goto(nt, goto))
      }
      for (let pos of set) {
        let next = pos.next
        if (next == null) {
          for (let follow of grammar.follows[pos.rule.name.name])
            state.addAction(new Reduce(follow, pos.rule), pos.prec, pos)
        } else if (next.name == "#") { // FIXME robust EOF representation
          state.addAction(new Accept(next), pos.prec, pos)
        }
      }
    }
    return state
  }

  explore(grammar.rules.filter(rule => rule.name.name == "S'").map(rule => new Pos(rule, 0, Precedence.null)))
  return states
}
