import {Term, TermSet, Rule, cmpSet, Conflicts, union} from "./grammar"
import {hash, hashString} from "./hash"

class Pos {
  hash: number

  constructor(readonly rule: Rule,
              readonly pos: number,
              readonly ahead: readonly Term[],
              readonly ambigAhead: readonly string[],
              readonly skipAhead: Term,
              readonly prev: Pos | null,
              readonly depth: number) {
    let h = hash(hash(rule.id, pos), skipAhead.hash)
    for (let a of this.ahead) h = hash(h, a.hash)
    for (let group of ambigAhead) h = hashString(h, group)
    this.hash = h
  }

  get next() {
    return this.pos < this.rule.parts.length ? this.rule.parts[this.pos] : null
  }

  advance() {
    return new Pos(this.rule, this.pos + 1, this.ahead, this.ambigAhead, this.skipAhead, this, this.depth)
  }

  reverse() {
    return new Pos(this.rule, this.pos - 1, this.ahead, this.ambigAhead, this.skipAhead, this.prev!.prev, this.depth)
  }

  get skip() {
    return this.pos == this.rule.parts.length ? this.skipAhead : this.rule.skip
  }

  cmp(pos: Pos) {
    return this.rule.cmp(pos.rule) || this.pos - pos.pos || this.skipAhead.hash - pos.skipAhead.hash ||
      cmpSet(this.ahead, pos.ahead, (a, b) => a.cmp(b)) || cmpSet(this.ambigAhead, pos.ambigAhead, cmpStr)
  }

  eqSimple(pos: Pos) {
    return pos.rule == this.rule && pos.pos == this.pos
  }

  toString() {
    let parts = this.rule.parts.map(t => t.name)
    parts.splice(this.pos, 0, "·")
    return `${this.rule.name} -> ${parts.join(" ")}`
  }

  eq(other: Pos) {
    return this == other ||
      this.hash == other.hash && this.rule == other.rule && this.pos == other.pos && this.skipAhead == other.skipAhead &&
      sameSet(this.ahead, other.ahead) &&
      sameSet(this.ambigAhead, other.ambigAhead)
  }

  trail() {
    let result = []
    for (let cur = this.prev; cur; cur = cur.prev) result.push(cur.next)
    return result.reverse().join(" ")
  }

  conflicts(pos = this.pos) {
    let result = this.rule.conflicts[pos]
    if (pos == this.rule.parts.length && this.ambigAhead.length) result = result.join(new Conflicts(0, this.ambigAhead))
    return result
  }

  static conflictsAt(group: readonly Pos[], context: readonly Pos[]) {
    let result = Conflicts.none
    let scan: Term[] = []
    for (let pos of group) {
      result = result.join(pos.conflicts())
      if (pos.pos == 0) addTo(pos.rule.name, scan)
    }
    for (let i = 0; i < scan.length; i++) {
      let name = scan[i]
      for (let pos of context) if (pos.next == name) {
        result = result.join(pos.conflicts())
        if (pos.pos == 0) addTo(pos.rule.name, scan)
      }
    }
    return result
  }
}

function cmpStr(a: string, b: string) {
  return a < b ? -1 : a > b ? 1 : 0
}

function termsAhead(rule: Rule, pos: number, after: readonly Term[], first: {[name: string]: Term[]}): Term[] {
  let found: Term[] = []
  for (let i = pos + 1; i < rule.parts.length; i++) {
    let next = rule.parts[i], cont = false
    if (next.terminal) {
      addTo(next, found)
    } else for (let term of first[next.name]) {
      if (term == null) cont = true
      else addTo(term, found)
    }
    if (!cont) return found
  }
  for (let a of after) addTo(a, found)
  return found
}

function eqSet<T extends {eq(other: T): boolean}>(a: readonly T[], b: readonly T[]): boolean {
  if (a.length != b.length) return false
  for (let i = 0; i < a.length; i++) if (!a[i].eq(b[i])) return false
  return true
}

function sameSet<T>(a: readonly T[], b: readonly T[]) {
  if (a.length != b.length) return false
  for (let i = 0; i < a.length; i++) if (a[i] != b[i]) return false
  return true
}

export class Shift {
  constructor(readonly term: Term, readonly target: State) {}

  eq(other: Shift | Reduce): boolean { return other instanceof Shift && this.term == other.term && other.target.id == this.target.id }

  cmp(other: Shift | Reduce): number { return other instanceof Reduce ? -1 : this.term.id - other.term.id || this.target.id - other.target.id }

  toString() { return "s" + this.target.id }

  map(mapping: number[], states: State[]) {
    let mapped = mapping[this.target.id]
    return mapped == this.target.id ? this : new Shift(this.term, states[mapped])
  }
}

export class Reduce {
  constructor(readonly term: Term, readonly rule: Rule) {}

  eq(other: Shift | Reduce): boolean {
    return other instanceof Reduce && this.term == other.term && other.rule.sameReduce(this.rule)
  }

  cmp(other: Shift | Reduce): number {
    return other instanceof Shift ? 1 : this.term.id - other.term.id || this.rule.name.id - other.rule.name.id ||
      this.rule.parts.length - other.rule.parts.length
  }

  toString() { return `${this.rule.name.name}(${this.rule.parts.length})` }

  map() { return this }
}

function hashPositions(set: readonly Pos[]) {
  let h = 5381
  for (let pos of set) h = hash(h, pos.hash)
  return h
}

export class State {
  actions: (Shift | Reduce)[] = []
  actionPositions: (readonly Pos[])[] = []
  goto: Shift[] = []
  recover: Shift[] = []
  tokenGroup: number = -1
  defaultReduce: Rule | null = null
  partOfSkip: Term | null = null
  nested = -1

  constructor(public id: number,
              public set: readonly Pos[],
              public flags = 0,
              readonly skip: Term,
              public hash = hashPositions(set)) {}

  toString() {
    let actions = this.actions.map(t => t.term + "=" + t).join(",") +
      (this.goto.length ? " | " + this.goto.map(g => g.term + "=" + g).join(",") : "")
    return this.id + ": " + this.set.filter(p => p.pos > 0).join() +
      (this.defaultReduce ? `\n  always ${this.defaultReduce.name}(${this.defaultReduce.parts.length})`
       : actions.length ? "\n  " + actions : "")
  }

  addActionInner(value: Shift | Reduce, positions: readonly Pos[]): Shift | Reduce | null {
    check: for (let i = 0; i < this.actions.length; i++) {
      let action = this.actions[i]
      if (action.term == value.term) {
        if (action.eq(value)) return null
        let conflicts = Pos.conflictsAt(positions, this.set)
        let actionConflicts = Pos.conflictsAt(this.actionPositions[i], this.set)
        let diff = conflicts.precedence - actionConflicts.precedence
        if (diff > 0) { // Drop the existing action
          this.actions.splice(i, 1)
          this.actionPositions.splice(i, 1)
          i--
          continue check
        } else if (diff < 0) { // Drop this one
          return null
        } else if (conflicts.ambigGroups.some(g => actionConflicts.ambigGroups.includes(g))) { // Explicitly allowed ambiguity
          continue check
        } else { // Not resolved
          return action
        }
      }
    }
    this.actions.push(value)
    this.actionPositions.push(positions)
    return null
  }

  addAction(value: Shift | Reduce, positions: readonly Pos[]) {
    let conflict = this.addActionInner(value, positions)
    if (conflict) {
      let conflictPos = this.actionPositions[this.actions.indexOf(conflict)][0]
      let error
      if (conflict instanceof Shift)
        error = `shift/reduce conflict between\n  ${conflictPos}\nand\n  ${positions[0].rule}`
      else
        error = `reduce/reduce conflict between\n  ${positions[0].rule}\nand\n  ${conflictPos.rule}`
      let trail = positions[0].trail()
      if (trail.length > 50) trail = trail.slice(trail.length - 50).replace(/.*? /, "… ")
      error += `\nWith input:\n  ${trail} · ${value.term} …`
      throw new Error(error)
    }
  }

  getGoto(term: Term) {
    return this.goto.find(a => a.term == term)
  }

  hasSet(set: readonly Pos[]) {
    return eqSet(this.set, set)
  }

  finish() {
    if (this.actions.length) {
      let first = this.actions[0]
      if (first instanceof Reduce) {
        let {rule} = first
        if (this.actions.every(a => a instanceof Reduce && a.rule.sameReduce(rule)))
          this.defaultReduce = rule
      }
    }
    this.actions.sort((a, b) => a.cmp(b))
    this.goto.sort((a, b) => a.cmp(b))
    this.recover.sort((a, b) => a.cmp(b))
  }

  eq(other: State) {
    let dThis = this.defaultReduce, dOther = other.defaultReduce
    if (dThis || dOther)
      return dThis && dOther ? dThis.sameReduce(dOther) : false
    return this.skip == other.skip &&
      this.tokenGroup == other.tokenGroup &&
      eqSet(this.actions, other.actions) &&
      eqSet(this.goto, other.goto) &&
      eqSet(this.recover, other.recover)
  }
}

class AddedPos {
  constructor(readonly rule: Rule,
              readonly ahead: Term[],
              readonly origIndex: number,
              public ambigAhead: readonly string[],
              readonly skipAhead: Term,
              readonly prev: Pos | null,
              readonly depth: number) {}
}

function closure(set: readonly Pos[], first: {[name: string]: Term[]}) {
  let added: AddedPos[] = [], redo: AddedPos[] = []
  function addFor(name: Term, ahead: readonly Term[], ambigAhead: readonly string[], skipAhead: Term, prev: Pos | null, depth: number) {
    for (let rule of name.rules) {
      let add = added.find(a => a.rule == rule)
      if (!add) {
        let existing = set.findIndex(p => p.pos == 0 && p.rule == rule)
        add = new AddedPos(rule, existing < 0 ? [] : set[existing].ahead.slice(), existing, ambigAhead, skipAhead, prev, depth + 1)
        added.push(add)
      } else {
        if (add.skipAhead != skipAhead)
          throw new Error("Inconsistent skip sets after " + add.prev!.trail())
        add.ambigAhead = union(add.ambigAhead, ambigAhead)
      }
      for (let term of ahead) if (!add.ahead.includes(term)) {
        add.ahead.push(term)
        if (add.rule.parts.length && !add.rule.parts[0].terminal) addTo(add, redo)
      }
    }
  }
  
  for (let pos of set) {
    let next = pos.next
    if (next && !next.terminal)
      addFor(next, termsAhead(pos.rule, pos.pos, pos.ahead, first),
             pos.conflicts(pos.pos + 1).ambigGroups, pos.pos == pos.rule.parts.length - 1 ? pos.skipAhead : pos.rule.skip,
             pos.prev, pos.depth)
  }
  while (redo.length) {
    let add = redo.pop()!
    addFor(add.rule.parts[0], termsAhead(add.rule, 0, add.ahead, first),
           union(add.rule.conflicts[1].ambigGroups, add.rule.parts.length == 1 ? add.ambigAhead : none),
           add.skipAhead, add.prev, add.depth)
  }

  let result = set.slice()
  for (let add of added) {
    let pos = new Pos(add.rule, 0, add.ahead.sort((a, b) => a.hash - b.hash), add.ambigAhead, add.skipAhead, add.prev, add.depth)
    if (add.origIndex > -1) result[add.origIndex] = pos
    else result.push(pos)
  }
  return result.sort((a, b) => a.cmp(b))
}

function addTo<T>(value: T, array: T[]) {
  if (!array.includes(value)) array.push(value)
}

export function computeFirstSets(terms: TermSet) {
  let table: {[term: string]: Term[]} = {}
  for (let t of terms.terms) if (!t.terminal) table[t.name] = []
  for (;;) {
    let change = false
    for (let nt of terms.terms) if (!nt.terminal) for (let rule of nt.rules) {
      let set = table[nt.name]
      let found = false, startLen = set.length
      for (let part of rule.parts) {
        found = true
        if (part.terminal) {
          addTo(part, set)
        } else {
          for (let t of table[part.name]) {
            if (t == null) found = false
            else addTo(t, set)
          }
        }
        if (found) break
      }
      if (!found) addTo(null, set)
      if (set.length > startLen) change = true
    }
    if (!change) return table
  }
}

class Core {
  constructor(readonly set: readonly Pos[], readonly state: State) {}
}

// Builds a full LR(1) automaton
export function buildFullAutomaton(terms: TermSet, startTerm: Term, first: {[name: string]: Term[]}) {
  let states: State[] = []
  let cores: {[hash: number]: Core[]} = {}
  function getState(core: readonly Pos[]) {
    if (core.length == 0) return null
    let coreHash = hashPositions(core), byHash = cores[coreHash]
    let skip: Term | undefined
    for (let pos of core) {
      if (!skip) skip = pos.skip
      else if (skip != pos.skip) throw new Error("Inconsistent skip sets after " + pos.trail())
    }
    if (byHash) for (let known of byHash) if (eqSet(core, known.set)) {
      if (known.state.skip != skip) throw new Error("Inconsistent skip sets after " + known.set[0].trail())
      return known.state
    }

    let set = closure(core, first)
    let hash = hashPositions(set), found
    for (let state of states) if (state.hash == hash && state.hasSet(set)) found = state
    if (!found) {
      found = new State(states.length, set, 0, skip!, hash)
      states.push(found)
    }
    ;(cores[coreHash] || (cores[coreHash] = [])).push(new Core(core, found))
    return found
  }
  let startSkip = startTerm.rules.length ? startTerm.rules[0].skip : terms.names["%noskip"]!
  getState(startTerm.rules.map(rule => new Pos(rule, 0, [terms.eof], none, startSkip, null, 0)))

  for (let filled = 0; filled < states.length; filled++) {
    let state = states[filled]
    let byTerm: Term[] = [], byTermPos: Pos[][] = [], atEnd: Pos[] = []
    for (let pos of state.set) {
      if (pos.pos == pos.rule.parts.length) {
        if (!pos.rule.name.top) atEnd.push(pos)
      } else {
        let next = pos.rule.parts[pos.pos]
        let index = byTerm.indexOf(next)
        if (index < 0) {
          byTerm.push(next)
          byTermPos.push([pos.advance()])
        } else {
          byTermPos[index].push(pos.advance())
        }
      }
    }
    for (let i = 0; i < byTerm.length; i++) {
      let term = byTerm[i]
      if (term.terminal) {
        let set = applyCut(byTermPos[i])
        let next = getState(set)
        if (next) state.addAction(new Shift(term, next), set.map(p => p.reverse()))
      } else {
        let goto = getState(byTermPos[i])
        if (goto) state.goto.push(new Shift(term, goto))
      }
    }

    let replaced = false
    for (let pos of atEnd) for (let ahead of pos.ahead) {
      let count = state.actions.length
      state.addAction(new Reduce(ahead, pos.rule), [pos])
      if (state.actions.length == count) replaced = true
    }
    // If some actions were replaced by others, double-check whether
    // goto entries are now superfluous (for example, in an operator
    // precedence-related state that has a shift for `*` but only a
    // reduce for `+`, we don't need a goto entry for rules that start
    // with `+`)
    if (replaced) for (let i = 0; i < state.goto.length; i++) {
      let start = first[state.goto[i].term.name]
      if (!start.some(term => state.actions.some(a => a.term == term && (a instanceof Shift))))
        state.goto.splice(i--, 1)
    }
  }

  for (let state of states) state.finish()
  return states
}

function applyCut(set: readonly Pos[]): readonly Pos[] {
  let found: null | Pos[] = null, cut = 1
  for (let pos of set) {
    let value = pos.rule.conflicts[pos.pos - 1].cut
    if (value < cut) continue
    if (!found || value > cut) {
      cut = value
      found = []
    }
    found.push(pos)
  }
  return found || set
}

function mergeState(mapping: number[], newStates: State[], state: State, target: State): boolean {
  if (state.defaultReduce) return true
  for (let j = 0; j < state.actions.length; j++)
    if (target.addActionInner(state.actions[j].map(mapping, newStates), state.actionPositions[j]))
      return false
  for (let goto of state.goto) {
    if (!target.goto.find(a => a.term == goto.term))
      target.goto.push(goto.map(mapping, newStates))
  }
  return true
}

function markConflicts(mapping: number[], newID: number, oldStates: readonly State[], newStates: State[], conflicts: number[]) {
  // For all combinations of merged states
  for (let i = 0; i < mapping.length; i++) if (mapping[i] == newID) {
    for (let j = 0; j < mapping.length; j++) if (j != i && mapping[j] == newID) {
      // Create a dummy state to determine whether there's a conflict
      let state = new State(0, oldStates[i].set, 0, oldStates[i].skip, 0)
      mergeState(mapping, newStates, oldStates[i], state)
      if (!mergeState(mapping, newStates, oldStates[j], state)) conflicts.push(i, j)
    }
  }
}

function hasConflict(id: number, newID: number, mapping: number[], conflicts: number[]) {
  for (let i = 0; i < conflicts.length; i++) if (conflicts[i] == id) {
    let other = conflicts[i + (i % 2 ? -1 : 1)] // Pick other side of the pair
    if (mapping[other] == newID) return true
  }
  return false
}

// Collapse an LR(1) automaton to an LALR-like automaton
function collapseAutomaton(states: readonly State[]): readonly State[] {
  let conflicts: number[] = []
  for (;;) {
    let newStates: State[] = [], mapping: number[] = []
    for (let i = 0; i < states.length; i++) {
      let state = states[i], {set} = state
      let newID = newStates.findIndex((s, index) => {
        return state.set.length == s.set.length &&
          state.set.every((p, i) => p.eqSimple(s.set[i])) &&
          s.tokenGroup == state.tokenGroup &&
          s.skip == state.skip &&
          !hasConflict(i, index, mapping, conflicts)
      })
      if (newID < 0) {
        newID = newStates.length
        let newState = new State(newID, set, state.flags, state.skip, state.hash)
        newState.tokenGroup = state.tokenGroup
        newState.defaultReduce = state.defaultReduce
        newStates.push(newState)
      } else {
        newStates[newID].flags |= state.flags
      }
      mapping.push(newID)
    }
    let conflicting: number[] = []
    for (let i = 0; i < states.length; i++) {
      let newID = mapping[i]
      if (conflicting.includes(newID)) continue // Don't do work for states that are known to conflict
      if (!mergeState(mapping, newStates, states[i], newStates[mapping[i]])) {
        conflicting.push(newID)
        markConflicts(mapping, newID, states, newStates, conflicts)
      }
    }
    if (!conflicting.length) return mergeIdentical(newStates)
  }
}

function mergeIdentical(states: readonly State[]): readonly State[] {
  // Resolve alwaysReduce and sort actions
  for (let state of states) state.finish()
  for (;;) {
    let mapping: number[] = [], didMerge = false
    let newStates: State[] = []
    // Find states that either have the same alwaysReduce or the same
    // actions, and merge them.
    for (let i = 0; i < states.length; i++) {
      let state = states[i]
      let match = newStates.findIndex(s => state.eq(s))
      if (match < 0) {
        mapping[i] = newStates.length
        newStates.push(state)
      } else {
        mapping[i] = match
        didMerge = true
        let other = newStates[match], add: Pos[] | null = null
        for (let pos of state.set) if (!other.set.some(p => p.eqSimple(pos))) (add || (add = [])).push(pos)
        if (add) other.set = add.concat(other.set).sort((a, b) => a.cmp(b))
      }
    }
    if (!didMerge) return states
    // Make sure actions point at merged state objects
    for (let state of newStates) if (!state.defaultReduce) {
      state.actions = state.actions.map(a => a.map(mapping, newStates))
      state.recover = state.recover.map(a => a.map(mapping, newStates))
      state.goto = state.goto.map(a => a.map(mapping, newStates))
    }
    // Renumber ids
    for (let i = 0; i < newStates.length; i++) newStates[i].id = i
    states = newStates
  }
}

const none: readonly any[] = []

function addRecoveryRules(table: readonly State[], first: {[name: string]: Term[]}) {
  for (let state of table) {
    for (let pos of state.set) if (pos.pos > 0) {
      for (let i = pos.pos + 1; i < pos.rule.parts.length; i++) {
        let part = pos.rule.parts[i]
        terms: for (let term of (part.terminal ? [part] : first[part.name])) if (term && !state.recover.some(a => a.term == term)) {
          let next = pos.rule.parts[pos.pos]
          let action = next.terminal ? state.actions.find(t => t.term == next) : state.getGoto(next)
          if (!action || !(action instanceof Shift)) continue
          state.recover.push(new Shift(term, action.target))
        }
      }
    }
  }
}

export function finishAutomaton(full: readonly State[], first: {[term: string]: Term[]}) {
  let table = collapseAutomaton(full)
  addRecoveryRules(table, first)
  return table
}
