import {Term, TermSet, Rule, cmpSet, Conflicts, union} from "./grammar"
import {hash, hashString} from "./hash"
import {GenError} from "./error"
import {timing} from "./log"

export class Pos {
  hash: number = 0

  constructor(readonly rule: Rule,
              readonly pos: number,
              // NOTE `ahead` and `ambigAhead` aren't mutated anymore after `finish()` has been called
              readonly ahead: Term[],
              public ambigAhead: readonly string[],
              readonly skipAhead: Term,
              readonly via: Pos | null) {}

  finish() {
    let h = hash(hash(this.rule.id, this.pos), this.skipAhead.hash)
    for (let a of this.ahead) h = hash(h, a.hash)
    for (let group of this.ambigAhead) h = hashString(h, group)
    this.hash = h
    return this
  }

  get next() {
    return this.pos < this.rule.parts.length ? this.rule.parts[this.pos] : null
  }

  advance() {
    return new Pos(this.rule, this.pos + 1, this.ahead, this.ambigAhead, this.skipAhead, this.via).finish()
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

  trail(maxLen: number = 60) {
    let result = []
    for (let pos: Pos | null = this; pos; pos = pos.via) {
      for (let i = pos.pos - 1; i >= 0; i--) result.push(pos.rule.parts[i])
    }
    let value = result.reverse().join(" ")
    if (value.length > maxLen) value = value.slice(value.length - maxLen).replace(/.*? /, "… ")
    return value
  }

  conflicts(pos = this.pos) {
    let result = this.rule.conflicts[pos]
    if (pos == this.rule.parts.length && this.ambigAhead.length) result = result.join(new Conflicts(0, this.ambigAhead))
    return result
  }

  static addOrigins(group: readonly Pos[], context: readonly Pos[]) {
    let result = group.slice()
    for (let i = 0; i < result.length; i++) {
      let next = result[i]
      if (next.pos == 0) for (let pos of context) {
        if (pos.next == next.rule.name && !result.includes(pos)) result.push(pos)
      }
    }
    return result
  }
}

function conflictsAt(group: readonly Pos[]) {
  let result = Conflicts.none
  for (let pos of group) result = result.join(pos.conflicts())
  return result
}

// Applies automatic action precedence based on repeat productions.
// These are left-associative, so reducing the `R -> R R` rule has
// higher precedence.
function compareRepeatPrec(a: readonly Pos[], b: readonly Pos[]) {
  for (let pos of a) if (pos.rule.name.repeated) {
    for (let posB of b) if (posB.rule.name == pos.rule.name) {
      if (pos.rule.isRepeatWrap && pos.pos == 2) return 1
      if (posB.rule.isRepeatWrap && posB.pos == 2) return -1
    }
  }
  return 0
}

function cmpStr(a: string, b: string) {
  return a < b ? -1 : a > b ? 1 : 0
}

function termsAhead(rule: Rule, pos: number, after: readonly Term[], first: {[name: string]: (Term | null)[]}): Term[] {
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

  matches(other: Shift | Reduce, mapping: readonly number[]) {
    return other instanceof Shift && mapping[other.target.id] == mapping[this.target.id]
  }

  toString() { return "s" + this.target.id }

  map(mapping: readonly number[], states: readonly State[]) {
    let mapped = states[mapping[this.target.id]]
    return mapped == this.target ? this : new Shift(this.term, mapped)
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

  matches(other: Shift | Reduce, mapping: readonly number[]) {
    return other instanceof Reduce && other.rule.sameReduce(this.rule)
  }

  toString() { return `${this.rule.name.name}(${this.rule.parts.length})` }

  map() { return this }
}

function hashPositions(set: readonly Pos[]) {
  let h = 5381
  for (let pos of set) h = hash(h, pos.hash)
  return h
}

class ConflictContext {
  conflicts: Conflict[] = []
  constructor(readonly first: {[name: string]: (Term | null)[]}) {}
}

export class State {
  actions: (Shift | Reduce)[] = []
  actionPositions: (readonly Pos[])[] = []
  goto: Shift[] = []
  tokenGroup: number = -1
  defaultReduce: Rule | null = null

  constructor(public id: number,
              public set: readonly Pos[],
              public flags = 0,
              readonly skip: Term,
              readonly hash = hashPositions(set),
              readonly startRule: Term | null = null) {}

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
        let fullPos = Pos.addOrigins(positions, this.set), actionFullPos = Pos.addOrigins(this.actionPositions[i], this.set)
        let conflicts = conflictsAt(fullPos), actionConflicts = conflictsAt(actionFullPos)
        let diff = compareRepeatPrec(fullPos, actionFullPos) || conflicts.precedence - actionConflicts.precedence
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

  addAction(value: Shift | Reduce, positions: readonly Pos[], context: ConflictContext) {
    let conflict = this.addActionInner(value, positions)
    if (conflict) {
      let conflictPos = this.actionPositions[this.actions.indexOf(conflict)][0]
      let rules = [positions[0].rule.name, conflictPos.rule.name]
      if (context.conflicts.some(c => c.rules.some(r => rules.includes(r)))) return
      let error
      if (conflict instanceof Shift)
        error = `shift/reduce conflict between\n  ${conflictPos}\nand\n  ${positions[0].rule}`
      else
        error = `reduce/reduce conflict between\n  ${conflictPos.rule}\nand\n  ${positions[0].rule}`
      error += `\nWith input:\n  ${positions[0].trail(70)} · ${value.term} …`
      if (conflict instanceof Shift)
        error += findConflictShiftSource(positions[0], conflict.term, context.first)
      error += findConflictOrigin(conflictPos, positions[0])
      context.conflicts.push(new Conflict(error, rules))
    }
  }

  getGoto(term: Term) {
    return this.goto.find(a => a.term == term)
  }

  hasSet(set: readonly Pos[]) {
    return eqSet(this.set, set)
  }

  _actionsByTerm: null | {[id: number]: (Shift | Reduce)[]} = null

  actionsByTerm() {
    let result = this._actionsByTerm
    if (!result) {
      this._actionsByTerm = result = Object.create(null) as {[id: number]: (Shift | Reduce)[]}
      for (let action of this.actions)
        (result[action.term.id] || (result[action.term.id] = [])).push(action)
    }
    return result
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
  }

  eq(other: State) {
    let dThis = this.defaultReduce, dOther = other.defaultReduce
    if (dThis || dOther)
      return dThis && dOther ? dThis.sameReduce(dOther) : false
    return this.skip == other.skip &&
      this.tokenGroup == other.tokenGroup &&
      eqSet(this.actions, other.actions) &&
      eqSet(this.goto, other.goto)
  }
}

function closure(set: readonly Pos[], first: {[name: string]: (Term | null)[]}) {
  let added: Pos[] = [], redo: Pos[] = []
  function addFor(name: Term, ahead: readonly Term[], ambigAhead: readonly string[], skipAhead: Term, via: Pos) {
    for (let rule of name.rules) {
      let add = added.find(a => a.rule == rule)
      if (!add) {
        let existing = set.find(p => p.pos == 0 && p.rule == rule)
        add = existing ? new Pos(rule, 0, existing.ahead.slice(), existing.ambigAhead, existing.skipAhead, existing.via)
          : new Pos(rule, 0, [], none, skipAhead, via)
        added.push(add)
      }
      if (add.skipAhead != skipAhead)
        throw new GenError("Inconsistent skip sets after " + via.trail())
      add.ambigAhead = union(add.ambigAhead, ambigAhead)
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
             pos)
  }
  while (redo.length) {
    let add = redo.pop()!
    addFor(add.rule.parts[0], termsAhead(add.rule, 0, add.ahead, first),
           union(add.rule.conflicts[1].ambigGroups, add.rule.parts.length == 1 ? add.ambigAhead : none),
           add.rule.parts.length == 1 ? add.skipAhead : add.rule.skip, add)
  }

  let result = set.slice()
  for (let add of added) {
    add.ahead.sort((a, b) => a.hash - b.hash)
    add.finish()
    let origIndex = set.findIndex(p => p.pos == 0 && p.rule == add.rule)
    if (origIndex > -1) result[origIndex] = add
    else result.push(add)
  }
  return result.sort((a, b) => a.cmp(b))
}

function addTo<T>(value: T, array: T[]) {
  if (!array.includes(value)) array.push(value)
}

export function computeFirstSets(terms: TermSet) {
  let table: {[term: string]: (Term | null)[]} = Object.create(null)
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

class Conflict {
  constructor(readonly error: string, readonly rules: readonly Term[]) {}
}

function findConflictOrigin(a: Pos, b: Pos) {
  if (a.eqSimple(b)) return ""
  function via(root: Pos, start: Pos) {
    let hist = []
    for (let p = start.via!; !p.eqSimple(root); p = p.via!) hist.push(p)
    if (!hist.length) return ""
    hist.unshift(start)
    return hist.reverse().map((p, i) => "\n" + "  ".repeat(i + 1) + (p == start ? "" : "via ") + p).join("")
  }

  for (let p: Pos | null = a; p; p = p.via) for (let p2: Pos | null = b; p2; p2 = p2.via) {
    if (p.eqSimple(p2)) return "\nShared origin: " + p + via(p, a) + via(p, b)
  }
  return ""
}

// Search for the reason that a given 'after' token exists at the
// given pos, by scanning up the trail of positions. Because the `via`
// link is only one source of a pos, of potentially many, this
// requires a re-simulation of the whole path up to the pos.
function findConflictShiftSource(conflictPos: Pos, termAfter: Term, first: {[name: string]: (Term | null)[]}) {
  let pos = conflictPos, path: Term[] = []
  for (;;) {
    for (let i = pos.pos - 1; i >= 0; i--) path.push(pos.rule.parts[i])
    if (!pos.via) break
    pos = pos.via
  }
  path.reverse()
  let seen = new Set<number>()
  function explore(pos: Pos, i: number, hasMatch: Pos | null): string {
    if (i == path.length && hasMatch && !pos.next)
      return `\nThe reduction of ${conflictPos.rule.name} is allowed before ${termAfter} because of this rule:\n  ${hasMatch}`

    for (let next; next = pos.next;) {
      if (i < path.length && next == path[i]) {
        let inner = explore(pos.advance(), i + 1, hasMatch)
        if (inner) return inner
      }
      let after = pos.rule.parts[pos.pos + 1], match = pos.pos + 1 == pos.rule.parts.length ? hasMatch : null
      if (after && (after.terminal ? after == termAfter : first[after.name].includes(termAfter)))
        match = pos.advance()
      for (let rule of next.rules) {
        let hash = (rule.id << 5) + i + (match ? 555 : 0)
        if (!seen.has(hash)) {
          seen.add(hash)
          let inner = explore(new Pos(rule, 0, [], [], next, pos), i, match)
          if (inner) return inner
        }
      }
      if (!next.terminal && first[next.name].includes(null)) pos = pos.advance()
      else break
    }
    return ""
  }
  return explore(pos, 0, null)
}

// Builds a full LR(1) automaton
export function buildFullAutomaton(terms: TermSet, startTerms: Term[], first: {[name: string]: (Term | null)[]}) {
  let states: State[] = [], statesBySetHash: {[hash: number]: State[]} = {}
  let cores: {[hash: number]: Core[]} = {}
  let t0 = Date.now()
  function getState(core: readonly Pos[], top?: Term) {
    if (core.length == 0) return null
    let coreHash = hashPositions(core), byHash = cores[coreHash]
    let skip: Term | undefined
    for (let pos of core) {
      if (!skip) skip = pos.skip
      else if (skip != pos.skip) throw new GenError("Inconsistent skip sets after " + pos.trail())
    }
    if (byHash) for (let known of byHash) if (eqSet(core, known.set)) {
      if (known.state.skip != skip) throw new GenError("Inconsistent skip sets after " + known.set[0].trail())
      return known.state
    }

    let set = closure(core, first)
    let hash = hashPositions(set), forHash = statesBySetHash[hash] || (statesBySetHash[hash] = [])
    let found
    if (!top) for (let state of forHash) if (state.hasSet(set)) found = state
    if (!found) {
      found = new State(states.length, set, 0, skip!, hash, top)
      forHash.push(found)
      states.push(found)
      if (timing && states.length % 500 == 0)
        console.log(`${states.length} states after ${((Date.now() - t0) / 1000).toFixed(2)}s`)
    }
    ;(cores[coreHash] || (cores[coreHash] = [])).push(new Core(core, found))
    return found
  }

  for (const startTerm of startTerms) {
    const startSkip = startTerm.rules.length ? startTerm.rules[0].skip : terms.names["%noskip"]!
    getState(startTerm.rules.map(rule => new Pos(rule, 0, [terms.eof], none, startSkip, null).finish()), startTerm)
  }

  let conflicts = new ConflictContext(first)

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
          byTermPos.push([pos])
        } else {
          byTermPos[index].push(pos)
        }
      }
    }
    for (let i = 0; i < byTerm.length; i++) {
      let term = byTerm[i], positions = byTermPos[i].map(p => p.advance())
      if (term.terminal) {
        let set = applyCut(positions)
        let next = getState(set)
        if (next) state.addAction(new Shift(term, next), byTermPos[i], conflicts)
      } else {
        let goto = getState(positions)
        if (goto) state.goto.push(new Shift(term, goto))
      }
    }

    let replaced = false
    for (let pos of atEnd) for (let ahead of pos.ahead) {
      let count = state.actions.length
      state.addAction(new Reduce(ahead, pos.rule), [pos], conflicts)
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

  if (conflicts.conflicts.length) throw new GenError(conflicts.conflicts.map(c => c.error).join("\n\n"))

  // Resolve alwaysReduce and sort actions
  for (let state of states) state.finish()
  if (timing) console.log(`${states.length} states total.`)
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

// Verify that there are no conflicting actions or goto entries in the
// two given states (using the state ID remapping provided in mapping)
function canMerge(a: State, b: State, mapping: readonly number[]) {
  // If a goto for the same term differs, that makes the states
  // incompatible
  for (let goto of a.goto) for (let other of b.goto) {
    if (goto.term == other.term && mapping[goto.target.id] != mapping[other.target.id]) return false
  }
  // If there is an action where a conflicting action exists in the
  // other state, the merge is only allowed when both states have the
  // exact same set of actions for this term.
  let byTerm = b.actionsByTerm()
  for (let action of a.actions) {
    let setB = byTerm[action.term.id]
    if (setB && setB.some(other => !other.matches(action, mapping))) {
      if (setB.length == 1) return false
      let setA = a.actionsByTerm()[action.term.id]
      if (setA.length != setB.length || setA.some(a1 => !setB.some(a2 => a1.matches(a2, mapping))))
        return false
    }
  }
  return true
}

function mergeStates(states: readonly State[], mapping: readonly number[]) {
  let newStates = []
  for (let state of states) {
    let newID = mapping[state.id]
    if (!newStates[newID]) {
      newStates[newID] = new State(newID, state.set, 0, state.skip, state.hash, state.startRule)
      newStates[newID].tokenGroup = state.tokenGroup
      newStates[newID].defaultReduce = state.defaultReduce
    }
  }
  for (let state of states) {
    let newID = mapping[state.id], target = newStates[newID]
    target.flags |= state.flags
    for (let i = 0; i < state.actions.length; i++) {
      let action = state.actions[i].map(mapping, newStates)
      if (!target.actions.some(a => a.eq(action))) {
        target.actions.push(action)
        target.actionPositions.push(state.actionPositions[i])
      }
    }
    for (let goto of state.goto) {
      let mapped = goto.map(mapping, newStates)
      if (!target.goto.some(g => g.eq(mapped))) target.goto.push(mapped)
    }
  }
  return newStates
}

class Group {
  members: number[]
  constructor(readonly origin: number, member: number) { this.members = [member] }
}

function samePosSet(a: readonly Pos[], b: readonly Pos[]) {
  if (a.length != b.length) return false
  for (let i = 0; i < a.length; i++) if (!a[i].eqSimple(b[i])) return false
  return true
}

// Collapse an LR(1) automaton to an LALR-like automaton
function collapseAutomaton(states: readonly State[]): readonly State[] {
  let mapping: number[] = [], groups: Group[] = []
  assignGroups: for (let i = 0; i < states.length; i++) {
    let state = states[i]
    if (!state.startRule) for (let j = 0; j < groups.length; j++) {
      let group = groups[j], other = states[group.members[0]]
      if (state.tokenGroup == other.tokenGroup &&
          state.skip == other.skip &&
          !other.startRule &&
          samePosSet(state.set, other.set)) {
        group.members.push(i)
        mapping.push(j)
        continue assignGroups
      }
    }
    mapping.push(groups.length)
    groups.push(new Group(groups.length, i))
  }

  function spill(groupIndex: number, index: number) {
    let group = groups[groupIndex], state = states[group.members[index]]
    let pop = group.members.pop()!
    if (index != group.members.length) group.members[index] = pop
    for (let i = groupIndex + 1; i < groups.length; i++) {
      mapping[state.id] = i
      if (groups[i].origin == group.origin &&
          groups[i].members.every(id => canMerge(state, states[id], mapping))) {
        groups[i].members.push(state.id)
        return
      }
    }
    mapping[state.id] = groups.length
    groups.push(new Group(group.origin, state.id))
  }

  for (let pass = 1;; pass++) {
    let conflicts = false, t0 = Date.now()
    for (let g = 0, startLen = groups.length; g < startLen; g++) {
      let group = groups[g]
      for (let i = 0; i < group.members.length - 1; i++) {
        for (let j = i + 1; j < group.members.length; j++) {
          let idA = group.members[i], idB = group.members[j]
          if (!canMerge(states[idA], states[idB], mapping)) {
            conflicts = true
            spill(g, j--)
          }
        }
      }
    }
    if (timing) console.log(`Collapse pass ${pass}${conflicts ? `` : `, done`} (${((Date.now() - t0) / 1000).toFixed(2)}s)`)
    if (!conflicts) return mergeStates(states, mapping)
  }
}

function mergeIdentical(states: readonly State[]): readonly State[] {
  for (let pass = 1;; pass++) {
    let mapping: number[] = [], didMerge = false, t0 = Date.now()
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
    if (timing) console.log(`Merge identical pass ${pass}${didMerge ? "" : ", done"} (${((Date.now() - t0) / 1000).toFixed(2)}s)`)
    if (!didMerge) return states
    // Make sure actions point at merged state objects
    for (let state of newStates) if (!state.defaultReduce) {
      state.actions = state.actions.map(a => a.map(mapping, newStates))
      state.goto = state.goto.map(a => a.map(mapping, newStates))
    }
    // Renumber ids
    for (let i = 0; i < newStates.length; i++) newStates[i].id = i
    states = newStates
  }
}

const none: readonly any[] = []

export function finishAutomaton(full: readonly State[]) {
  return mergeIdentical(collapseAutomaton(full))
}
