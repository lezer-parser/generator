import {Term, Grammar} from "./grammar/grammar"
import {Action, State, Shift, Reduce} from "./grammar/automaton"

const BADNESS_DELETE = 110, BADNESS_INSERT = 100, BADNESS_RECOVER = 90
const BADNESS_STABILIZING = 50, BADNESS_WILD = 150 // Limits in between stacks are less agressively pruned

// (FIXME: this will go out of date before I know it, revisit at some
// point)
//
// Badness is a measure of how off-the-rails a given parse is. It is
// bumped when a recovery strategy is applied, and then reduced (by
// multiplication with a constant < 1) for every successful (real)
// token shifted.
//
// Stacks with a low badness are relatively credible parses that have
// shift matching the input in their recent history. Stacks with a
// high badness are deeply in the weeds and likely wrong. For each of
// these, we prune agressively by dropping stacks when another stack
// at the same position is looking better.
//
// For those in the BADNESS_STABILIZING - BADNESS_WILD range, we
// assume that they are in the process of trying to recover and allow
// a bunch of them to continue alongside each other to see which one
// works out better.
//
// Stacks with the same low badness score are likely to be valid GLR
// parsing branches, so in that case it's often a good idea to let
// both continue.
//
// When a stack fails to find an advancing action, recovery is only
// applied when its badness is < BADNESS_WILD, or no better parse
// exists at that point.

class Frame {
  constructor(readonly prev: Frame | null,
              readonly value: Node | null,
              readonly state: State,
              readonly start: number,
              readonly pos: number,
              readonly badness: number) {}

  get depth() {
    let d = 0
    for (let f: Frame | null = this; f; f = f.prev) d++
    return d
  }

  parent(n: number) {
    let stack: Frame = this
    for (let i = 0; i < n; i++) stack = stack.prev!
    return stack
  }

  toString() {
    return this.prev ? `${this.prev} ${this.value} [${this.state.id}]` : `[${this.state.id}]`
  }

  eq(other: Frame): boolean {
    return this.state == other.state && this.pos == other.pos &&
      (this.prev == other.prev || (this.prev && other.prev ? this.prev.eq(other.prev) : false))
  }

  reduce(depth: number, name: Term, badness = this.badness, nextState: State | null = null): Frame {
    // FIXME destructively update when depth==1?
    let prev = this.parent(depth)
    return new Frame(prev, Node.fromStack(this, depth, name),
                     nextState || prev.state.getGoto(name)!.target, prev.start, this.pos, badness)
  }
}

import {takeFromHeap, addToHeap} from "./heap"

function compareFrames(a: Frame, b: Frame) { return a.pos - b.pos }

function addFrame(heap: Frame[], frame: Frame, strict = frame.badness < BADNESS_STABILIZING || frame.badness > BADNESS_WILD): boolean {
  for (let i = 0; i < heap.length; i++) {
    let other = heap[i]
    if ((strict || other.state == frame.state) && other.pos == frame.pos) {
      let diff = frame.badness - other.badness || (frame.badness < BADNESS_STABILIZING ? 0 : frame.depth - other.depth)
      if (diff < 0) { heap[i] = frame; return true }
      else if (diff > 0) return false
    }
  }
  addToHeap(heap, frame, compareFrames)
  return true
}

const none: any[] = []

export class Node {
  constructor(readonly name: Term | null,
              readonly length: number,
              readonly children: Node[],
              readonly positions: number[]) {}

  toString() {
    return this.name ? (this.children.length ? this.name.tag + "(" + this.children + ")" : this.name.tag!) : this.children.join()
  }

  static leaf(name: Term | null, length: number) {
    if (name && !name.tag) name = null
    return name || length ? new Node(name, length, none, none) : Node.empty
  }

  static fromStack(stack: Frame, depth: number, name: Term | null) {
    if (depth == 0) return Node.leaf(name, 0)
    if (name && !name.tag) name = null
    if (depth == 1 && !name) return stack.value!
    let frames: Frame[] = []
    for (let i = 0, s = stack; i < depth; s = s.prev!, i++) frames.push(s)
    let children: Node[] = [], positions: number[] = []
    let start = frames[frames.length - 1].start
    for (let i = frames.length - 1; i >= 0; i--) {
      let frame = frames[i], value = frame.value!
      if (value.name) {
        children.push(value)
        positions.push(frame.start - start)
      } else {
        for (let i = 0; i < value.children.length; i++) {
          children.push(value.children[i])
          positions.push(frame.start + value.positions[i] - start)
        }
      }
    }
    return new Node(name, stack.start + stack.value!.length, children, positions)
  }

  partial(start: number, end: number, offset: number, target: Node) {
    if (start <= 0 && end >= this.length) {
      target.children.push(this)
      target.positions.push(offset)
    } else {
      for (let i = 0; i < this.children.length; i++) {
        let from = this.positions[i]
        if (from >= end) break
        let child = this.children[i], to = from + child.length
        if (to > start) child.partial(start - from, end - from, offset + from, target)
      }
    }
  }

  static empty = new Node(null, 0, none, none)
}

class TreeCursor {
  nodes: Node[]
  start = [0]
  index = [0]

  constructor(node: Node) { this.nodes = [node] }

  // `pos` must be >= any previously given `pos` for this cursor
  nodeAt(pos: number) {
    for (;;) {
      let last = this.nodes.length - 1
      if (last < 0) return null
      let top = this.nodes[last], index = this.index[last]
      if (index == top.children.length) {
        this.nodes.pop()
        this.start.pop()
        this.index.pop()
        continue
      }
      let next = top.children[index]
      let start = this.start[last] + top.positions[index]
      if (start >= pos) return start == pos ? next : null
      this.index[last]++
      if (start + next.length >= pos) { // Enter this node
        this.nodes.push(next)
        this.start.push(start)
        this.index.push(0)
      }
    }
  }
}

function applyAction(stack: Frame, action: Action, next: Term, nextStart: number, nextEnd: number, badness = 0): Frame {
  if (action instanceof Reduce) {
    return stack.reduce(action.rule.parts.length, action.rule.name)
  } else { // Shift
    return new Frame(stack, Node.leaf(next, nextEnd - nextStart), (action as Shift).target,
                     nextStart, nextEnd, badness ? stack.badness + badness : (stack.badness >> 1) + (stack.badness >> 2)) // (* 0.75)
  }
}

export function parse(input: string, grammar: Grammar, cache = Node.leaf(null, 0),
                      verbose = false, strict = false): Node {
  let parses = [new Frame(null, null, grammar.table[0], 0, 0, 0)]
  let cacheIter = new TreeCursor(cache)

  function advance(stack: Frame, next: Term, nextStart: number, nextEnd: number) {
    let found = false
    for (let action of stack.state.terminals) if (action.term == next) {
      let frame = applyAction(stack, action, next, nextStart, nextEnd)
      if (verbose) console.log(`${frame} (via ${next} ${action})`, input.slice(nextStart, nextEnd))
      found = true
      addFrame(parses, frame, action instanceof Shift)
    }
    return found
  }

  parse: for (;;) {
    let stack = takeFromHeap(parses, compareFrames)

    if (!stack.state.ambiguous) {
      for (let cached = cacheIter.nodeAt(stack.pos); cached;
           cached = cached.children.length && cached.positions[0] == 0 ? cached.children[0] : null) {
        let match = stack.state.getGoto(cached.name!)
        if (match) {
          addFrame(parses, new Frame(stack, cached, match.target, stack.pos /* FIXME */, stack.pos + cached.length,
                                     stack.badness >> 1)) // FIXME how to adjust badness for a whole tree? zero it?
          continue parse
        }
      }
    }

    let token: Term | null = null, start = stack.pos, end = start, sawEof = false
    let maxStart = start, advanced = false
    // FIXME cache token info
    for (let tokenCx of grammar.tokenTable[stack.state.id]) {
      let curPos = stack.pos
      if (tokenCx.skip) {
        let skip = tokenCx.skip.simulate(input, curPos)
        if (skip) { curPos = skip.end; maxStart = Math.max(maxStart, skip.end) }
      }
      if (curPos == input.length) {
        if (sawEof) continue
        sawEof = true
        token = grammar.terms.eof
        start = end = curPos
      } else {
        let found = tokenCx.tokens.simulate(input, curPos)
        if (!found) continue
        start = curPos
        end = found.end
        token = found.term
        let specialized = grammar.specialized[token.name]
        if (specialized) {
          let value = specialized[input.slice(start, end)]
          if (value && advance(stack, value, start, end)) {
            advanced = true
            continue
          }
        }
      }
      if (advance(stack, token, start, end)) advanced = true
    }
    if (!advanced && maxStart == input.length) {
      if (!stack.prev!.prev) return stack.value!
      return Node.fromStack(stack, stack.depth - 1, null)
    }

    if (!advanced && !strict &&
        !(stack.badness > BADNESS_WILD && parses.some(s => s.pos >= stack.pos && s.badness <= stack.badness))) {
      if (!token) {
        token = maxStart == input.length ? grammar.terms.eof : grammar.terms.error
        start = maxStart
        end = start + (maxStart == input.length ? 0 : 1)
      }

      // Prefer recovering with a recovery table, only use insert when
      // that doesn't work and the stack's badness is low
      if (!recoverByTable(stack, parses, grammar, token, start, end) &&
          stack.badness < BADNESS_WILD)
        recoverByInsert(stack, parses, grammar, token, start)
      recoverByDelete(stack, parses, grammar, token, start, end)
    }
    if (!parses.length)
      throw new SyntaxError("No parse at " + start + " with " + token + " (stack is " + stack + ")")
  }
}

function recoverByInsert(stack: Frame, parses: Frame[], grammar: Grammar, next: Term, nextStart: number) {
  if (next.error) return

  function explore(stack: Frame) {
    for (let action of stack.state.terminals) {
      if (action instanceof Reduce && !action.term.eof) {
        let newStack = applyAction(stack, action, grammar.terms.error, nextStart, nextStart)
        if (addFrame(parses, newStack)) explore(newStack)
      } else if (action instanceof Shift && action.target.terminals.some(a => a.term == next)) {
        addFrame(parses, applyAction(stack, action, grammar.terms.error, nextStart, nextStart, BADNESS_INSERT))
      }
    }
  }
  explore(stack)
}

function recoverByDelete(stack: Frame, parses: Frame[], grammar: Grammar, next: Term, nextStart: number, nextEnd: number) {
  let value = stack.value!
  // Don't do anything when adding an error token to an error or when the token doesn't have a tag
  if (next.tag && !(next.error && value.name && value.name.error)) {
    let node = Node.leaf(next, nextEnd - nextStart)
    if (value.name && value.name.error) {
      value = new Node(value.name, value.length + (nextEnd - stack.pos),
                       value.children.concat(node), value.positions.concat(nextStart - stack.start))
    } else {
      if (!next.error) node = new Node(grammar.terms.error, node.length, [node], [0])
      value = new Node(null, value.length + (nextEnd - stack.pos), [value, node], [0, nextStart - stack.start])
    }
  }
  addFrame(parses, new Frame(stack.prev, value, stack.state, stack.start, nextEnd, stack.badness + BADNESS_DELETE))
}

function recoverByTable(stack: Frame, parses: Frame[], grammar: Grammar, next: Term, nextStart: number, nextEnd: number) {
  for (let depth = 0, cur: Frame | null = stack; cur; cur = cur.prev, depth++) {
    let action = cur.state.recover.find(a => a.term == next)
    if (action) {
      let newStack = cur
      for (let state of action.states) {
        if (newStack == cur && depth > 0)
          newStack = stack.reduce(depth, grammar.terms.error, BADNESS_RECOVER, state)
        else
          newStack = new Frame(newStack, Node.leaf(grammar.terms.error, 0), state, stack.pos, stack.pos, BADNESS_RECOVER)
      }
      addFrame(parses, newStack)
      return true
    }
  }
  return false
}
