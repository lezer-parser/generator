import {Term, Grammar, termTable} from "./grammar/grammar"
import {State, Shift, Reduce} from "./grammar/automaton"

const BADNESS_DELETE = 100, BADNESS_RECOVER = 100
const BADNESS_STABILIZING = 50, BADNESS_WILD = 150 // Limits in between which stacks are less agressively pruned

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

class Stack {
  constructor(readonly grammar: Grammar,
              public stack: number[], // Holds state, pos pairs
              public values: (Node | number[])[],
              public valueStarts: number[],
              public badness: number) {}

  get state() {
    return this.grammar.table[this.stack[this.stack.length - 2]]
  }

  get pos() { return this.stack[this.stack.length - 1] }

  get depth() { return this.values.length }

  toString() {
    return "[" + this.stack.filter((_, i) => i % 2 == 0).join(",") + "] " +
      this.values.map(v => v instanceof Node ? v : NodeBuffer.build(v, 0, 0)).join(",")
  }

  static start(grammar: Grammar) {
    return new Stack(grammar, [0, 0], [[]], [0], 0)
  }

  reduceValue(start: number, name: Term) {
    let children: (Node | NodeBuffer)[] = [], positions: number[] = []
    for (;;) {
      let value = this.values.pop()!, valueStart = this.valueStarts.pop()!
      if (valueStart >= start) {
        positions.push(valueStart - start)
        children.push(value instanceof Node ? value : NodeBuffer.build(value, 0, valueStart))
      } else {
        let buffer = value as number[], index = 0
        while (index < buffer.length && buffer[index + 1] < start) index += 3
        positions.push(0)
        children.push(NodeBuffer.build(buffer, index, start))
        buffer.length = index
        this.values.push(value)
        this.valueStarts.push(valueStart)
      }
      if (valueStart <= start) break
    }
    this.values.push(new Node(name, this.pos - start, children, positions))
    this.valueStarts.push(start)
  }

  reduce(depth: number, name: Term) {
    let pos = this.pos
    if (depth) {
      let newLen = this.stack.length - (depth << 1)
      let start = this.stack[newLen - 1]
      this.stack.length = newLen
      let last = this.values[this.values.length - 1]
      if (name.tag) {
        if (Array.isArray(last) && this.valueStarts[this.values.length - 1] <= start)
          last.push(name.id, start, pos)
        else
          this.reduceValue(start, name)
      }
    }
    this.stack.push(this.state.getGoto(name)!.target.id, pos)
  }

  shiftValue(term: Term, start: number, end: number) {
    let last = this.values[this.values.length - 1]
    if (!Array.isArray(last)) {
      this.values.push(last = [])
      this.valueStarts.push(start)
    }
    if (term.error && last.length && last[last.length - 3] == term.id &&
        (start == end || last[last.length - 2] == start)) return
    last.push(term.id, start, end)
  }

  apply(action: Shift | Reduce, next: Term, nextStart: number, nextEnd: number) {
    if (action instanceof Reduce) {
      this.reduce(action.rule.parts.length, action.rule.name)
    } else { // Shift
      this.stack.pop()
      this.stack.push(nextStart, action.target.id, nextEnd)
      if (next.tag) this.shiftValue(next, nextStart, nextEnd)
      this.badness = (this.badness >> 1) + (this.badness >> 2) // (* 0.75)
    }
  }

  useCached(value: Node, start: number, next: State) {
    this.stack.push(next.id, start + value.length /* FIXME */)
    this.values.push(value)
    this.valueStarts.push(start)
    this.badness >> 1 // FIXME
  }

  split() {
    return new Stack(this.grammar, this.stack.slice(), this.values.map(v => Array.isArray(v) ? v.slice() : v),
                     this.valueStarts.slice(), this.badness)
  }

  recoverByDelete(next: Term, nextStart: number, nextEnd: number, verbose: boolean) {
    if (next.tag) this.shiftValue(next, nextStart, nextEnd)
    // FIXME merge errors?
    this.shiftValue(this.grammar.terms.error, nextStart, nextEnd)
    this.stack[this.stack.length - 1] = nextEnd
    this.badness += BADNESS_DELETE
    if (verbose) console.log("delete token " + next + ": " + this, nextStart, nextEnd)
  }

  canRecover(next: Term) {
    // Scan for a state that has either a direct action or a recovery
    // action for next, without actually building up a new stack
    for (let top = this.state, rest = this.stack, offset = rest.length - 2;;) {
      if (top.terminals.some(a => a.term == next) ||
          top.recover.some(a => a.term == next)) return true
      // Find a way to reduce from here
      let term, n
      let direct = top.terminals.find(a => a instanceof Reduce) as Reduce, pos
      if (direct) {
        term = direct.rule.name
        n = direct.rule.parts.length
      } else if (pos = top.set.find(p => p.pos > 0)) { // FIXME store this in the run-time states
        term = pos.rule.name
        n = pos.pos
      } else {
        return false
      }
      if (n == 0) { // FIXME
        rest = rest.slice()
        rest.push(top.id, 0)
        offset += 2
      } else {
        offset -= (n - 1) << 1
      }
      let goto = this.grammar.table[rest[offset - 2]].getGoto(term)
      if (!goto) return false
      top = goto.target
    }
  }

  recoverByInsert(next: Term, nextStart: number, nextEnd: number, verbose: boolean): Stack | null {
    if (!this.canRecover(next)) return null
    // Now that we know there's a recovery to be found, run the
    // reduces again, the expensive way, updating the stack

    let result = this.split()
    result.badness += BADNESS_RECOVER
    for (;;) {
      for (;;) {
        if (result.state.terminals.some(a => a.term == next)) {
          if (verbose) console.log("recovered to " + result)
          return result
        }
        let recover = result.state.recover.find(a => a.term == next)
        if (!recover) break
        if (verbose) console.log("skip from state " + result.state.id + " to " + recover.target.id)
        result.shiftValue(this.grammar.terms.error, result.pos, result.pos) // FIXME zero-length nodes are tricky
        result.stack.push(recover.target.id, result.pos)
      }
      
      let direct = result.state.terminals.find(a => a instanceof Reduce) as Reduce, pos
      if (direct) {
        result.reduce(direct.rule.parts.length, direct.rule.name)
      } else if (pos = result.state.set.find(p => p.pos > 0)) {
        // Force a reduce using this position
        result.shiftValue(this.grammar.terms.error, result.pos, result.pos)
        result.reduce(pos.pos, pos.rule.name)
      }
    }
  }
}

import {takeFromHeap, addToHeap} from "./heap"

function compareStacks(a: Stack, b: Stack) { return a.pos - b.pos }

function addStack(heap: Stack[], stack: Stack, strict = stack.badness < BADNESS_STABILIZING || stack.badness > BADNESS_WILD): boolean {
  for (let i = 0; i < heap.length; i++) {
    let other = heap[i]
    if ((strict || other.state == stack.state) && other.pos == stack.pos) {
      let diff = stack.badness - other.badness || (stack.badness < BADNESS_STABILIZING ? 0 : stack.depth - stack.depth)
      if (diff < 0) { heap[i] = stack; return true }
      else if (diff > 0) return false
    }
  }
  addToHeap(heap, stack, compareStacks)
  return true
}

export class Node {
  constructor(readonly name: Term | null,
              readonly length: number,
              readonly children: (Node | NodeBuffer)[],
              readonly positions: number[]) {}

  toString() {
    return this.name ? (this.children.length ? this.name.tag + "(" + this.children + ")" : this.name.tag!) : this.children.join()
  }

/* FIXME restore
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
  }*/
}

//const MAX_BUFFER = 8192

// Node buffers contain type,start,end triplets for each node. They
// are stored in postfix order (with parent nodes being written after
// child nodes).
export class NodeBuffer {
  constructor(readonly buffer: Uint16Array) {}

  static build(source: number[], start: number, offset: number) {
    if (!Array.isArray(source)) throw new Error("X")
    let buffer = new Uint16Array(source.length - start)
    let i = buffer.length
    function build(pos: number) {
      let to = source[--pos], from = source[--pos], tag = source[--pos]
      while (pos > start && source[pos - 1] > from) pos = build(pos)
      buffer[--i] = to; buffer[--i] = from; buffer[--i] = tag
      return pos
    }
    for (let pos = source.length; pos > start;) pos = build(pos)
    return new NodeBuffer(buffer)
  }

  toString() {
    let pos = 0
    let next = () => {
      let tag = this.buffer[pos], to = this.buffer[pos+2]
      pos += 3
      let children = ""
      while (pos < this.buffer.length && this.buffer[pos + 1] < to)
        children += (children ? "," : "") + next()
      return termTable[tag].tag! + (children ? "(" + children + ")" : "")
    }
    let result = ""
    while (pos < this.buffer.length) result += (result ? "," : "") + next()

    return result
  }
}

/*
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
*/

export function parse(input: string, grammar: Grammar, cache = null, verbose = false, strict = false): Node {
  let parses = [Stack.start(grammar)]
//  let cacheIter = new TreeCursor(cache)

  function advance(stack: Stack, next: Term, nextStart: number, nextEnd: number) {
    let used = false
    for (let i = 0, actions = stack.state.terminals; i < actions.length; i++) {
      let action = actions[i]
      if (action.term != next) continue
      let local = stack
      for (let j = i + 1; j < actions.length; j++) if (actions[j].term == next) {
        local = stack.split()
        break
      }
      used = true
      local.apply(action, next, nextStart, nextEnd)
      if (verbose) console.log(`${local} (via ${next} ${action})`, input.slice(nextStart, nextEnd))
      addStack(parses, local, action instanceof Shift)
    }
    return used
  }

  parse: for (;;) {
    let stack = takeFromHeap(parses, compareStacks)

/*    if (!stack.state.ambiguous) { // FIXME this isn't robust
      for (let cached = cacheIter.nodeAt(stack.pos); cached;
           cached = cached.children.length && cached.positions[0] == 0 ? cached.children[0] : null) {
        let match = stack.state.getGoto(cached.name!)
        if (match) {
          stack.useCached(cached, match.target)
          addStack(parses, stack)
          continue parse
        }
      }
    }*/

    let token: Term | null = null, start = stack.pos, end = start, sawEof = false
    let maxStart = start
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
          if (value && advance(stack, value, start, end)) continue parse
        }
      }
      if (advance(stack, token, start, end)) continue parse // FIXME allow advancement via multiple tokens?
    }

    // If we're here, the stack failed to advance

    if (maxStart == input.length) {
      if (stack.depth != 1) stack.reduce(stack.depth, grammar.terms.error)
      let value = stack.values[0]
      return Array.isArray(value) ? NodeBuffer.build(value, 0, 0) as any as Node : value
    }

    if (!strict &&
        !(stack.badness > BADNESS_WILD && parses.some(s => s.pos >= stack.pos && s.badness <= stack.badness))) {
      if (!token) {
        token = maxStart == input.length ? grammar.terms.eof : grammar.terms.error
        start = maxStart
        end = start + (maxStart == input.length ? 0 : 1)
      }

      let inserted = stack.recoverByInsert(token, start, end, verbose)
      if (inserted) addStack(parses, inserted)
      stack.recoverByDelete(token, start, end, verbose)
      addStack(parses, stack)
    }
    if (!parses.length)
      throw new SyntaxError("No parse at " + start + " with " + token + " (stack is " + stack + ")")
  }
}
