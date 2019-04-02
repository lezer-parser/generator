import {Term, Grammar} from "./grammar/grammar"
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
              public stack: number[], // Holds state, pos pairs, length = (values.length + 1) * 2
              public values: Node[],
              public badness: number) {}

  get state() {
    return this.grammar.table[this.stack[this.stack.length - 2]]
  }

  get pos() { return this.stack[this.stack.length - 1] }

  set pos(value: number) { this.stack[this.stack.length - 1] = value }

  get depth() { return this.values.length }

  get topValue() { return this.values[this.values.length - 1] }

  set topValue(value: Node) { this.values[this.values.length - 1] = value }

  get topStart() { return this.stack[this.stack.length - 3] }

  toString() {
    let result = ""
    for (let i = 0;; i++) {
      result += `[${this.stack[i << 1]}]`
      if (i == this.values.length) break
      result += ` ${this.values[i]} `
    }
    return result
  }

  static start(grammar: Grammar) {
    return new Stack(grammar, [0, 0], [], 0)
  }

  reduceValue(depth: number, name: Term | null, append?: Node) {
    if (depth == 0) return Node.leaf(name, 0)
    if (name && !name.tag) name = null
    if (depth == 1 && !name) return this.values[this.values.length - 1]
    let children: Node[] = [], positions: number[] = []
    let startPos = this.values.length - depth, start = this.stack[(startPos << 1) + 1]
    for (let i = this.values.length - depth; i < this.values.length; i++) {
      this.values[i].flattenTo(children, positions, this.stack[(i << 1) + 1] - start, name)
    }
    let length = this.stack[this.stack.length - 1] - start
    if (append) { children.push(append); positions.push(length); length += append.length }
    return new Node(name, length, children, positions)
  }

  reduce(depth: number, name: Term, badness = this.badness, nextState: State | null = null) {
    let newValue = this.reduceValue(depth, name)
    let pos = this.stack[this.stack.length - 1]
    this.stack.length -= depth * 2
    this.stack.push((nextState || this.state.getGoto(name)!.target).id, pos)
    this.values.length -= depth
    this.values.push(newValue)
    this.badness = badness
  }

  apply(action: Shift | Reduce, next: Term, nextStart: number, nextEnd: number, badness = 0) {
    if (action instanceof Reduce) {
      this.reduce(action.rule.parts.length, action.rule.name)
    } else { // Shift
      this.stack.pop()
      this.stack.push(nextStart, action.target.id, nextEnd)
      this.values.push(Node.leaf(next, nextEnd - nextStart))
      if (badness) this.badness += badness
      else this.badness = (this.badness >> 1) + (this.badness >> 2) // (* 0.75)
    }
  }

  useCached(value: Node, next: State) {
    this.stack.push(next.id, this.pos + value.length /* FIXME */)
    this.values.push(value)
    this.badness >> 1 // FIXME
  }

  split() {
    return new Stack(this.grammar, this.stack.slice(), this.values.slice(), this.badness)
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

  flattenTo(nodes: Node[], positions: number[], offset: number, name: Term | null) {
    if (this.name && !(this.name == name && name.error)) {
      nodes.push(this)
      positions.push(offset)
    } else {
      for (let i = 0; i < this.children.length; i++)
        this.children[i].flattenTo(nodes, positions, offset + this.positions[i], name)
    }
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

export function parse(input: string, grammar: Grammar, cache = Node.leaf(null, 0),
                      verbose = false, strict = false): Node {
  let parses = [Stack.start(grammar)]
  let cacheIter = new TreeCursor(cache)

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

    if (!stack.state.ambiguous) { // FIXME this isn't robust
      for (let cached = cacheIter.nodeAt(stack.pos); cached;
           cached = cached.children.length && cached.positions[0] == 0 ? cached.children[0] : null) {
        let match = stack.state.getGoto(cached.name!)
        if (match) {
          stack.useCached(cached, match.target)
          addStack(parses, stack)
          continue parse
        }
      }
    }

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
      if (advance(stack, token, start, end)) continue parse // FIXME allow advancement via multiple tokenizers?
    }

    // If we're here, the stack failed to advance

    if (maxStart == input.length) {
      if (stack.depth == 1) return stack.values[0]
      else if (!strict) return stack.reduceValue(stack.depth, null)
    }

    if (!strict &&
        !(stack.badness > BADNESS_WILD && parses.some(s => s.pos >= stack.pos && s.badness <= stack.badness))) {
      if (!token) {
        token = maxStart == input.length ? grammar.terms.eof : grammar.terms.error
        start = maxStart
        end = start + (maxStart == input.length ? 0 : 1)
      }

      recoverByInsert(stack, parses, grammar, token, start, end, verbose)
      recoverByDelete(stack, parses, grammar, token, start, end, verbose)
    }
    if (!parses.length)
      throw new SyntaxError("No parse at " + start + " with " + token + " (stack is " + stack + ")")
  }
}

// FIXME move to stack method
function recoverByDelete(stack: Stack, parses: Stack[], grammar: Grammar, next: Term, nextStart: number, nextEnd: number,
                         verbose: boolean) {
  let value = stack.topValue, valueStart = stack.topStart
  // Don't do anything with the value when adding an error token to an
  // error or when the token doesn't have a tag
  if (next.tag && !(next.error && value.name && value.name.error)) {
    let node = Node.leaf(next, nextEnd - nextStart)
    if (value.name && value.name.error) {
      value = new Node(value.name, value.length + (nextEnd - stack.pos),
                       value.children.concat(node), value.positions.concat(nextStart - valueStart))
    } else {
      if (!next.error) node = new Node(grammar.terms.error, node.length, [node], [0])
      value = new Node(null, value.length + (nextEnd - stack.pos), [value, node], [0, nextStart - valueStart])
    }
  }
  stack.topValue = value
  stack.pos = nextEnd
  stack.badness += BADNESS_DELETE
  if (verbose) console.log("delete token " + next + ": " + stack, nextStart, nextEnd)
  addStack(parses, stack)
}

function recoverByInsert(stack: Stack, parses: Stack[], grammar: Grammar, next: Term, nextStart: number, nextEnd: number,
                         verbose: boolean) {
  // Scan for a state that has either a direct action or a recovery
  // action for next, without actually building up a new stack
  for (let top = stack.state, rest = stack.stack, offset = rest.length - 2;;) {
    if (top.terminals.some(a => a.term == next) ||
        top.recover.some(a => a.term == next)) break
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
      return
    }
    if (n == 0) { // FIXME
      rest = rest.slice()
      rest.push(top.id, 0)
      offset += 2
    } else {
      offset -= (n - 1) << 1
    }
    let goto = grammar.table[rest[offset - 2]].getGoto(term)
    if (!goto) return
    top = goto.target
  }

  // Now that we know there's a recovery to be found, do it again, the
  // expensive way, to get a new stack
  let result = stack.split()
  result.badness += BADNESS_RECOVER
  for (;;) {
    for (;;) {
      if (result.state.terminals.some(a => a.term == next)) {
        if (verbose) console.log("recovered to " + result)
        addStack(parses, result)
        return
      }
      let recover = result.state.recover.find(a => a.term == next)
      if (!recover) break
      if (verbose) console.log("skip from state " + result.state.id + " to " + recover.target.id)
      result.values.push(Node.leaf(grammar.terms.error, 0))
      result.stack.push(recover.target.id, result.pos)
    }
 
    let direct = result.state.terminals.find(a => a instanceof Reduce) as Reduce, pos
    if (direct) {
      result.reduce(direct.rule.parts.length, direct.rule.name)
    } else if (pos = result.state.set.find(p => p.pos > 0)) {
      // Force a reduce using this position
      let value = result.reduceValue(pos.pos, pos.rule.name, Node.leaf(grammar.terms.error, 0))
      result.values.length -= pos.pos
      result.values.push(value)
      let stackPos = result.pos
      result.stack.length -= pos.pos << 1
      result.stack.push(result.state.getGoto(pos.rule.name)!.target.id, stackPos)
    }
  }
}
