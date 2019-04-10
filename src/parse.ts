import {Term, Grammar, termTable} from "./grammar/grammar"
import {Token, Tokenizer} from "./grammar/token"
import {State, Shift, Reduce} from "./grammar/automaton"

const BADNESS_DELETE = 100, BADNESS_RECOVER = 100
const BADNESS_STABILIZING = 50, BADNESS_WILD = 150 // Limits in between which stacks are less agressively pruned

const MAX_BUFFER_LENGTH = 2048

const BALANCE_LEAF_LENGTH = MAX_BUFFER_LENGTH, BALANCE_BRANCH_FACTOR = 5

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
              public stack: number[], // Holds state, pos, node count triplets
              public values: (Node | number[])[],
              public valueStarts: number[],
              public badness: number) {}

  get state() { return this.grammar.table[this.stateID] }

  get stateID() { return this.stack[this.stack.length - 3] }

  get pos() { return this.stack[this.stack.length - 2] }

  get nodeCount() { return this.stack[this.stack.length - 1] }

  toString() {
    return "[" + this.stack.filter((_, i) => i % 3 == 0).join(",") + "] " +
      this.values.map(v => Array.isArray(v) ? Tree.fromBuffer(v, 0) : v).join(",")
  }

  static start(grammar: Grammar) {
    return new Stack(grammar, [0, 0, 0], [[]], [], 0)
  }

  reduceValue(childCount: number, start: number): Tree {
    let children: (Node | TreeBuffer)[] = [], positions: number[] = []
    for (let remaining = childCount; remaining > 0;) {
      let value = this.values.pop()!, valueStart = this.valueStarts.pop()!
      if (Array.isArray(value)) {
        let size = value.length >> 2, startIndex = Math.max(0, (size - remaining) << 2)
        if (startIndex < value.length)
          TreeBuffer.build(value, startIndex, start, children, positions)
        remaining -= size
        if (startIndex > 0) {
          value.length = startIndex
          this.values.push(value)
          this.valueStarts.push(0)
        }
      } else {
        children.push(value)
        positions.push(valueStart)
        remaining -= value.nodeCount
      }
    }
    return new Tree(childCount, children.reverse(), positions.reverse())
  }

  reduce(depth: number, name: Term) {
    let {pos, nodeCount} = this
    if (depth) {
      let newLen = this.stack.length - (depth * 3)
      let start = this.stack[newLen - 2], length = this.pos - start
      let count = nodeCount - this.stack[newLen - 1]
      if (name.tag) {
        let last
        if (length <= MAX_BUFFER_LENGTH &&
            Array.isArray(last = this.values[this.values.length - 1] as number[]) &&
            (last.length >> 2) >= count) {
          last.push(name.id, start, pos, count)
        } else {
          this.values.push(this.reduceValue(count, start).toNode(name, length))
          this.valueStarts.push(start)
        }
        nodeCount++
      } else if (name.repeats && length > BALANCE_LEAF_LENGTH) {
        let balanced = this.reduceValue(count, start).balance(name.repeats)
        this.values.push(balanced)
        this.valueStarts.push(start)
        nodeCount += balanced.nodeCount - count
      }
      this.stack.length = newLen
    }
    this.stack.push(this.state.getGoto(name)!.target.id, pos, nodeCount)
  }

  shiftValue(term: Term, start: number, end: number, count = 0) {
    let last = this.values[this.values.length - 1]
    if (!Array.isArray(last)) {
      this.values.push(last = [])
      this.valueStarts.push(0)
    }
    if (term.error && last.length && last[last.length - 4] == term.id &&
        (start == end || last[last.length - 3] == start)) return
    last.push(term.id, start, end, count)
    this.stack[this.stack.length - 1]++
  }

  apply(action: Shift | Reduce, next: Term, nextStart: number, nextEnd: number) {
    if (action instanceof Reduce) {
      this.reduce(action.rule.parts.length, action.rule.name)
    } else { // Shift
      this.stack[this.stack.length - 2] = nextStart
      this.stack.push(action.target.id, nextEnd, this.nodeCount)
      if (next.tag) this.shiftValue(next, nextStart, nextEnd)
      this.badness = (this.badness >> 1) + (this.badness >> 2) // (* 0.75)
    }
  }

  useCached(value: Node, start: number, next: State) {
    this.stack.push(next.id, start + value.length, this.nodeCount + value.nodeCount)
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
    this.shiftValue(this.grammar.terms.error, nextStart, nextEnd, next.tag ? 1 : 0)
    this.stack[this.stack.length - 2] = nextEnd
    this.badness += BADNESS_DELETE
    if (verbose) console.log("delete token " + next + ": " + this, nextStart, nextEnd)
  }

  canRecover(next: Term) {
    // Scan for a state that has either a direct action or a recovery
    // action for next, without actually building up a new stack
    for (let top = this.state, rest = this.stack, offset = rest.length - 3;;) {
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
        rest.push(top.id, 0, 0)
        offset += 3
      } else {
        offset -= (n - 1) * 3
      }
      let goto = this.grammar.table[rest[offset - 3]].getGoto(term)
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
        let pos = result.pos
        result.stack.push(recover.target.id, pos, result.nodeCount)
        result.shiftValue(this.grammar.terms.error, pos, pos)
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

  toTree(): SyntaxTree {
    return this.reduceValue(this.nodeCount, 0)
  }
}

import {takeFromHeap, addToHeap} from "./heap"

function compareStacks(a: Stack, b: Stack) { return a.pos - b.pos }

function addStack(heap: Stack[], stack: Stack, strict = stack.badness < BADNESS_STABILIZING || stack.badness > BADNESS_WILD): boolean {
  for (let i = 0; i < heap.length; i++) {
    let other = heap[i]
    if ((strict || other.state == stack.state) && other.pos == stack.pos) {
      let diff = stack.badness - other.badness || (stack.badness < BADNESS_STABILIZING ? 0 : stack.stack.length - other.stack.length)
      if (diff < 0) { heap[i] = stack; return true }
      else if (diff > 0) return false
    }
  }
  addToHeap(heap, stack, compareStacks)
  return true
}

export class Tree {
  constructor(readonly nodeCount: number,
              readonly children: (Node | TreeBuffer)[],
              readonly positions: number[]) {}

  toString() {
    return this.children.join()
  }

  toNode(name: Term, length: number) {
    return new Node(name, length, this.nodeCount + 1, this.children, this.positions)
  }

  get length() {
    let last = this.children.length - 1
    return last < 0 ? 0 : this.positions[last] + this.children[last].length
  }

  balanceRange(name: Term, from: number, to: number): Node {
    let start = this.positions[from], length = (this.positions[to - 1] + this.children[to - 1].length) - start
    let children = [], positions = [], size = 1
    if (length <= BALANCE_LEAF_LENGTH) {
      for (let i = from; i < to; i++) {
        let child = this.children[i]
        size += child.nodeCount
        children.push(child)
        positions.push(this.positions[i] - start)
      }
    } else {
      let maxChild = Math.max(BALANCE_LEAF_LENGTH, Math.ceil(length / BALANCE_BRANCH_FACTOR))
      for (let i = from; i < to;) {
        let groupFrom = i, groupStart = this.positions[i]
        i++
        for (; i < to; i++) {
          let nextEnd = this.positions[i] + this.children[i].length
          if (nextEnd - groupStart > maxChild) break
        }
        let sub = i == groupFrom + 1 ? this.children[groupFrom] : this.balanceRange(name, groupFrom, i)
        size += sub.nodeCount
        children.push(sub)
        positions.push(groupStart - start)
      }
    }
    return new Node(name, length, size, children, positions)
  }

  balance(name: Term): Node {
    return this.balanceRange(name, 0, this.children.length)
  }

  partial(start: number, end: number, offset: number, target: Node) {
    for (let i = 0; i < this.children.length; i++) {
      let from = this.positions[i]
      if (from >= end) break
      let child = this.children[i], to = from + child.length
      if (to > start) child.partial(start - from, end - from, offset + from, target)
    }
  }

  static fromBuffer(buffer: number[], start: number) {
    let children: (Node | TreeBuffer)[] = [], positions: number[] = []
    TreeBuffer.build(buffer, 0, start, children, positions)
    return new Tree(buffer.length >> 2, children, positions)
  }
}

export type SyntaxTree = TreeBuffer | Tree

export class Node extends Tree {
  constructor(readonly name: Term,
              private _length: number,
              nodeCount: number,
              children: (Node | TreeBuffer)[],
              positions: number[]) {
    super(nodeCount, children, positions)
  }

  get length() { return this._length } // Because super class already has a getter

  toString() {
    let name = this.name.tag
    return !name ? this.children.join() : name + (this.children.length ? "(" + this.children + ")" : "")
  }

  partial(start: number, end: number, offset: number, target: Node) {
    if (start <= 0 && end >= this.length) {
      target.children.push(this)
      target.positions.push(offset)
    } else {
      super.partial(start, end, offset, target)
    }
  }
}

// Tree buffers contain type,start,end,childCount quads for each node.
// The nodes are built in postfix order (with parent nodes being
// written after child nodes), but converted to prefix order when
// wrapped in a TreeBuffer.
export class TreeBuffer {
  constructor(readonly buffer: Uint16Array) {}

  get nodeCount() { return this.buffer.length >> 2 }

  get length() { return this.buffer[this.buffer.length - 2] }

  static copy(source: number[], startIndex: number, endIndex: number, startOffset: number): TreeBuffer {
    let buffer = new Uint16Array(endIndex - startIndex)
    let i = buffer.length, pos = endIndex
    function build() {
      let count = source[--pos], to = source[--pos], from = source[--pos], tag = source[--pos]
      let toPos = pos - (count << 2)
      while (pos > toPos) build()
      buffer[--i] = count; buffer[--i] = to - startOffset; buffer[--i] = from - startOffset; buffer[--i] = tag
    }
    while (pos > startIndex) build()
    return new TreeBuffer(buffer)
  }

  static build(source: number[], startIndex: number, startOffset: number, children: (Node | TreeBuffer)[], positions: number[]) {
    for (let pos = source.length; pos > startIndex;) {
      let partStart = pos, partOffset!: number
      let minStart = Math.max(startIndex, partStart - (MAX_BUFFER_LENGTH << 2))
      for (;;) {
        let count = source[partStart - 1], newStart = partStart - 4 - (count << 2)
        if (newStart < minStart) break
        partOffset = source[partStart - 3]
        partStart = newStart
      }
      if (partStart == pos) throw new Error("Oversized node in buffer")
      children.push(TreeBuffer.copy(source, partStart, pos, partOffset - startOffset))
      positions.push(partOffset - startOffset)
      pos = partStart
    }
  }

  toString() {
    let pos = 0
    let next = () => {
      let tag = this.buffer[pos], count = this.buffer[pos+3]
      pos += 4
      let children = "", end = pos + (count << 2)
      while (pos < end) children += (children ? "," : "") + next()
      return termTable[tag].tag! + (children ? "(" + children + ")" : "")
    }
    let result = ""
    while (pos < this.buffer.length) result += (result ? "," : "") + next()
    return result
  }

  partial(start: number, end: number, offset: number, target: Node) {
    if (start <= 0 && end >= this.length) {
      target.children.push(this)
      target.positions.push(offset)
    }    
  }
}

class CacheCursor {
  trees: Tree[]
  start = [0]
  index = [0]
  nextStart: number = 0

  constructor(tree: Tree) { this.trees = [tree] }

  // `pos` must be >= any previously given `pos` for this cursor
  nodeAt(pos: number) {
    if (pos < this.nextStart) return null

    for (;;) {
      let last = this.trees.length - 1
      if (last < 0) { // End of tree
        this.nextStart = 1e9
        return null
      }
      let top = this.trees[last], index = this.index[last]
      if (index == top.children.length) {
        this.trees.pop()
        this.start.pop()
        this.index.pop()
        continue
      }
      let next = top.children[index]
      let start = this.start[last] + top.positions[index]
      if (next instanceof TreeBuffer) {
        this.index[last]++
        this.nextStart = start + next.length
      } else if (start >= pos) {
        return start == pos ? next : null
      } else {
        this.index[last]++
        if (start + next.length >= pos) { // Enter this node
          this.trees.push(next)
          this.start.push(start)
          this.index.push(0)
        }
      }
    }
  }
}

class TokenCache { // FIXME cache whitespace separately for improved reuse and incremental parsing
  tokens: Token[] = []
  tokenizers: Tokenizer[] = []
  pos = 0
  index = 0

  update(grammar: Grammar, tokenizers: ReadonlyArray<Tokenizer>, input: string, pos: number) {
    if (pos > this.pos) { this.index = 0; this.pos = pos }
    tokenize: for (let tokenizer of tokenizers) {
      for (let i = 0; i < this.index; i++) if (this.tokenizers[i] == tokenizer) continue tokenize
      let token
      if (this.tokens.length <= this.index) this.tokens.push(token = new Token)
      else token = this.tokens[this.index]
      this.tokenizers[this.index++] = tokenizer
      if (!tokenizer.simulate(input, pos, token)) {
        if (token.start == input.length) {
          token.end = token.start
          token.term = grammar.terms.eof
        } else {
          token.end = token.start + 1
          token.term = grammar.terms.error
        }
      }
    }
  }

  hasOtherMatch(state: State, tokenIndex: number, sawEof: boolean) {
    for (let i = tokenIndex; i < this.index; i++) {
      let token = this.tokens[i]
      if (token.term.error || token.term.eof && sawEof) continue
      if (token.specialized && state.terminals.some(a => a.term == token.specialized) ||
          state.terminals.some(a => a.term == token.term)) return true
    }
    return false
  }

  some() {
    for (let i = 0; i < this.index; i++) if (!this.tokens[i].term.error) return this.tokens[i]
    return this.tokens[0]
  }
}

function hasOtherMatchInState(state: State, actionIndex: number, token: Token) {
  for (let i = actionIndex; i < state.terminals.length; i++) {
    let term = state.terminals[i].term
    if (term == token.term || term == token.specialized) return true
  }
  return false
}

export type ParseOptions = {cache?: Tree | null, verbose?: boolean, strict?: boolean}

export function parse(input: string, grammar: Grammar, {cache = null, verbose = false, strict = false}: ParseOptions): SyntaxTree {
  let parses = [Stack.start(grammar)]
  let cacheCursor = cache && new CacheCursor(cache)

  let tokens = new TokenCache

  parse: for (;;) {
    let stack = takeFromHeap(parses, compareStacks), pos = stack.pos

    if (cacheCursor && !stack.state.ambiguous) { // FIXME this isn't robust
      // FIXME need position after whitespace, not stack.pos
      for (let cached = cacheCursor.nodeAt(stack.pos); cached;) {
        let match = stack.state.getGoto(cached.name!)
        if (match) {
          stack.useCached(cached, stack.pos, match.target)
          addStack(parses, stack)
          continue parse
        }
        if (cached.children.length == 0 || cached.positions[0] > 0) break
        let inner = cached.children[0]
        if (inner instanceof Node) cached = inner
        else break
      }
    }

    tokens.update(grammar, grammar.tokenTable[stack.stateID], input, pos)

    let sawEof = false, state = stack.state, advanced = false
    for (let i = 0; i < tokens.index; i++) {
      let token = tokens.tokens[i]
      if (token.term == grammar.terms.error) continue
      if (sawEof && token.term == grammar.terms.eof) continue
      for (let j = token.specialized ? 1 : 0; j >= 0; j--) {
        let term = j ? token.specialized : token.term
        for (let k = 0, actions = state.terminals; k < actions.length; k++) {
          let action = actions[k]
          if (action.term != term) continue
          if (term.eof) sawEof = true
          advanced = true
          let localStack = stack
          if (hasOtherMatchInState(state, k + 1, token) || tokens.hasOtherMatch(state, i + 1, sawEof))
            localStack = localStack.split()
          localStack.apply(action, term, token.start, token.end)
          if (verbose) console.log(`${localStack} (via ${term} ${action}${localStack == stack ? "" : ", split"})`)
          addStack(parses, localStack, action instanceof Shift)
          j = 0 // Don't try a non-specialized version of a token when the specialized one matches
        }
      }
    }
    if (advanced) continue

    // If we're here, the stack failed to advance normally

    let token = tokens.some(), term = token.specialized || token.term
    if (term.eof) return stack.toTree()

    if (!strict &&
        !(stack.badness > BADNESS_WILD && parses.some(s => s.pos >= stack.pos && s.badness <= stack.badness))) {

      let inserted = stack.recoverByInsert(term, token.start, token.end, verbose)
      if (inserted) addStack(parses, inserted)
      stack.recoverByDelete(term, token.start, token.end, verbose)
      addStack(parses, stack)
    }
    if (!parses.length)
      throw new SyntaxError("No parse at " + token.start + " with " + term + " (stack is " + stack + ")")
  }
}
