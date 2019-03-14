class Term {
  readonly terminal: boolean
  constructor(readonly name: string) {
    this.terminal = !/^[A-Z]/.test(name)
  }
  toString() { return this.name }
  cmp(other: Term) { return this == other ? 0 : this.name < other.name ? -1 : 1 }
}

class TermSet {
  terms: Term[]
  constructor() { this.terms = [] }
  get(name: string) {
    let found = this.terms.find(t => t.name == name)
    if (!found) this.terms.push(found = new Term(name))
    return found
  }
}

class Rule {
  name: Term
  parts: ReadonlyArray<Term>
  constructor(str: string, terms: TermSet) {
    let [, name, expr] = /(\w+)\s*->\s*(.*)/.exec(str)!
    this.name = terms.get(name)
    this.parts = expr.split(/\s+/).filter(x => x).map(n => terms.get(n))
  }

  cmp(rule: Rule) {
    return this.name.cmp(rule.name) ||
      this.parts.length - rule.parts.length ||
      this.parts.reduce((r, s, i) => r || s.cmp(rule.parts[i]), 0)
  }

  toString() {
    return this.name + "->" + this.parts.join("")
  }
}

class Grammar {
  terms = new TermSet
  rules: Rule[]
  nonTerminals: Term[]
  terminals: Term[]
  first: {[name: string]: Term[]}
  follows: {[name: string]: Term[]}

  constructor(rules: string[]) {
    this.rules = rules.map(rule => new Rule(rule, this.terms))
    this.nonTerminals = this.terms.terms.filter(t => !t.terminal)
    this.terminals = this.terms.terms.filter(t => t.terminal)
    this.first = computeFirst(this.rules, this.nonTerminals)
    this.follows = computeFollows(this.rules, this.nonTerminals, this.first)
  }

  closure(set: ReadonlyArray<Pos>) {
    let result = set.slice()
    for (let pos of result) {
      let next = pos.next
      if (!next || next.terminal) continue
      for (let rule of this.rules) if (rule.name == next) {
        if (!result.some(p => p.pos == 0 && p.rule == rule))
          result.push(new Pos(rule, 0))
      }
    }
    return result.sort((a, b) => a.cmp(b))
  }

  table() {
    let states: State[] = [], grammar = this
    function explore(set: Pos[]) {
      if (set.length == 0) return null
      set = grammar.closure(set)
      let state = states.find(s => sameSet(s.set, set))
      if (!state) {
        states.push(state = new State(states.length, set))
        for (let term of grammar.terminals) {
          if (term.name == "#") continue
          let shift = explore(advance(set, term))
          if (shift) state.addAction(new Goto(term, shift))
        }
        for (let nt of grammar.nonTerminals) {
          let goto = explore(advance(set, nt))
          if (goto) state.goto.push(new Goto(nt, goto))
        }
        for (let pos of set) {
          let next = pos.next
          if (next == null) {
            for (let follow of grammar.follows[pos.rule.name.name])
              state.addAction(new Reduce(follow, pos.rule), pos)
          } else if (next.name == "#") { // FIXME robust EOF representation
            state.addAction(new Accept(next), pos)
          }
        }
      }
      return state
    }

    explore(grammar.rules.filter(rule => rule.name.name == "S").map(rule => new Pos(rule, 0)))
    return states
  }
}

function add<T>(value: T, array: T[]) {
  if (!array.includes(value)) array.push(value)
}

function computeFirst(rules: Rule[], nonTerminals: Term[]) {
  let table: {[term: string]: Term[]} = {}
  for (let t of nonTerminals) table[t.name] = []
  for (;;) {
    let change = false
    for (let rule of rules) {
      let set = table[rule.name.name]
      let found = false, startLen = set.length
      for (let part of rule.parts) {
        found = true
        if (part.terminal) {
          add(part, set)
        } else {
          for (let t of table[part.name]) {
            if (t == null) found = false
            else add(t, set)
          }
        }
        if (found) break
      }
      if (!found) add(null, set)
      if (set.length > startLen) change = true
    }
    if (!change) return table
  }
}

function computeFollows(rules: Rule[], nonTerminals: Term[], first: {[name: string]: Term[]}) {
  let table: {[name: string]: Term[]} = {}
  for (let t of nonTerminals) table[t.name] = []
  for (;;) {
    let change = false
    for (let rule of rules) {
      for (let i = 0; i < rule.parts.length; i++) {
        let part = rule.parts[i], toEnd = true
        if (part.terminal) continue
        let set = table[part.name], startLen = set.length
        for (let j = i + 1; j < rule.parts.length; j++) {
          let next = rule.parts[j]
          toEnd = false
          if (next.terminal) {
            add(next, set)
          } else {
            for (let f of first[next.name]) {
              if (f == null) toEnd = true
              else add(f, set)
            }
          }
          if (!toEnd) break
        }
        if (toEnd) for (let follow of table[rule.name.name]) add(follow, set)
        if (set.length > startLen) change = true
      }
    }
    if (!change) return table
  }
}

class Pos {
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

interface Action {
  term: Term
  eq(other: Action): boolean
}

class Goto implements Action {
  constructor(readonly term: Term, readonly target: State) {}

  eq(other: Action): boolean { return other instanceof Goto && other.target == this.target }

  toString() { return "goto " + this.target.id }
}

class Accept implements Action {
  constructor(readonly term: Term) {}

  eq(other: Action) { return other instanceof Accept }

  toString() { return "accept" }
}

class Reduce implements Action {
  constructor(readonly term: Term, readonly rule: Rule) {}

  eq(other: Action): boolean { return other instanceof Reduce && other.rule == this.rule }

  toString() { return "reduce " + this.rule }
}

class State {
  terminals: Action[] = []
  goto: Goto[] = []
  ambiguous = false

  constructor(readonly id: number, readonly set: ReadonlyArray<Pos>) {}

  toString() {
    return this.id + "=" + this.set.join() +  ": " +
      this.terminals.map(t => t.term + "=" + t).join(",") + "|" + this.goto.map(g => g.term + "=" + g).join(",")
  }

  addAction(value: Action, pos?: Pos) {
    for (let action of this.terminals) {
      if (action.term == value.term) {
        if (action.eq(value)) return
        // FIXME only allow duplicates when choices are explicitly marked
        // as ambiguous
        // throw new Error("Conflict at " + pos + ": " + action + " vs " + value)
        this.ambiguous = true
        console.log("duplicate rule added in " + this.id + " for", value.term + " " + value + " / " + action)
      }
    }
    this.terminals.push(value)
  }

  forEachAction(term: Term, f: (action: Action) => void) {
    for (let a of this.terminals) if (a.term == term) f(a)
  }

  getGoto(term: Term) {
    return this.goto.find(a => a.term == term)
  }
}

class Frame {
  constructor(readonly prev: Frame | null,
              readonly value: Node | null,
              readonly state: State,
              readonly start: number,
              readonly pos: number) {}

  toString() {
    return this.prev ? `${this.prev} ${this.value} [${this.state.id}]` : `[${this.state.id}]`
  }

  eq(other: Frame): boolean {
    return this.state == other.state && this.pos == other.pos &&
      (this.prev == other.prev || (this.prev && other.prev ? this.prev.eq(other.prev) : false))
  }
}

import {takeFromHeap, addToHeap} from "./heap"

function compareFrames(a: Frame, b: Frame) { return a.pos - b.pos }

function addFrame(heap: Frame[], frame: Frame) {
  if (!heap.some(f => f.eq(frame))) addToHeap(heap, frame, compareFrames)
}

const none: any[] = []

class Node {
  constructor(readonly name: Term | null,
              readonly length: number,
              readonly children: Node[],
              readonly positions: number[]) {}

  toString() {
    return this.name ? (this.children.length ? this.name + "(" + this.children + ")" : this.name.name) : this.children.join()
  }

  static leaf(name: Term | null, length: number) {
    return new Node(name, length, none, none)
  }

  static of(name: Term | null, children: Node[], positions: number[]) {
    let length = positions[positions.length - 1] + children[children.length - 1].length
    if (children.some(ch => !ch.name)) {
      let flatChildren = [], flatPositions = []
      for (let i = 0; i < children.length; i++) {
        let ch = children[i], pos = positions[i]
        if (ch.name) {
          flatChildren.push(ch)
          flatPositions.push(pos)
        } else {
          for (let j = 0; j < ch.children.length; j++) {
            flatChildren.push(ch.children[j])
            flatPositions.push(pos + ch.positions[j])
          }
        }
      }
      children = flatChildren; positions = flatPositions
    }
    return new Node(name, length, children, positions)
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

function parse(input: string[], grammar: Grammar, table: State[], cache = Node.leaf(null, 0)): Node {
  let parses = [new Frame(null, null, table[0], 0, 0)]
  let done = null, maxPos = 0
  let cacheIter = new TreeCursor(cache)
  parse: for (; !done;) {
    if (parses.length == 0) throw new Error("NO PARSE @ " + maxPos)
    console.log("stack is " + parses.join(" || "))
    let stack = takeFromHeap(parses, compareFrames), pos = stack.pos
    let next = grammar.terms.get(pos < input.length ? input[pos] : "#")
    console.log("token is", next.name, "@", pos)
    if (!stack.state.ambiguous) {
      for (let cached = cacheIter.nodeAt(pos); cached;
           cached = cached.children.length && cached.positions[0] == 0 ? cached.children[0] : null) {
        let match = stack.state.getGoto(cached.name!)
        if (match) {
          addFrame(parses, new Frame(stack, cached, match.target, pos /* FIXME */, pos + cached.length))
          maxPos = Math.max(maxPos, pos + cached.length)
          console.log("REUSE " + cached, "@", pos, "-", pos + cached.length)
          continue parse
        }
      }
    }

    stack.state.forEachAction(next, action => {
      if (action instanceof Goto) {
        maxPos = Math.max(maxPos, pos + 1)
        addFrame(parses, new Frame(stack, Node.leaf(next, 1), action.target, pos, pos + 1))
      } else if (action instanceof Reduce) {
        let newStack = stack, children = [], positions = []
        for (let i = action.rule.parts.length; i > 0; i--) {
          children.unshift(newStack.value!)
          positions.unshift(newStack.start)
          newStack = newStack.prev!
        }
        let newState = newStack.state.getGoto(action.rule.name)!.target, frame
        if (children.length) {
          let start = positions[0]
          for (let i = 0; i < positions.length; i++) positions[i] -= start
          frame = new Frame(newStack, Node.of(action.rule.name, children, positions), newState, start, pos)
        } else {
          frame = new Frame(newStack, Node.leaf(null, 0), newState, pos, pos)
        }
        addFrame(parses, frame)
      } else { // Accept
        console.log("Success: " + stack.value)
        done = stack.value
      }
    })
  }
  return done
}

const g = new Grammar([
  "S -> A #",
  "A -> A + M",
  "A -> A - M",
  "A -> M",
  "M -> M * V",
  "M -> M / V",
  "M -> V",
  "V -> x",
  "V -> y",
  "V -> ( A )"
])

let table = g.table()
console.log(table.join("\n"))

let input = ["x", "*", "y", "+", "(", "y", "/", "x", ")"]
let ast = parse(input, g, table)

console.log("--------------")

let cache = new Node(null, 0, [], [])
ast.partial(0, 2, 0, cache)
ast.partial(4, input.length, 0, cache)
let newInput = input.slice()
newInput[3] = "*"
parse(newInput, g, table, cache)
