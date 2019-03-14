class Term {
  constructor(name) {
    this.name = name
    this.terminal = !/^[A-Z]/.test(name)
  }
  toString() { return this.name }
  cmp(other) { return this == other ? 0 : this.name < other.name ? -1 : 1 }
}

class TermSet {
  constructor() { this.terms = [] }
  get(name) {
    let found = this.terms.find(t => t.name == name)
    if (!found) this.terms.push(found = new Term(name))
    return found
  }
}

class Rule {
  constructor(str, terms) {
    let [_, name, expr] = /(\w+)\s*->\s*(.*)/.exec(str)
    this.name = terms.get(name)
    this.parts = expr.split(/\s+/).filter(x => x).map(n => terms.get(n))
  }

  cmp(rule) {
    return this.name.cmp(rule.name) ||
      this.parts.length - rule.parts.length ||
      this.parts.reduce((r, s, i) => r || s.cmp(rule.parts[i]), 0)
  }

  toString() {
    return this.name + "->" + this.parts.join("")
  }
}

class Grammar {
  constructor(rules) {
    this.terms = new TermSet
    this.rules = rules.map(rule => new Rule(rule, this.terms))
    this.nonTerminals = this.terms.terms.filter(t => !t.terminal)
    this.terminals = this.terms.terms.filter(t => t.terminal)
    this.first = computeFirst(this.rules, this.nonTerminals)
    this.follows = computeFollows(this.rules, this.nonTerminals, this.first)
  }

  closure(set) {
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
    let states = [], grammar = this
    function explore(set) {
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
            for (let follow of grammar.follows[pos.rule.name])
              state.addAction(new Reduce(follow, pos.rule), pos)
          } else if (next.name == "#") { // FIXME robust EOF representation
            state.addAction(new Accept(next), pos)
          }
        }
      }
      return state
    }

    explore(grammar.rules.filter(rule => rule.name == "S").map(rule => new Pos(rule, 0)))
    return states
  }
}

function add(value, array) {
  if (!array.includes(value)) array.push(value)
}

function computeFirst(rules, nonTerminals) {
  let table = {}
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

function computeFollows(rules, nonTerminals, first) {
  let table = {}
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
  constructor(rule, pos) {
    this.rule = rule; this.pos = pos
  }

  get next() {
    return this.pos < this.rule.parts.length ? this.rule.parts[this.pos] : null
  }

  advance() {
    return new Pos(this.rule, this.pos + 1)
  }

  cmp(pos) {
    return this.rule.cmp(pos.rule) ||
      this.pos - pos.pos
  }

  toString() {
    let parts = this.rule.parts.slice()
    parts.splice(this.pos, 0, "·")
    return this.rule.name + "->" + parts.join("")
  }
}

function advance(set, expr) {
  let result = []
  for (let pos of set) if (pos.next == expr)
    result.push(pos.advance())
  return result
}

function sameSet(a, b) {
  if (a.length != b.length) return false
  for (let i = 0; i < a.length; i++) if (a[i].cmp(b[i]) != 0) return false
  return true
}

class Goto {
  constructor(term, target) {
    this.term = term
    this.target = target
  }

  eq(other) { return other instanceof Goto && other.target == this.target }

  toString() { return "goto " + this.target.id }
}

class Accept {
  constructor(term) { this.term = term }

  eq(other) { return other instanceof Accept }

  toString() { return "accept" }
}

class Reduce {
  constructor(term, rule) {
    this.term = term
    this.rule = rule
  }

  eq(other) { return other instanceof Reduce && other.rule == this.rule }

  toString() { return "reduce " + this.rule }
}

class State {
  constructor(id, set) {
    this.id = id
    this.set = set
    this.terminals = []
    this.goto = []
  }

  toString() {
    return this.id + "=" + this.set.join() +  ": " +
      this.terminals.map(t => t.term + "=" + t).join(",") + "|" + this.goto.map(g => g.term + "=" + g).join(",")
  }

  addAction(value, _pos) {
    // FIXME only allow duplicates when choices are explicitly marked
    // as ambiguous
    for (let action of this.terminals) {
      if (action.term == value.term) {
        if (action.eq(value)) return
        console.log("duplicate rule added in " + this.id + " for", value.term + " " + value + " / " + action)
        // throw new Error("Conflict at " + pos + ": " + action + " vs " + value)
      }
    }
    this.terminals.push(value)
  }

  forEachAction(term, f) {
    for (let a of this.terminals) if (a.term == term) f(a)
  }

  getGoto(term) {
    return this.goto.find(a => a.term == term)
  }
}

class Frame {
  constructor(prev, value, state, start, pos) {
    this.prev = prev
    this.value = value
    this.state = state
    this.start = start
    this.pos = pos
  }

  toString() {
    return this.prev ? `${this.prev} ${this.value} [${this.state.id}]` : `[${this.state.id}]`
  }

  eq(other) {
    return this.state == other.state && this.pos == other.pos &&
      (this.prev == other.prev || this.prev && other.prev && this.prev.eq(other.prev))
  }
}

const {takeFromHeap, addToHeap} = require("./heap")

function compareFrames(a, b) { return a.pos - b.pos }

function addFrame(heap, frame) {
  if (!heap.some(f => f.eq(frame))) addToHeap(heap, frame, compareFrames)
}

const none = []

class Node {
  constructor(name, length, children, positions) {
    this.name = name
    this.length = length
    this.children = children
    this.positions = positions
  }

  toString() { return this.name ? (this.children.length ? this.name + "(" + this.children + ")" : this.name.name) : this.children.join() }

  static leaf(name, length) {
    return new Node(name, length, none, none)
  }

  static of(name, children, positions) {
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
}

class TreeCursor {
  constructor(node) {
    this.nodes = [node]
    this.start = [0]
    this.index = [0]
  }

  // `pos` must be >= any previously given `pos` for this cursor
  find(pos, type) {
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
      if (start > pos) return null
      if (start == pos) for (;;) {
        if (next.type == type) return next
        if (next.children.length == 0 || next.position[0] > 0) return null
        next = next.children[0]
      }
      this.index[last]++
      if (start + next.length >= pos) { // Enter this node
        this.nodes.push(next)
        this.start.push(start)
        this.index.push(0)
      }
    }
  }
}

function parse(input, grammar, table) {
  let parses = [new Frame(null, null, table[0], 0, 0)]
  let done = false, maxPos = 0
  for (; !done;) {
    if (parses.length == 0) throw new Error("NO PARSE @ " + maxPos)
    console.log("stack is " + parses.join(" || "))
    let stack = takeFromHeap(parses, compareFrames), pos = stack.pos
    let next = grammar.terms.get(pos < input.length ? input[pos] : "#")
    console.log("token is", next.name, "@", pos)
    stack.state.forEachAction(next, action => {
      if (action instanceof Goto) {
        maxPos = Math.max(maxPos, pos + 1)
        addFrame(parses, new Frame(stack, Node.leaf(next, 1), action.target, pos, pos + 1))
      } else if (action instanceof Accept) {
        console.log("Success: " + stack.value)
        done = true
      } else { // A reduce
        let newStack = stack, children = [], positions = []
        for (let i = action.rule.parts.length; i > 0; i--) {
          children.unshift(newStack.value)
          positions.unshift(newStack.start)
          newStack = newStack.prev
        }
        let newState = newStack.state.getGoto(action.rule.name).target
        let frame = children.length ? new Frame(newStack, Node.of(action.rule.name, children, positions), newState, positions[0], pos)
            : new Frame(newStack, Node.leaf(null, 0), newState, pos, pos)
        addFrame(parses, frame)
      }
    })
  }
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

parse(["x", "+", "y", "*", "(", "y", "/", "x", ")"], g, table)
