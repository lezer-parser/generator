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
    this.parts = expr.split(/\s+/).map(n => terms.get(n))
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

function computeFollows(rules, nonTerminals) {
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
            for (let first of first[next.name]) {
              if (first == null) toEnd = true
              else add(first, set)
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
      this.terminals.join(",") + "|" + this.goto.join(",")
  }

  addAction(value, pos) {
    // FIXME only allow duplicates when choices are explicitly marked
    // as ambiguous
    if (0) for (let action of this.terminals) {
      if (action.term == value.term) {
        if (action.eq(value)) return
        throw new Error("Conflict at " + pos + ": " + action + " vs " + value)
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
  constructor(prev, value, state, pos) {
    this.prev = prev
    this.value = value
    this.state = state
    this.pos = pos
  }

  toString() {
    return this.prev ? this.prev + " " + this.value + " " + this.state.id : this.state.id
  }
}

const {takeFromHeap, addToHeap} = require("./heap")

function compareFrames(a, b) { return a.pos - b.pos }

function parse(input, grammar, table) {
  let parses = [new Frame(null, null, table[0], 0)]
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
        addToHeap(parses, new Frame(stack, next, action.target, pos + 1), compareFrames)
      } else if (action instanceof Accept) {
        console.log("Success")
        done = true
      } else { // A reduce
        for (let i = action.rule.parts.length; i > 0; i--) stack = stack.prev
        let newState = stack.state.getGoto(action.rule.name).target
        addToHeap(parses, new Frame(stack, action.rule.name, newState, pos), compareFrames)
      }
    })
  }
}

const g = new Grammar([
  "S -> A #",
  "A -> A + T",
  "A -> A - T",
  "A -> T",
  "T -> T * B",
  "T -> T / B",
  "T -> B",
  "B -> 0",
  "B -> 1",
  "B -> ( A )"
])

let table = g.table()
console.log(table.join("\n"))

parse(["0", "+", "1", "*", "(", "1", "/", "0", ")"], g, table)
