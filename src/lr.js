class Rule {
  constructor(str) {
    let [_, name, expr] = /(\w+)\s*->\s*(.*)/.exec(str)
    this.name = name
    this.parts = expr.split(/\s+/)
  }

  cmp(rule) {
    return cmpStr(this.name, rule.name) ||
      this.parts.length - rule.parts.length ||
      this.parts.reduce((r, s, i) => r || cmpStr(s, rule.parts[i]), 0)
  }

  toString() {
    return this.name + "->" + this.parts.join("")
  }
}

function isTerm(name) { return !/^[A-Z]/.test(name) }

function uniq(names) {
  let result = []
  for (let name of names) if (!result.includes(name)) result.push(name)
  return result
}

class Grammar {
  constructor(rules) {
    this.rules = rules.map(rule => new Rule(rule))
    this.nonTerminals = uniq(this.rules.map(r => r.name))
    this.terminals = uniq(this.rules.reduce((set, rule) => set.concat(rule.parts.filter(p => isTerm(p))), []))
    this.first = this.computeFirst()
    this.follows = this.computeFollows()
  }

  computeFirst() {
    let table = {}, change = false
    for (let nt of this.nonTerminals) table[nt] = []
    function add(name, value) {
      let set = table[name]
      if (!set.includes(value)) { set.push(value); change = true }
    }
    for (;;) {
      change = false
      for (let rule of this.rules) {
        let found = false
        for (let part of rule.parts) {
          found = true
          if (isTerm(part)) {
            add(rule.name, part)
          } else {
            for (let t in table[part]) {
              if (t == null) found = false
              else add(rule.name, t)
            }
          }
          if (found) break
        }
        if (!found) add(rule.name, null)
      }
      if (!change) return table
    }
  }

  computeFollows() {
    let table = [], change = false
    for (let nt of this.nonTerminals) table[nt] = []
    function add(name, value) {
      let set = table[name]
      if (!set.includes(value)) { set.push(value); change = true }
    }
    for (;;) {
      change = false
      for (let rule of this.rules) {
        for (let i = 0; i < rule.parts.length; i++) {
          let part = rule.parts[i], toEnd = true
          if (isTerm(part)) continue
          for (let j = i + 1; j < rule.parts.length; j++) {
            let next = rule.parts[j]
            toEnd = false
            if (isTerm(next)) {
              add(part, next)
            } else {
              for (let first of this.first[next]) {
                if (first == null) toEnd = true
                else add(part, first)
              }
            }
            if (!toEnd) break
          }
          if (toEnd) for (let follow of table[rule.name]) add(part, follow)
        }
      }
      if (!change) return table
    }
  }

  closure(set) {
    let result = set.slice()
    for (let pos of result) {
      let next = pos.next
      if (!next || isTerm(next)) continue
      for (let rule of this.rules) if (rule.name == next) {
        if (!result.some(p => p.rule.cmp(rule) == 0 && p.pos == 0))
          result.push(new Pos(rule, 0))
      }
    }
    return result.sort((a, b) => a.cmp(b))
  }

  table() {
    let states = [], grammar = this
    function explore(set) {
      if (set.length == 0) return -1
      set = grammar.closure(set)
      let found = states.findIndex(s => sameSet(s.set, set))
      if (found < 0) {
        let state = new State(found = states.length, set)
        states.push(state)
        for (let i = 0; i < grammar.terminals.length; i++)
          state.terminals[i] = explore(advance(set, grammar.terminals[i]))
        for (let i = 0; i < grammar.nonTerminals.length; i++)
          state.goto[i] = explore(advance(set, grammar.nonTerminals[i]))
        for (let pos of set) {
          if (pos.next == "#") {
            state.terminals[grammar.terminals.indexOf("#")] = "a"
          } else if (pos.next == null) {
            for (let follow of grammar.follows[pos.rule.name]) {
              let index = grammar.terminals.indexOf(follow)
              let value = "r" + grammar.rules.indexOf(pos.rule), cur = state.terminals[index]
              if (cur != -1 && cur != value) throw new Error("Conflict at " + pos + ": " + cur + " vs " + value)
              state.terminals[index] = value
            }
          }
        }
      }
      return found
    }

    explore(grammar.rules.filter(rule => rule.name == "S").map(rule => new Pos(rule, 0)))
    return states
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

function cmpStr(a, b) {
  return a == b ? 0 : a < b ? -1 : 1
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
}

function parse(input, grammar, table) {
  let stack = [table[0]], pos = 0
  for (;;) {
    let next = pos < input.length ? input[pos] : "#"
    console.log("stack is", stack.map(e => e instanceof State ? table.indexOf(e) : e))
    console.log("token is", next)
    let state = stack[stack.length - 1]
    let action = state.terminals[grammar.terminals.indexOf(next)]
    if (action == -1) {
      throw new Error("Fail at " + pos)
    } else if (typeof action == "number") { // A shift
      stack.push(next, table[action])
      pos++
    } else if (action == "a") {
      break
    } else { // A reduce
      let rule = grammar.rules[+action.slice(1)] // Ewww
      stack.length -= rule.parts.length * 2 // Pop off consumed parts
      let newState = stack[stack.length - 1].goto[grammar.nonTerminals.indexOf(rule.name)]
      stack.push(rule.name, table[newState])
    }
  }  
  console.log("Success")
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
console.log(g.terminals)
console.log(g.follows)

parse(["0", "+", "1", "*", "(", "1", "/", "0", ")"], g, table)
