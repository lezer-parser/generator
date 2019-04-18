import {Term} from "./grammar"

export const MAX_CHAR = 0xffff

export class Edge {
  public target!: State
  constructor(readonly from: number, readonly to: number = from + 1, target?: State) {
    if (target) this.target = target
  }

  toString() {
    return `-> ${this.target.id}[label=${JSON.stringify(
      this.from < 0 ? "ε" : charFor(this.from) +
        (this.to > this.from + 1 ? "-" + charFor(this.to - 1) : ""))}]`
  }
}

function charFor(n: number) {
  return n > MAX_CHAR ? "∞" : n >= 0xd800 && n < 0xdfff ? "\\u{" + n.toString(16) + "}" : String.fromCharCode(n)
}

const MAX_SOURCE_BRANCH = 8

let stateID = 1

export class State {
  edges: Edge[] = []

  constructor(readonly accepting: Term | null = null, readonly id = stateID++) {}

  connect(edges: Edge[]) {
    for (let e of edges) {
      if (e.target) throw new Error("Trying to connect edge twice")
      e.target = this
    }
  }

  edge(from: number, to: number = from + 1, target?: State) {
    let e = new Edge(from, to, target)
    this.edges.push(e)
    return e
  }

  nullEdge(target?: State) { return this.edge(-1, -1, target) }

  compile() {
    let labeled: {[id: string]: State} = Object.create(null), localID = 0
    return explore(this.closure().sort((a, b) => a.id - b.id))

    function explore(states: State[]) {
      let newState = labeled[ids(states)] = new State(states.reduce((a: Term | null, s: State) => {
        if (!s.accepting) return a
        if (a && a != s.accepting)
          throw new SyntaxError(`Overlapping tokens ${a.name} and ${s.accepting.name}`)
        return s.accepting
      }, null), localID++)
      let out: Edge[] = []
      for (let state of states) for (let edge of state.edges) {
        if (edge.from >= 0) out.push(edge)
      }
      let transitions = mergeEdges(out)
      for (let merged of transitions) {
        let targets = merged.targets.sort((a, b) => a.id - b.id)
        newState.edge(merged.from, merged.to, labeled[ids(targets)] || explore(targets))
      }
      return newState
    }
  }

  closure() {
    let result: State[] = []
    function explore(state: State): void {
      if (result.includes(state)) return
      // States with only one outgoing epsilon edge and no accepting
      // term that isn't also in the next state are left out to help
      // reduce the number of unique state combinations
      if (state.edges.length == 1 && state.edges[0].from < 0 &&
          !(state.accepting && state.edges[0].target.accepting != state.accepting))
        return explore(state.edges[0].target)
      result.push(state)
      for (let edge of state.edges) if (edge.from < 0) explore(edge.target)
    }
    explore(this)
    return result
  }

  toString() {
    return `digraph {\n${this.toGraphViz([this])}}`
  }

  toGraphViz(seen: State[]) {
    let out = ""
    if (this.accepting)
      out += `  ${this.id} [label=${this.accepting.name}];\n`
    for (let edge of this.edges)
      out += `  ${this.id} ${edge};\n`
    for (let edge of this.edges) {
      if (!seen.includes(edge.target)) {
        seen.push(edge.target)
        out += edge.target.toGraphViz(seen)
      }
    }
    return out
  }

  toSource() {
    let enter = Object.create(null)
    let head = `function (input) {\n  let result = -2, state = 0, next\n  for (;;) {\n   next = input.next()\n    `
    let tail = `if (result > -2) return result\n    input.adv()\n  }\n}`
    let states: string[] = [], nextID = 0
    function explore(state: State): string {
      let known = enter[state.id]
      if (known != null) return known
      if (state.edges.length == 0 && state.accepting) return `input.adv(result = ${state.accepting.id})`
      let id = nextID++
      let here = enter[state.id] = `state = ${id}`
      states[id] = state.toLocalSource(explore, here)
      return here
    }
    explore(this)
    // FIXME profile whether this is worthwhile and whether another technique (switch, functions) is faster
    function flattenStates(from: number, to: number): string {
      if (to - from > MAX_SOURCE_BRANCH) {
        let mid = (to + from) >> 1
        return `state < ${mid}\n    ? ${flattenStates(from, mid)}: ${flattenStates(mid, to)}`
      } else {
        let text = ""
        for (let i = from; i < to - 1; i++) text += `state == ${i} ? ${states[i]}\n    : `
        text += `${states[to - 1]}\n    `
        return text
      }
    }
    return head + flattenStates(0, states.length) + tail
  }

  toLocalSource(explore: (state: State) => string, here: string) {
    let edgesToSource = (start: number, end: number, low: number, hi: number): string => {
      if (end == start + 1 && this.edges[start].from == low && this.edges[start].to >= hi)
        return explore(this.edges[start].target)

      if (end > start + MAX_SOURCE_BRANCH) {
        let mid = (end + start) >> 1, midChar = this.edges[mid].from
        return `next < ${midChar} ? ${edgesToSource(start, mid, low, midChar)} : ${edgesToSource(mid, end, midChar, hi)}`
      }

      let tests = [], actions = []
      for (let  i = start; i < end; i++) {
        let edge = this.edges[i]
        if (edge.to == edge.from + 1) tests.push(`next == ${edge.from}`)
        else if (edge.to >= hi) tests.push(`next > ${edge.from - 1}`)
        else if (edge.from == low) tests.push(`next < ${edge.to}`)
        else tests.push(`next > ${edge.from - 1} && next < ${edge.to}`)
        actions.push(explore(edge.target))
      }
      let fallThrough = `result = ${this.accepting ? this.accepting.id : -1}`
      let text = ""
      for (let i = 0; i < tests.length; i++) {
        let test = tests[i], action = actions[i]
        while (i < actions.length - 1 && actions[i + 1] == action)
          test += " || " + tests[++i]
        if (action == here) action = "0"
        text += `${test} ? ${action} : `
      }
      text += fallThrough
      return text
    }
    return edgesToSource(0, this.edges.length, -1, MAX_CHAR)
  }

  toFunction() {
    return (1,eval)("(" + this.toSource() + ")")
  }
}

function ids(states: State[]) {
  let result = ""
  for (let state of states) result += (result.length ? "-" : "") + state.id
  return result
}

class MergedEdge {
  constructor(readonly from: number, readonly to: number, readonly targets: State[]) {}
}

// Merge multiple edges (tagged by character ranges) into a set of
// mutually exclusive ranges pointing at all target states for that
// range
function mergeEdges(edges: Edge[]): MergedEdge[] {
  let separate: number[] = [], result: MergedEdge[] = []
  for (let edge of edges) {
    if (!separate.includes(edge.from)) separate.push(edge.from)
    if (!separate.includes(edge.to)) separate.push(edge.to)
  }
  separate.sort((a, b) => a - b)
  for (let i = 1; i < separate.length; i++) {
    let from = separate[i - 1], to = separate[i]
    let found: State[] = []
    for (let edge of edges) if (edge.to > from && edge.from < to) {
      for (let target of edge.target.closure()) if (!found.includes(target))
        found.push(target)
    }
    if (found.length) result.push(new MergedEdge(from, to, found))
  }
  return result
}
