import {Term} from "./grammar"

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
  return n == 2e8 ? "∞" : String.fromCodePoint(n)
}

let stateID = 1

export class Token {
  public start = 0
  public end = 0
  public term: Term = null as any
  public specialized: Term | null = null
}

export class State {
  edges: Edge[] = []
  // FIXME number per automaton?
  id = stateID++

  constructor(readonly accepting: Term | null = null) {}

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
    let labeled: {[id: string]: State} = Object.create(null)
    return explore(this.closure().sort((a, b) => a.id - b.id))

    function explore(states: State[]) {
      let newState = labeled[ids(states)] = new State(states.reduce((a: Term | null, s: State) => {
        if (!s.accepting) return a
        if (a && a != s.accepting)
          throw new SyntaxError(`Overlapping tokens ${a.name} and ${s.accepting.name}`)
        return s.accepting
      }, null))
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

  // Runs the state machine on input, returns the accepting term if it
  // found one, or null otherwise. Updates `target.end` to the end of
  // the token when returning successfull.
  simulate(input: string, pos: number, target: Token): Term | null {
    let state: State = this
    for (; pos < input.length;) {
      let next = input.codePointAt(pos)!
      let edge = state.edges.find(e => e.from <= next && e.to > next)
      if (!edge) break
      state = edge.target
      pos += next > 0xffff ? 2 : 1
    }
    if (state.accepting) { target.end = pos; return state.accepting }
    return null
  }
}

export class Tokenizer {
  constructor(readonly skip: State | null,
              readonly startState: State,
              readonly specialized: {[terminal: string]: {[value: string]: Term}}) {}

  toString() { return this.startState.toString() }

  // Tries to fill in a token and write it to `target`. `.start` is
  // always updated with the position after the whitespace. The other
  // fields are left as they are when no token is found.
  simulate(input: string, pos: number, target: Token): boolean {
    target.start = pos
    let found = this.startState.simulate(input, pos, target)
    if (!found) return false
    target.term = found
    let spec = this.specialized[found.name]
    target.specialized = spec && spec[input.slice(target.start, target.end)] || null
    return true
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
