import {Term} from "./grammar"

export class Edge {
  public target!: State
  constructor(readonly from: number, readonly to: number = from + 1, target?: State) {
    if (target) this.target = target
  }
}

let stateID = 1

export class State {
  edges: Edge[] = []
  accepting: Term[] = []
  id = stateID++

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
    return explore(this.closure())

    function explore(states: State[]) {
      // FIXME properly compare and split ranges. Optimize
      let out: {[key: number]: State[]} = {}
      let newState = new State
      for (let state of states) {
        for (let acc of state.accepting) if (!newState.accepting.includes(acc)) newState.accepting.push(acc)
        for (let edge of state.edges) {
          if (edge.from < 0) continue
          let set = out[edge.from] || (out[edge.from] = [])
          for (let state of edge.target.closure())
            if (!set.includes(state)) set.push(state)
        }
      }
      labeled[ids(states)] = newState
      for (let n in out) {
        let targets = out[n].sort((a, b) => a.id - b.id)
        newState.edge(+n, +n + 1, labeled[ids(targets)] || explore(targets))
      }
      return newState
    }
  }

  closure() {
    let result: State[] = [this]
    for (let edge of this.edges) if (edge.from < 0 && !result.includes(edge.target)) result.push(edge.target)
    return result
  }

  simulate(input: string, pos: number): {term: Term, end: number}[] {
    let result = []
    for (let state: State = this; pos < input.length;) {
      let next = input.codePointAt(pos)!
      pos += next > 0xffff ? 2 : 1
      let edge = state.edges.find(e => e.from <= next && e.to > next)
      if (!edge) break
      state = edge.target
      // FIXME try to avoid pushing duplicate tokens
      for (let acc of state.accepting) result.push({term: acc, end: pos})
    }
    return result
  }

  toString() {
    return `digraph {\n${this.toGraphViz([])}\n}`
  }

  toGraphViz(seen: State[]) {
    let out = ""
    if (this.accepting.length)
      out += `  ${this.id} [label=${this.accepting.map(t => t.name).join()}];\n`
    for (let edge of this.edges) {
      out += `  ${this.id} -> ${edge.target.id}[label=${JSON.stringify(String.fromCharCode(edge.from))}];\n`
      if (!seen.includes(edge.target)) {
        seen.push(edge.target)
        out += edge.target.toGraphViz(seen)
      }
    }
    return out
  }
}

function ids(states: State[]) {
  let result = ""
  for (let state of states) result += (result.length ? "-" : "") + state.id
  return result
}
