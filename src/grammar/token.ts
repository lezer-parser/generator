import {Term} from "./grammar"

export class Edge {
  public target!: State
  constructor(readonly from: number, readonly to: number = from + 1, target?: State) {
    if (target) this.target = target
  }
}

export class State {
  edges: Edge[] = []
  accepting: Term[] = []

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
}
