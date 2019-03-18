import {Grammar} from "./grammar/grammar"
import {buildAutomaton} from "./grammar/automaton"
import {parse} from "./parse"

function test(grammar: string, input: string[]) {
  const g = new Grammar(grammar)
  let table = buildAutomaton(g)
  return parse(input, g, table)
}

test(`
S { Atom | Product | Sum }
Product left { S ("*" | "/") S }
Sum left { S ("+" | "-") S }
Atom { "x" | "y" | "(" S ")" }
`, ["x", "+", "y", "*", "x"])
