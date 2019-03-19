import {Grammar} from "./grammar/grammar"
import {buildAutomaton} from "./grammar/automaton"
import {parse} from "./parse"

function test(grammar: string, input: string[]) {
  const g = new Grammar(grammar)
  console.log(g + "")
  let table = buildAutomaton(g)
  console.log(table.join("\n"))
  return parse(input, g, table)
}

test(`
S left { Atom / S ("*" | "/") S / S ("+" | "-") S }
Atom { "x" | "y" | "(" S ")" }
`, ["x", "*", "y", "-", "x"])
