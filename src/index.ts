import {Grammar} from "./grammar/grammar"
import {buildAutomaton} from "./grammar/automaton"
import {parse} from "./parse"

function test(grammar: string, input: string[]) {
  const g = new Grammar(grammar)
  console.log(g.rules.join("\n"))
  let table = buildAutomaton(g)
  return parse(input, g, table)
}

if (0) test(`
S { P (("+" | "-") P)* }
P { T (("*" | "/") T)* }
T { "x" | "y" | "(" S ")" }
`, ["x", "*", "y", "+", "(", "y", "/", "x", ")"])

test(`
S { "0" | S "+" S }
`, ["0", "+", "0", "+", "0"])
