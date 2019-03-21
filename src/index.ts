import {buildGrammar} from "./grammar/build"
import {buildAutomaton} from "./grammar/automaton"
import {parse} from "./parse"

function test(grammar: string, input: string[]) {
  const g = buildGrammar(grammar)
  let table = buildAutomaton(g)
  console.log(table.join("\n"))
  return parse(input, g, table)
}

test(`
prec left binOp { mult, plus }

program { expr }
expr {
  Atom |
  BinOp<binOp.mult, "*" | "/"> |
  BinOp<binOp.plus, "+" | "-">
}
BinOp<prec, op> { prec<expr op expr> }
Atom { "x" | "y" | "(" expr ")" }
`, ["x", "+", "y", "/", "x"])

// LR-but-not-LALR
/*
test(`
program { "a" E "a" | "b" E "b" | "a" F "b" | "b" F "a" }
E { "e" }
F { "e" }`, ["a", "e", "b"])
*/
