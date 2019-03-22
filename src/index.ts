import {buildGrammar} from "./grammar/build"
import {buildAutomaton} from "./grammar/automaton"
import {parse} from "./parse"

function test(grammar: string, input: string[]) {
  const g = buildGrammar(grammar)
  let table = buildAutomaton(g)
  return parse(input, g, table)
}

test(`
prec left binOp { mult, plus }

program { expr }
expr {
  Atom |
  binOp.mult<BinaryExpr<MultOp>> |
  binOp.plus<BinaryExpr<AddOp>>
}
BinaryExpr<op> { expr op expr }
Atom { Symbol | "(" expr ")" }

tokens {
  MultOp { "*" | "/" }
  AddOp { "+" | "-" }
  Symbol { "x" | "y" }
}
`, ["x", "+", "y", "/", "x"])

// LR-but-not-LALR
/*
test(`
program { "a" E "a" | "b" E "b" | "a" F "b" | "b" F "a" }
E { "e" }
F { "e" }`, ["a", "e", "b"])
*/
