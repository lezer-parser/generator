import {buildGrammar} from "./grammar/build"
import {buildAutomaton} from "./grammar/automaton"
import {parse} from "./parse"

function test(grammarText: string, input: string) {
  let {grammar, tokens} = buildGrammar(grammarText)
  let table = buildAutomaton(grammar)
  return parse(input, grammar, tokens, table)
}

test(`
prec left binOp { mult, plus }

program { expr }
expr {
  atom |
  binOp.mult<BinaryExpr<MultOp>> |
  binOp.plus<BinaryExpr<AddOp>>
}
BinaryExpr<op> { expr op expr }
atom { Symbol | "(" expr ")" }

tokens {
  MultOp { "*" | "/" }
  AddOp { "+" | "-" }
  Symbol { "x" | "y" }
}
`, "(x+y)/x")

// LR-but-not-LALR
/*
test(`
program { "a" E "a" | "b" E "b" | "a" F "b" | "b" F "a" }
E { "e" }
F { "e" }`, ["a", "e", "b"])
*/
