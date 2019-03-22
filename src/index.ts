import {buildGrammar} from "./grammar/build"
import {parse} from "./parse"

function test(grammarText: string, input: string) {
  let grammar = buildGrammar(grammarText)
  return parse(input, grammar)
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
