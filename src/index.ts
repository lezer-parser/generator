import {buildGrammar} from "./grammar/build"
import {buildAutomaton} from "./grammar/automaton"
import {parse} from "./parse"

function test(grammar: string, input: string[]) {
  const g = buildGrammar(grammar)
  let table = buildAutomaton(g)
  console.log(table.join("\n"))
  return parse(input, g, table)
}

if (0) test(`
prec left BinOp { mult, plus }

Program { Expr }
Expr {
  Atom |
  BinOp.mult<Expr ("*" | "/") Expr> |
  BinOp.plus<Expr ("+" | "-") Expr>
}
Parens<E> { "(" E ")" }
Atom { "x" | "y" | Parens<Expr> }
`, ["x", "+", "y", "/", "x"])

test(`
Program { "a" E "a" | "b" E "b" | "a" F "b" | "b" F "a" }
E { "e" }
F { "e" }`, ["a", "e", "b"])
