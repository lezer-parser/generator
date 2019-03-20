import {buildGrammar} from "./grammar/build"
import {buildAutomaton} from "./grammar/automaton"
import {parse} from "./parse"

function test(grammar: string, input: string[]) {
  const g = buildGrammar(grammar)
  console.log(g + "")
  let table = buildAutomaton(g)
  console.log(table.join("\n"))
  return parse(input, g, table)
}

test(`
prec left BinOp { mult, plus }

Program { Expr }
Expr {
  Atom |
  BinOp.mult<Expr ("*" | "/") Expr> |
  BinOp.plus<Expr ("+" | "-") Expr>
}
Atom { "x" | "y" | "(" Expr ")" }
`, ["x", "*", "y", "-", "x"])
