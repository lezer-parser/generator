import {buildGrammar} from "../src/grammar/build"
import {Grammar} from "../src/grammar/grammar"
import {parse, SyntaxTree, Node, Tree} from "../src/parse"
const ist = require("ist")

let _g1: Grammar | null = null
function g1() {
  if (!_g1) _g1 = buildGrammar(`
    program { (X | Y)+ }
    X { "x" }
    Y { "y" ";"* }`)
  return _g1
}

function depth(tree: SyntaxTree) {
  return tree instanceof Tree ? tree.children.reduce((d, c) => Math.max(d, depth(c) + 1), 1) : 1
}

function breadth(tree: SyntaxTree) {
  return tree instanceof Tree ? tree.children.reduce((b, c) => Math.max(b, breadth(c)), tree.children.length) : 0
}

describe("sequence parsing", () => {
  it("balances parsed sequences", () => {
    let ast = parse("x".repeat(100000), g1(), {strict: true})
    let d = depth(ast), b = breadth(ast)
    ist(d, 6, "<=")
    ist(d, 4, ">=")
    ist(b, 5, ">=")
    ist(b, 10, "<")
  })
})
