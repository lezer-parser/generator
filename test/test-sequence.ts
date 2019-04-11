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

function shared(a: SyntaxTree, b: SyntaxTree) {
  let inA = [], shared = 0
  ;(function register(t: SyntaxTree) {
    inA.push(t)
    if (t instanceof Tree) t.children.forEach(register)
  })(a)
  ;(function scan(t: SyntaxTree) {
    if (inA.includes(t)) shared += t.length
    else if (t instanceof Tree) t.children.forEach(scan)
  })(b)
  return Math.round(100 * shared / b.length)
}

function change(tree: SyntaxTree, ...changes: ([number, number] | [number, number, number, number])[]) {
  return tree.unchanged(changes.map(([fromA, toA, fromB = fromA, toB = toA]) => ({fromA, toA, fromB, toB})))
}

describe("sequence parsing", () => {
  it("balances parsed sequences", () => {
    let ast = parse("x".repeat(1000), g1(), {strict: true, bufferLength: 10})
    let d = depth(ast), b = breadth(ast)
    ist(d, 6, "<=")
    ist(d, 4, ">=")
    ist(b, 5, ">=")
    ist(b, 10, "<")
  })

  it("caches parts of sequences", () => {
    let doc = "x".repeat(1000), grammar = g1()
    let ast = parse(doc, grammar, {bufferLength: 10})
    let full = parse(doc, grammar, {cache: ast, bufferLength: 10})
    ist(shared(ast, full), 99, ">")
    let front = parse(doc, grammar, {cache: change(ast, [900, 1000]), bufferLength: 10})
    ist(shared(ast, front), 50, ">")
    let back = parse(doc, grammar, {cache: change(ast, [0, 100]), bufferLength: 10})
    ist(shared(ast, back), 50, ">")
    let middle = parse(doc, grammar, {cache: change(ast, [0, 100], [900, 1000]), bufferLength: 10})
    ist(shared(ast, middle), 50, ">")
    let sides = parse(doc, grammar, {cache: change(ast, [450, 550]), bufferLength: 10})
    ist(shared(ast, sides), 50, ">")
  })
})
