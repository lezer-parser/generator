import {buildGrammar} from "../src/grammar/build"
import {Grammar} from "../src/grammar/grammar"
import {parse, SyntaxTree, Node, Tree} from "../src/parse/parse"
const ist = require("ist")

function g(text: string): () => Grammar {
  let value: Grammar | null = null
  return () => value || (value = buildGrammar(text))
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

describe("parsing", () => {
  let g1 = g(`
    prec { call }

    program { statement* }
    statement { Conditional | Loop | Block | expression ";" }
    Conditional { kw<"if"> expression statement }
    Block { "{" statement* "}" }
    Loop { kw<"while"> expression statement }
    expression { CallExpression | Number | Variable | "!" expression }
    CallExpression { expression !call "(" expression* ")" }

    kw<value> { specialize<Variable, value> }
    tokens {
      Number { std.digit+ }
      Variable { std.asciiLetter+ }
      skip { std.whitespace* }
    }`)

  function q(ast: SyntaxTree, tag: string, offset = 1): {start: number, end: number} {
    for (let cur = ast.cursor; cur.next();) if (cur.tag.tag == tag) {
      if (--offset == 0) return cur
    }
  }

  it("can parse incrementally", () => {
    let doc = "if true { print(1); hello; } while false { if 1 do(something 1 2 3); }"
    let ast = parse(doc, g1(), {bufferLength: 2})
    let expected = "Conditional(Variable,Block(CallExpression(Variable,Number),Variable))," +
      "Loop(Variable,Block(Conditional(Number,CallExpression(Variable,Variable,Number,Number,Number))))"
    ist(ast.toString(), expected)
    ist(ast.length, 70)
    let pos = doc.indexOf("false"), doc2 = doc.slice(0, pos) + "x" + doc.slice(pos + 5)
    let ast2 = parse(doc2, g1(), {bufferLength: 2, cache: change(ast, [pos, pos + 5, pos, pos + 1])})
    ist(ast2.toString(), expected)
    ist(shared(ast, ast2), 75, ">")
    ist(ast2.length, 66)
  })

  it("assigns the correct node positions", () => {
    let doc = "if 1 { while 2 { foo(bar(baz bug)); } }"
    let ast = parse(doc, g1(), {bufferLength: 10, strict: true})
    ist(ast.length, 39)
    let cond = q(ast, "Conditional"), one = q(ast, "Number")
    ist(cond.start, 0); ist(cond.end, 39)
    ist(one.start, 3); ist(one.end, 4)
    let loop = q(ast, "Loop"), two = q(ast, "Number", 2)
    ist(loop.start, 7); ist(loop.end, 37)
    ist(two.start, 13); ist(two.end, 14)
    let call = q(ast, "CallExpression"), inner = q(ast, "CallExpression", 2)
    ist(call.start, 17); ist(call.end, 34)
    ist(inner.start, 21); ist(inner.end, 33)
    let bar = q(ast, "Variable", 2), bug = q(ast, "Variable", 4)
    ist(bar.start, 21); ist(bar.end, 24)
    ist(bug.start, 29); ist(bug.end, 32)
  })
})

describe("sequences", () => {
  let g1 = g(`
    program { (X | Y)+ }
    X { "x" }
    Y { "y" ";"* }`)

  function depth(tree: SyntaxTree) {
    return tree instanceof Tree ? tree.children.reduce((d, c) => Math.max(d, depth(c) + 1), 1) : 1
  }

  function breadth(tree: SyntaxTree) {
    return tree instanceof Tree ? tree.children.reduce((b, c) => Math.max(b, breadth(c)), tree.children.length) : 0
  }

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

  it("assigns the right positions to sequences", () => {
    let doc = "x".repeat(100) + "y;;;;;;;;;" + "x".repeat(90)
    let ast = parse(doc, g1(), {bufferLength: 10})
    for (let cursor = ast.cursor, i = 0; cursor.next(); i++) {
      if (i == 100) {
        ist(cursor.tag.tag, "Y")
        ist(cursor.start, 100)
        ist(cursor.end, 110)
      } else {
        ist(cursor.tag.tag, "X")
        ist(cursor.end, cursor.start + 1)
        ist(cursor.start, i < 100 ? i : i + 9)
      }
    }
  })
})
