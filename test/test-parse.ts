import {buildParser} from ".."
import {Parser, StringStream, SyntaxTree, Tree} from "lezer"
const ist = require("ist")

function p(text: string): () => Parser {
  let value: Parser | null = null
  return () => value || (value = buildParser(text))
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
  let p1 = p(`
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

  function qq(parser: Parser, ast: SyntaxTree) {
    return function(tag: string, offset = 1): {start: number, end: number} {
      for (let cur = ast.cursor(parser); cur.next();) if (cur.tag == tag) {
        if (--offset == 0) return cur
      }
      throw new Error("Couldn't find " + tag)
    }
  }

  it("can parse incrementally", () => {
    let doc = "if true { print(1); hello; } while false { if 1 do(something 1 2 3); }"
    let ast = p1().parse(new StringStream(doc), {bufferLength: 2})
    let expected = "Conditional(Variable,Block(CallExpression(Variable,Number),Variable))," +
      "Loop(Variable,Block(Conditional(Number,CallExpression(Variable,Variable,Number,Number,Number))))"
    ist(ast.toString(p1()), expected)
    ist(ast.length, 70)
    let pos = doc.indexOf("false"), doc2 = doc.slice(0, pos) + "x" + doc.slice(pos + 5)
    let ast2 = p1().parse(new StringStream(doc2), {bufferLength: 2, cache: change(ast, [pos, pos + 5, pos, pos + 1])})
    ist(ast2.toString(p1()), expected)
    ist(shared(ast, ast2), 75, ">")
    ist(ast2.length, 66)
  })

  it("assigns the correct node positions", () => {
    let doc = "if 1 { while 2 { foo(bar(baz bug)); } }"
    let ast = p1().parse(new StringStream(doc), {bufferLength: 10, strict: true})
    let q = qq(p1(), ast)
    ist(ast.length, 39)
    let cond = q("Conditional"), one = q("Number")
    ist(cond.start, 0); ist(cond.end, 39)
    ist(one.start, 3); ist(one.end, 4)
    let loop = q("Loop"), two = q("Number", 2)
    ist(loop.start, 7); ist(loop.end, 37)
    ist(two.start, 13); ist(two.end, 14)
    let call = q("CallExpression"), inner = q("CallExpression", 2)
    ist(call.start, 17); ist(call.end, 34)
    ist(inner.start, 21); ist(inner.end, 33)
    let bar = q("Variable", 2), bug = q("Variable", 4)
    ist(bar.start, 21); ist(bar.end, 24)
    ist(bug.start, 29); ist(bug.end, 32)
  })
})

describe("sequences", () => {
  let p1 = p(`
    program { (X | Y)+ }
    X { "x" }
    Y { "y" ";"* }`)

  function depth(tree: SyntaxTree): number {
    return tree instanceof Tree ? tree.children.reduce((d, c) => Math.max(d, depth(c) + 1), 1) : 1
  }

  function breadth(tree: SyntaxTree): number {
    return tree instanceof Tree ? tree.children.reduce((b, c) => Math.max(b, breadth(c)), tree.children.length) : 0
  }

  it("balances parsed sequences", () => {
    let ast = p1().parse(new StringStream("x".repeat(1000)), {strict: true, bufferLength: 10})
    let d = depth(ast), b = breadth(ast)
    ist(d, 6, "<=")
    ist(d, 4, ">=")
    ist(b, 5, ">=")
    ist(b, 10, "<")
  })

  it("caches parts of sequences", () => {
    let doc = "x".repeat(1000), str = new StringStream(doc), p = p1()
    let ast = p.parse(str, {bufferLength: 10})
    let full = p.parse(str, {cache: ast, bufferLength: 10})
    ist(shared(ast, full), 99, ">")
    let front = p.parse(str, {cache: change(ast, [900, 1000]), bufferLength: 10})
    ist(shared(ast, front), 50, ">")
    let back = p.parse(str, {cache: change(ast, [0, 100]), bufferLength: 10})
    ist(shared(ast, back), 50, ">")
    let middle = p.parse(str, {cache: change(ast, [0, 100], [900, 1000]), bufferLength: 10})
    ist(shared(ast, middle), 50, ">")
    let sides = p.parse(str, {cache: change(ast, [450, 550]), bufferLength: 10})
    ist(shared(ast, sides), 50, ">")
  })

  it("assigns the right positions to sequences", () => {
    let doc = "x".repeat(100) + "y;;;;;;;;;" + "x".repeat(90)
    let ast = p1().parse(new StringStream(doc), {bufferLength: 10})
    for (let cursor = ast.cursor(p1()), i = 0; cursor.next(); i++) {
      if (i == 100) {
        ist(cursor.tag, "Y")
        ist(cursor.start, 100)
        ist(cursor.end, 110)
      } else {
        ist(cursor.tag, "X")
        ist(cursor.end, cursor.start + 1)
        ist(cursor.start, i < 100 ? i : i + 9)
      }
    }
  })
})
