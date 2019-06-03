 import {buildParser} from ".."
import {Parser, StringStream, Tree, SyntaxTree} from "lezer"
const ist = require("ist")

function p(text: string): () => Parser {
  let value: Parser | null = null
  return () => value || (value = buildParser(text))
}

function shared(a: Tree, b: Tree) {
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

function change(tree: Tree, ...changes: ([number, number] | [number, number, number, number])[]) {
  return tree.unchanged(changes.map(([fromA, toA, fromB = fromA, toB = toA]) => ({fromA, toA, fromB, toB})))
}

describe("parsing", () => {
  let p1 = p(`
    precedence { call }

    program { statement* }
    statement { Conditional | Loop | Block | expression p<";"> }
    Conditional { kw<"if"> expression statement }
    Block { p<"{"> statement* p<"}"> }
    Loop { kw<"while"> expression statement }
    expression { CallExpression | Number | Variable | p<"!"> expression }
    CallExpression { expression !call p<"("> expression* p<")"> }

    kw<value> { specialize<Variable, value> }
    tokens {
      p<x> { x }
      Number { std.digit+ }
      Variable { std.asciiLetter+ }
      whitespace { std.whitespace+ }
    }
    skip { whitespace }`)

  function qq(parser: Parser, ast: Tree) {
    return function(tag: string, offset = 1): {start: number, end: number} {
      let result = null
      ast.iterate(0, ast.length, (term, start, end) => {
        if (parser.getTag(term) == tag && --offset == 0) result = {start, end}
      })
      if (result) return result
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

  it("can resolve positions", () => {
    let doc = "while 111 { one; two(three 20); }", parser = p1()
    for (let i = 0; i < 2; i++) {
      let ast = parser.parse(new StringStream(doc), {strict: true, bufferLength: i ? 2 : 1024})

      let cx111 = ast.resolve(7)
      ist(cx111.depth, 2)
      ist(parser.getTag(cx111.type), "Number")
      ist(cx111.start, 6)
      ist(cx111.end, 9)
      ist(parser.getTag(cx111.parent!.type), "Loop")
      ist(cx111.parent!.start, 0)
      ist(cx111.parent!.end, 33)

      let cxThree = ast.resolve(22)
      ist(cxThree.depth, 4)
      ist(parser.getTag(cxThree.type), "Variable")
      ist(cxThree.start, 21)
      ist(cxThree.end, 26)

      let cxCall = cxThree.parent!
      ist(parser.getTag(cxCall.type), "CallExpression")
      ist(cxCall.start, 17)
      ist(cxCall.end, 30)
      ist(cxCall.children.map(c => parser.getTag(c.type)).join(","), "Variable,Variable,Number")

      let branch = cxThree.resolve(18)
      ist(branch.depth, 4)
      ist(parser.getTag(branch.type), "Variable")
      ist(branch.start, 17)
      ist(branch.end, 20)

      // Always resolve to the uppermost context for a position
      ist(ast.resolve(6).depth, 1)
      ist(ast.resolve(9).depth, 1)

      ist(cxCall.childBefore(cxCall.start), null)
      ist(cxCall.childAfter(cxCall.end), null)
      ist(parser.getTag(cxCall.childBefore(27)!.type), "Variable")
      ist(parser.getTag(cxCall.childAfter(26)!.type), "Number")
      ist(parser.getTag(cxCall.childBefore(28)!.type), "Number")
      ist(parser.getTag(cxCall.childAfter(28)!.type), "Number")
    }
  })
})

describe("sequences", () => {
  let p1 = p(`
    program { (X | Y)+ }
    X { t<"x"> }
    Y { t<"y"> t<";">* }
    tokens { t<x> { x } }`)

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
    ist(b, 10, "<=")
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
    let parser = p1()
    let ast = parser.parse(new StringStream(doc), {bufferLength: 10})
    let i = 0
    ast.iterate(0, ast.length, (term, start, end) => {
      let tag = parser.getTag(term)
      if (i == 100) {
        ist(tag, "Y")
        ist(start, 100)
        ist(end, 110)
      } else {
        ist(tag, "X")
        ist(end, start + 1)
        ist(start, i < 100 ? i : i + 9)
      }
      i++
    })
  })
})
