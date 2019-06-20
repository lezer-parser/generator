import {buildParser, BuildOptions} from ".."
import {Parser, InputStream, Stack, Tree, TagMap} from "lezer"
const ist = require("ist")

function p(text: string, options?: BuildOptions): () => Parser {
  let value: Parser | null = null
  return () => value || (value = buildParser(text, options))
}

function shared(a: Tree, b: Tree) {
  let inA = [], shared = 0
  ;(function register(t: any) {
    inA.push(t)
    if (t instanceof Tree) t.children.forEach(register)
  })(a)
  ;(function scan(t: any) {
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

    top { statement* }
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
        if (parser.tags.get(term) == tag && --offset == 0) result = {start, end}
      })
      if (result) return result
      throw new Error("Couldn't find " + tag)
    }
  }

  it("can parse incrementally", () => {
    let doc = "if true { print(1); hello; } while false { if 1 do(something 1 2 3); }"
    let ast = p1().parse(doc, {bufferLength: 2})
    let expected = "Conditional(Variable,Block(CallExpression(Variable,Number),Variable))," +
      "Loop(Variable,Block(Conditional(Number,CallExpression(Variable,Variable,Number,Number,Number))))"
    ist(ast.toString(p1().tags), expected)
    ist(ast.length, 70)
    let pos = doc.indexOf("false"), doc2 = doc.slice(0, pos) + "x" + doc.slice(pos + 5)
    let ast2 = p1().parse(doc2, {bufferLength: 2, cache: change(ast, [pos, pos + 5, pos, pos + 1])})
    ist(ast2.toString(p1().tags), expected)
    ist(shared(ast, ast2), 60, ">")
    ist(ast2.length, 66)
  })

  it("assigns the correct node positions", () => {
    let doc = "if 1 { while 2 { foo(bar(baz bug)); } }"
    let ast = p1().parse(doc, {bufferLength: 10, strict: true})
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

  let resolveDoc = "while 111 { one; two(three 20); }"

  function testResolve(bufferLength: number) {
    let parser = p1()
    let ast = parser.parse(resolveDoc, {strict: true, bufferLength})

    let cx111 = ast.resolve(7)
    ist(cx111.depth, 2)
    ist(parser.tags.get(cx111.type), "Number")
    ist(cx111.start, 6)
    ist(cx111.end, 9)
    ist(parser.tags.get(cx111.parent!.type), "Loop")
    ist(cx111.parent!.start, 0)
    ist(cx111.parent!.end, 33)

    let cxThree = ast.resolve(22)
    ist(cxThree.depth, 4)
    ist(parser.tags.get(cxThree.type), "Variable")
    ist(cxThree.start, 21)
    ist(cxThree.end, 26)

    let cxCall = cxThree.parent!
    ist(parser.tags.get(cxCall.type), "CallExpression")
    ist(cxCall.start, 17)
    ist(cxCall.end, 30)

    let branch = cxThree.resolve(18)
    ist(branch.depth, 4)
    ist(parser.tags.get(branch.type), "Variable")
    ist(branch.start, 17)
    ist(branch.end, 20)

    // Always resolve to the uppermost context for a position
    ist(ast.resolve(6).depth, 1)
    ist(ast.resolve(9).depth, 1)

    ist(cxCall.childBefore(cxCall.start), null)
    ist(cxCall.childAfter(cxCall.end), null)
    ist(parser.tags.get(cxCall.childBefore(27)!.type), "Variable")
    ist(parser.tags.get(cxCall.childAfter(26)!.type), "Number")
    ist(parser.tags.get(cxCall.childBefore(28)!.type), "Number")
    ist(parser.tags.get(cxCall.childAfter(28)!.type), "Number")
  }

  it("can resolve positions in buffers", () => testResolve(1024))

  it("can resolve positions in trees", () => testResolve(2))

  let iterDoc = "while 1 { a; b; c(d e); } while 2 { f; }"
  let iterSeq = ["Loop", 0, "Number", 6, "/Number", 7, "Block", 8, "Variable", 10, "/Variable", 11,
                 "Variable", 13, "/Variable", 14, "CallExpression", 16, "Variable", 16, "/Variable", 17,
                 "Variable", 18, "/Variable", 19, "Variable", 20, "/Variable", 21, "/CallExpression", 22,
                 "/Block", 25, "/Loop", 25, "Loop", 26, "Number", 32, "/Number", 33, "Block", 34, "Variable", 36,
                 "/Variable", 37, "/Block", 40, "/Loop", 40]
  // Node boundaries seen when iterating range 13-19 ("b; c(d")
  let partialSeq = ["Loop", 0, "Block", 8, "Variable", 13, "/Variable", 14, "CallExpression", 16, "Variable", 16,
                    "/Variable", 17, "Variable", 18, "/Variable", 19, "/CallExpression", 22, "/Block", 25, "/Loop", 25]

  function testIter(bufferLength: number, partial: boolean) {
    let parser = p1(), output: any[] = []
    let ast = parser.parse(iterDoc, {strict: true, bufferLength})
    ast.iterate(partial ? 13 : 0, partial ? 19 : ast.length,
                (open, start) => { output.push(parser.tags.get(open), start) },
                (close, _, end) => { output.push("/" + parser.tags.get(close), end) })
    ist(output.join(), (partial ? partialSeq : iterSeq).join())
  }

  it("supports forward iteration in buffers", () => testIter(1024, false))

  it("supports forward iteration in trees", () => testIter(2, false))

  it("supports partial forward iteration in buffers", () => testIter(1024, true))

  it("supports partial forward iteration in trees", () => testIter(2, true))

  function testIterRev(bufferLength: number, partial: boolean) {
    let parser = p1(), output: any[] = []
    let ast = parser.parse(iterDoc, {strict: true, bufferLength})
    ast.iterate(partial ? 19 : ast.length, partial ? 13 : 0,
                (close, _, end) => { output.push(end, "/" + parser.tags.get(close)) },
                (open, start) => { output.push(start, parser.tags.get(open)) })
    ist(output.reverse().join(), (partial ? partialSeq : iterSeq).join())
  }

  it("supports reverse iteration in buffers", () => testIterRev(1024, false))

  it("supports reverse iteration in trees", () => testIterRev(2, false))

  it("supports partial reverse iteration in buffers", () => testIterRev(1024, true))

  it("supports partial reverse iteration in trees", () => testIterRev(2, true))

  it("doesn't incorrectly reuse nodes", () => {
    let parser = buildParser(`
precedence { times left, plus left }
top { expr+ }
expr { BinOp | Var }
BinOp { expr !plus "+" expr | expr !times "*" expr }
skip { space }
tokens { space { " "+ } Var { "x" } }
`)
    let ast = parser.parse("x + x + x", {strict: true, bufferLength: 2})
    ist(ast.toString(parser.tags), 'BinOp(BinOp(Var,"+",Var),"+",Var)')
    let ast2 = parser.parse("x * x + x + x", {strict: true, bufferLength: 2, cache: change(ast, [0, 0, 0, 4])})
    ist(ast2.toString(parser.tags), 'BinOp(BinOp(BinOp(Var,"*",Var),"+",Var),"+",Var)')
  })

  it("can cache skipped content", () => {
    let comments = buildParser(`
top { "x"+ }
skip { space | Comment }
skip {} {
  Comment { commentStart (Comment | commentContent)* commentEnd }
}
tokens {
  space { " "+ }
  commentStart { "(" }
  commentEnd { ")" }
  commentContent { [^()]+ }
}`)
    let doc = "x  (one (two) (three " + "(y)".repeat(500) + ")) x"
    let ast = comments.parse(doc, {bufferLength: 10, strict: true})
    let ast2 = comments.parse(doc.slice(1), {cache: change(ast, [0, 1, 0, 0]), bufferLength: 10})
    ist(shared(ast, ast2), 80, ">")
  })
})

describe("sequences", () => {
  let p1 = p(`
    top { (X | Y)+ }
    X { t<"x"> }
    Y { t<"y"> t<";">* }
    tokens { t<x> { x } }`)

  function depth(tree: any): number {
    return tree instanceof Tree ? tree.children.reduce((d, c) => Math.max(d, depth(c) + 1), 1) : 1
  }

  function breadth(tree: any): number {
    return tree instanceof Tree ? tree.children.reduce((b, c) => Math.max(b, breadth(c)), tree.children.length) : 0
  }

  it("balances parsed sequences", () => {
    let ast = p1().parse("x".repeat(1000), {strict: true, bufferLength: 10})
    let d = depth(ast), b = breadth(ast)
    ist(d, 6, "<=")
    ist(d, 4, ">=")
    ist(b, 5, ">=")
    ist(b, 10, "<=")
  })

  it("caches parts of sequences", () => {
    let doc = "x".repeat(1000), p = p1()
    let ast = p.parse(doc, {bufferLength: 10})
    let full = p.parse(doc, {cache: ast, bufferLength: 10})
    ist(shared(ast, full), 99, ">")
    let front = p.parse(doc, {cache: change(ast, [900, 1000]), bufferLength: 10})
    ist(shared(ast, front), 50, ">")
    let back = p.parse(doc, {cache: change(ast, [0, 100]), bufferLength: 10})
    ist(shared(ast, back), 50, ">")
    let middle = p.parse(doc, {cache: change(ast, [0, 100], [900, 1000]), bufferLength: 10})
    ist(shared(ast, middle), 50, ">")
    let sides = p.parse(doc, {cache: change(ast, [450, 550]), bufferLength: 10})
    ist(shared(ast, sides), 50, ">")
  })

  it("assigns the right positions to sequences", () => {
    let doc = "x".repeat(100) + "y;;;;;;;;;" + "x".repeat(90)
    let parser = p1()
    let ast = parser.parse(doc, {bufferLength: 10})
    let i = 0
    ast.iterate(0, ast.length, (term, start, end) => {
      let tag = parser.tags.get(term)
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

describe("nesting", () => {
  it("can nest grammars", () => {
    let inner = buildParser(`
top { expr+ }
expr { tag.B<"(" expr+ ")"> | "." }`)
    let outer = buildParser(`
external grammar inner from "."
top { expr+ }
expr { "[[" nest.inner<Foo> "]]" | "!" }
`, {nestedGrammar() { return inner }})

    let tags = TagMap.combine([outer.tags, inner.tags])
    ist(outer.parse("![[((.).)]][[.]]").toString(tags),
        '"!","[[",Foo(B("(",B("(",".",")"),".",")")),"]]","[[",Foo("."),"]]"')
    ist(outer.parse("[[/\]]").toString(tags),
        '"[[",Foo(⚠),"]]"')
  })

  it("supports conditional nesting and end token predicates", () => {
    let inner = buildParser(`top { any } tokens { any { _+ } }`)
    let outer = buildParser(`
external grammar inner from "."
top { Tag }
Tag { Open nest.inner<Text, "</" name ">", Tag*> Close }
Open { h<"<"> name h<">"> }
Close { h<"</"> name h<">"> }
tokens {
  name { std.asciiLetter+ }
  h<str> { str }
}
`, {nestedGrammar() { return nest }})

    function nest(stream: InputStream, stack: Stack) {
      let tag = /<(\w+)>$/.exec(stream.read(stack.ruleStart, stream.pos))
      if (!tag || !["textarea", "script", "style"].includes(tag[1])) return {stay: true}
      return {
        parser: inner,
        filterEnd(token: string) { return token == "</" + tag![1] + ">" }
      }
    }

    let tags = TagMap.combine([outer.tags, inner.tags])
    ist(outer.parse("<foo><bar></baz></foo>").toString(tags),
        "Tag(Open,Tag(Open,Close),Close)")
    ist(outer.parse("<textarea><bar></baz></textarea>").toString(tags),
        "Tag(Open,Text,Close)")
  })

  it("allows updating the nested grammars for a parser", () => {
    let inner1 = buildParser(`top { tag.A<"x">+ }`)
    let inner2 = buildParser(`top { tag.B<"x">+ }`)
    let outer = buildParser(`external grammar inner from "." top { "[" nest.inner<N> "]" }`,
                            {nestedGrammar() { return inner1 }})
    let tags = TagMap.combine([outer.tags, inner1.tags, inner2.tags])
    ist(outer.parse("[x]").toString(tags), '"[",N(A("x")),"]"')
    ist(outer.withNested({inner: inner2}).parse("[x]").toString(tags), '"[",N(B("x")),"]"')
  })

  it("supports tag-less nesting", () => {
    let inner = buildParser(`top { "x" }`)
    let outer = buildParser(`external grammar x from "." top { "&" nest.x "&" }`, {nestedGrammar() { return inner }})
    ist(outer.parse("&x&").toString(TagMap.combine([inner.tags, outer.tags])), '"&","x","&"')
  })

  it("skips ranges with missing nested parsers", () => {
    let outer = buildParser(`external grammar inner top { "[" nest.inner<N> "]" }`)
    ist(outer.parse("[lfkdsajfa]").toString(outer.tags), '"[",N,"]"')
  })
})
