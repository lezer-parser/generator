import {buildParser, BuildOptions} from ".."
import {Parser, InputStream, Stack, Tree} from "lezer"
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
  return tree.applyChanges(changes.map(([fromA, toA, fromB = fromA, toB = toA]) => ({fromA, toA, fromB, toB})))
}

describe("parsing", () => {
  let p1 = p(`
    precedence { call }

    top { statement* }
    statement { conditional | loop | block | expression p<";"> }
    conditional:cond { kw<"if"> expression statement }
    block:block { p<"{"> statement* p<"}"> }
    loop:loop { kw<"while"> expression statement }
    expression { callExpression | number | variable | p<"!"> expression }
    callExpression:call { expression !call p<"("> expression* p<")"> }

    kw<value> { specialize<variable, value> }
    tokens {
      p<x> { x }
      number:num { std.digit+ }
      variable:var { std.asciiLetter+ }
      whitespace { std.whitespace+ }
    }
    skip { whitespace }`)

  function qq(parser: Parser, ast: Tree) {
    return function(query: string, offset = 1): {start: number, end: number} {
      let result = null
      ast.iterate(0, ast.length, (tag, start, end) => {
        if (tag.tag == query && --offset == 0) result = {start, end}
      })
      if (result) return result
      throw new Error("Couldn't find " + query)
    }
  }

  it("can parse incrementally", () => {
    let doc = "if true { print(1); hello; } while false { if 1 do(something 1 2 3); }"
    let ast = p1().parse(doc, {bufferLength: 2})
    let expected = "cond(var,block(call(var,num),var))," +
      "loop(var,block(cond(num,call(var,var,num,num,num))))"
    ist(ast.toString(), expected)
    ist(ast.length, 70)
    let pos = doc.indexOf("false"), doc2 = doc.slice(0, pos) + "x" + doc.slice(pos + 5)
    let ast2 = p1().parse(doc2, {bufferLength: 2, cache: change(ast, [pos, pos + 5, pos, pos + 1])})
    ist(ast2.toString(), expected)
    ist(shared(ast, ast2), 60, ">")
    ist(ast2.length, 66)
  })

  it("assigns the correct node positions", () => {
    let doc = "if 1 { while 2 { foo(bar(baz bug)); } }"
    let ast = p1().parse(doc, {bufferLength: 10, strict: true})
    let q = qq(p1(), ast)
    ist(ast.length, 39)
    let cond = q("cond"), one = q("num")
    ist(cond.start, 0); ist(cond.end, 39)
    ist(one.start, 3); ist(one.end, 4)
    let loop = q("loop"), two = q("num", 2)
    ist(loop.start, 7); ist(loop.end, 37)
    ist(two.start, 13); ist(two.end, 14)
    let call = q("call"), inner = q("call", 2)
    ist(call.start, 17); ist(call.end, 34)
    ist(inner.start, 21); ist(inner.end, 33)
    let bar = q("var", 2), bug = q("var", 4)
    ist(bar.start, 21); ist(bar.end, 24)
    ist(bug.start, 29); ist(bug.end, 32)
  })

  let resolveDoc = "while 111 { one; two(three 20); }"

  function testResolve(bufferLength: number) {
    let parser = p1()
    let ast = parser.parse(resolveDoc, {strict: true, bufferLength})

    let cx111 = ast.resolve(7)
    ist(cx111.depth, 2)
    ist(cx111.tag.tag, "num")
    ist(cx111.start, 6)
    ist(cx111.end, 9)
    ist(cx111.parent!.tag.tag, "loop")
    ist(cx111.parent!.start, 0)
    ist(cx111.parent!.end, 33)

    let cxThree = ast.resolve(22)
    ist(cxThree.depth, 4)
    ist(cxThree.tag.tag, "var")
    ist(cxThree.start, 21)
    ist(cxThree.end, 26)

    let cxCall = cxThree.parent!
    ist(cxCall.tag.tag, "call")
    ist(cxCall.start, 17)
    ist(cxCall.end, 30)

    let branch = cxThree.resolve(18)
    ist(branch.depth, 4)
    ist(branch.tag.tag, "var")
    ist(branch.start, 17)
    ist(branch.end, 20)

    // Always resolve to the uppermost context for a position
    ist(ast.resolve(6).depth, 1)
    ist(ast.resolve(9).depth, 1)

    ist(cxCall.childBefore(cxCall.start), null)
    ist(cxCall.childAfter(cxCall.end), null)
    ist(cxCall.childBefore(27)!.tag.tag, "var")
    ist(cxCall.childAfter(26)!.tag.tag, "num")
    ist(cxCall.childBefore(28)!.tag.tag, "num")
    ist(cxCall.childAfter(28)!.tag.tag, "num")
  }

  it("can resolve positions in buffers", () => testResolve(1024))

  it("can resolve positions in trees", () => testResolve(2))

  let iterDoc = "while 1 { a; b; c(d e); } while 2 { f; }"
  let iterSeq = ["loop", 0, "num", 6, "/num", 7, "block", 8, "var", 10, "/var", 11,
                 "var", 13, "/var", 14, "call", 16, "var", 16, "/var", 17,
                 "var", 18, "/var", 19, "var", 20, "/var", 21, "/call", 22,
                 "/block", 25, "/loop", 25, "loop", 26, "num", 32, "/num", 33, "block", 34, "var", 36,
                 "/var", 37, "/block", 40, "/loop", 40]
  // Node boundaries seen when iterating range 13-19 ("b; c(d")
  let partialSeq = ["loop", 0, "block", 8, "var", 13, "/var", 14, "call", 16, "var", 16,
                    "/var", 17, "var", 18, "/var", 19, "/call", 22, "/block", 25, "/loop", 25]

  function testIter(bufferLength: number, partial: boolean) {
    let parser = p1(), output: any[] = []
    let ast = parser.parse(iterDoc, {strict: true, bufferLength})
    ast.iterate(partial ? 13 : 0, partial ? 19 : ast.length,
                (open, start) => { output.push(open.tag, start) },
                (close, _, end) => { output.push("/" + close.tag, end) })
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
                (close, _, end) => { output.push(end, "/" + close.tag) },
                (open, start) => { output.push(start, open.tag) })
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
expr { binOp | var }
binOp:bin { expr !plus "+" expr | expr !times "*" expr }
skip { space }
tokens { space { " "+ } var:var { "x" } "*":times "+":plus }
`)
    let ast = parser.parse("x + x + x", {strict: true, bufferLength: 2})
    ist(ast.toString(), "bin(bin(var,plus,var),plus,var)")
    let ast2 = parser.parse("x * x + x + x", {strict: true, bufferLength: 2, cache: change(ast, [0, 0, 0, 4])})
    ist(ast2.toString(), "bin(bin(bin(var,times,var),plus,var),plus,var)")
  })

  it("can cache skipped content", () => {
    let comments = buildParser(`
top { "x"+ }
skip { space | comment }
skip {} {
  comment:comment { commentStart (comment | commentContent)* commentEnd }
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
    top { (x | y)+ }
    x:x { "x" }
    y:y { "y" ";"* }`)

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
    ast.iterate(0, ast.length, (tag, start, end) => {
      if (i == 100) {
        ist(tag.tag, "y")
        ist(start, 100)
        ist(end, 110)
      } else {
        ist(tag.tag, "x")
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
expr { ("(":p.open expr+ ")":p.close):b | ".":dot }`)
    let outer = buildParser(`
external grammar inner from "."
top { expr+ }
expr { "[[" nest.inner<:foo> "]]" | "!":bang }
tokens { "[[":b.open "]]":b.close }
`, {nestedGrammar() { return inner }})

    ist(outer.parse("![[((.).)]][[.]]").toString(),
        'bang,b.open,foo(b(p.open,b(p.open,dot,p.close),dot,p.close)),b.close,b.open,foo(dot),b.close')
    ist(outer.parse("[[/\]]").toString(),
        'b.open,foo(⚠),b.close')
  })

  it("supports conditional nesting and end token predicates", () => {
    let inner = buildParser(`top { any } tokens { any { _+ } }`)
    let outer = buildParser(`
external grammar inner from "."
top { tag }
tag:tag { open nest.inner<:text, "</" name ">", tag*> close }
open:open { "<" name ">" }
close:close { "</" name ">" }
tokens {
  name { std.asciiLetter+ }
}
`, {nestedGrammar() { return nest }})

    function nest(stream: InputStream, stack: Stack) {
      let tag = /<(\w+)>$/.exec(stream.read(stack.ruleStart, stack.pos))
      if (!tag || !["textarea", "script", "style"].includes(tag[1])) return {stay: true}
      return {
        parser: inner,
        filterEnd(token: string) { return token == "</" + tag![1] + ">" }
      }
    }

    ist(outer.parse("<foo><bar></baz></foo>").toString(),
        "tag(open,tag(open,close),close)")
    ist(outer.parse("<textarea><bar></baz></textarea>").toString(),
        "tag(open,text,close)")
  })

  it("allows updating the nested grammars for a parser", () => {
    let inner1 = buildParser(`top { "x":a+ }`)
    let inner2 = buildParser(`top { "x":b+ }`)
    let outer = buildParser(`external grammar inner from "." top { "[" nest.inner<:nest> "]" } tokens { "[":o "]":c }`,
                            {nestedGrammar() { return inner1 }})
    ist(outer.parse("[x]").toString(), 'o,nest(a),c')
    ist(outer.withNested({inner: inner2}).parse("[x]").toString(), 'o,nest(b),c')
  })

  it("supports tag-less nesting", () => {
    let inner = buildParser(`top { "x":x }`)
    let outer = buildParser(`external grammar x from "." top { "&" nest.x "&" } tokens { "&":and }`, {nestedGrammar() { return inner }})
    ist(outer.parse("&x&").toString(), 'and,x,and')
  })

  it("skips ranges with missing nested parsers", () => {
    let outer = buildParser(`external grammar inner top { "[" nest.inner<:n> "]" } tokens { "[":o "]":c }`)
    ist(outer.parse("[lfkdsajfa]").toString(), 'o,n,c')
  })
})
