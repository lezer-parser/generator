import {buildParser, BuildOptions} from ".."
import {Parser, Input, Stack, Tree} from "lezer"
import {TreeFragment} from "lezer-tree"
// @ts-ignore
import {testTree} from "../dist/test.cjs"
import ist from "ist"

function p(text: string, options?: BuildOptions): () => Parser {
  let value: Parser | null = null
  return () => value || (value = buildParser(text, Object.assign({}, {warn(e: string) { throw new Error(e) }}, options)))
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

function fragments(tree: Tree, ...changes: ([number, number] | [number, number, number, number])[]) {
  return TreeFragment.applyChanges(TreeFragment.addTree(tree),
                                   changes.map(([fromA, toA, fromB = fromA, toB = toA]) => ({fromA, toA, fromB, toB})),
                                   2)
}

describe("parsing", () => {
  let p1 = p(`
    @precedence { call }

    @top T { statement* }
    statement { Cond | Loop | Block | expression ";" }
    Cond { kw<"if"> expression statement }
    Block { "{" statement* "}" }
    Loop { kw<"while"> expression statement }
    expression { Call | Num | Var | "!" expression }
    Call { expression !call "(" expression* ")" }

    kw<value> { @specialize<Var, value> }
    @tokens {
      Num { std.digit+ }
      Var { std.asciiLetter+ }
      whitespace { std.whitespace+ }
    }
    @skip { whitespace }`)

  function qq(ast: Tree) {
    return function(query: string, offset = 1): {start: number, end: number} {
      let result = null, cursor = ast.cursor()
      do {
        if (cursor.name == query && --offset == 0) result = {start: cursor.from, end: cursor.to}
      } while (cursor.next())
      if (result) return result
      throw new Error("Couldn't find " + query)
    }
  }

  it("can parse incrementally", () => {
    let doc = "if true { print(1); hello; } while false { if 1 do(something 1 2 3); }"
    let ast = p1().parse(doc, {bufferLength: 2})
    let expected = "T(Cond(Var,Block(Call(Var,Num),Var)),Loop(Var,Block(Cond(Num,Call(Var,Var,Num,Num,Num)))))"
    testTree(ast, expected)
    ist(ast.length, 70)
    let pos = doc.indexOf("false"), doc2 = doc.slice(0, pos) + "x" + doc.slice(pos + 5)
    let ast2 = p1().parse(doc2, {bufferLength: 2, fragments: fragments(ast, [pos, pos + 5, pos, pos + 1])})
    testTree(ast2, expected)
    ist(shared(ast, ast2), 40, ">")
    ist(ast2.length, 66)
  })

  it("assigns the correct node positions", () => {
    let doc = "if 1 { while 2 { foo(bar(baz bug)); } }"
    let ast = p1().parse(doc, {bufferLength: 10, strict: true})
    let q = qq(ast)
    ist(ast.length, 39)
    let cond = q("Cond"), one = q("Num")
    ist(cond.start, 0); ist(cond.end, 39)
    ist(one.start, 3); ist(one.end, 4)
    let loop = q("Loop"), two = q("Num", 2)
    ist(loop.start, 7); ist(loop.end, 37)
    ist(two.start, 13); ist(two.end, 14)
    let call = q("Call"), inner = q("Call", 2)
    ist(call.start, 17); ist(call.end, 34)
    ist(inner.start, 21); ist(inner.end, 33)
    let bar = q("Var", 2), bug = q("Var", 4)
    ist(bar.start, 21); ist(bar.end, 24)
    ist(bug.start, 29); ist(bug.end, 32)
  })

  let resolveDoc = "while 111 { one; two(three 20); }"

  function testResolve(bufferLength: number) {
    let ast = p1().parse(resolveDoc, {strict: true, bufferLength})

    let cx111 = ast.cursor(7)
    ist(cx111.name, "Num")
    ist(cx111.from, 6)
    ist(cx111.to, 9)
    cx111.parent()
    ist(cx111.name, "Loop")
    ist(cx111.from, 0)
    ist(cx111.to, 33)

    let cxThree = ast.cursor(22)
    ist(cxThree.name, "Var")
    ist(cxThree.from, 21)
    ist(cxThree.to, 26)
    cxThree.parent()
    ist(cxThree.name, "Call")
    ist(cxThree.from, 17)
    ist(cxThree.to, 30)

    let branch = cxThree.moveTo(18)
    ist(branch.name, "Var")
    ist(branch.from, 17)
    ist(branch.to, 20)

    // Always resolve to the uppermost context for a position
    ist(ast.cursor(6).name, "Loop")
    ist(ast.cursor(9).name, "Loop")

    let c = ast.cursor(20)
    ist(c.firstChild())
    ist(c.name, "Var")
    ist(c.nextSibling())
    ist(c.name, "Var")
    ist(c.nextSibling())
    ist(c.name, "Num")
    ist(!c.nextSibling())
  }

  it("can resolve positions in buffers", () => testResolve(1024))

  it("can resolve positions in trees", () => testResolve(2))

  let iterDoc = "while 1 { a; b; c(d e); } while 2 { f; }"
  let iterSeq = ["T", 0, "Loop", 0, "Num", 6, "/Num", 7, "Block", 8, "Var", 10, "/Var", 11,
                 "Var", 13, "/Var", 14, "Call", 16, "Var", 16, "/Var", 17,
                 "Var", 18, "/Var", 19, "Var", 20, "/Var", 21, "/Call", 22,
                 "/Block", 25, "/Loop", 25, "Loop", 26, "Num", 32, "/Num", 33, "Block", 34, "Var", 36,
                 "/Var", 37, "/Block", 40, "/Loop", 40, "/T", 40]
  // Node boundaries seen when iterating range 13-19 ("b; c(d")
  let partialSeq = ["T", 0, "Loop", 0, "Block", 8, "Var", 13, "/Var", 14, "Call", 16, "Var", 16,
                    "/Var", 17, "Var", 18, "/Var", 19, "/Call", 22, "/Block", 25, "/Loop", 25, "/T", 40]

  function testIter(bufferLength: number, partial: boolean) {
    let parser = p1(), output: any[] = []
    let ast = parser.parse(iterDoc, {strict: true, bufferLength})
    ast.iterate({
      from: partial ? 13 : 0,
      to: partial ? 19 : ast.length,
      enter(open, start) { output.push(open.name, start) },
      leave(close, _, end) { output.push("/" + close.name, end) }
    })
    ist(output.join(), (partial ? partialSeq : iterSeq).join())
  }

  it("supports forward iteration in buffers", () => testIter(1024, false))

  it("supports forward iteration in trees", () => testIter(2, false))

  it("supports partial forward iteration in buffers", () => testIter(1024, true))

  it("supports partial forward iteration in trees", () => testIter(2, true))

  it("can skip individual nodes during iteration", () => {
    let ast = p1().parse("foo(baz(baz), bug(quux)")
    let ids = 0
    ast.iterate({
      enter(type, start) {
        if (type.name == "Var") ids++
        return start == 4 && type.name == "Call" ? false : undefined
      }
    })
    ist(ids, 3)
  })

  it("doesn't incorrectly reuse nodes", () => {
    let parser = buildParser(`
@precedence { times @left, plus @left }
@top T { expr+ }
expr { Bin | Var }
Bin { expr !plus "+" expr | expr !times "*" expr }
@skip { space }
@tokens { space { " "+ } Var { "x" } "*"[@name=Times] "+"[@name=Plus] }
`)
    let ast = parser.parse("x + x + x", {strict: true, bufferLength: 2})
    testTree(ast, "T(Bin(Bin(Var,Plus,Var),Plus,Var))")
    let ast2 = parser.parse("x * x + x + x", {strict: true, bufferLength: 2, fragments: fragments(ast, [0, 0, 0, 4])})
    testTree(ast2, "T(Bin(Bin(Bin(Var,Times,Var),Plus,Var),Plus,Var))")
  })

  it("can cache skipped content", () => {
    let comments = buildParser(`
@top T { "x"+ }
@skip { space | Comment }
@skip {} {
  Comment { commentStart (Comment | commentContent)* commentEnd }
}
@tokens {
  space { " "+ }
  commentStart { "(" }
  commentEnd { ")" }
  commentContent { ![()]+ }
}`)
    let doc = "x  (one (two) (three " + "(y)".repeat(500) + ")) x"
    let ast = comments.parse(doc, {bufferLength: 10, strict: true})
    let ast2 = comments.parse(doc.slice(1), {fragments: fragments(ast, [0, 1, 0, 0]), bufferLength: 10})
    ist(shared(ast, ast2), 80, ">")
  })

  it("doesn't get slow on long invalid input", () => {
    let t0 = Date.now()
    let ast = p1().parse("#".repeat(2000))
    // Testing for timing is always dodgy, but I'm trying to ensure
    // there's no exponential complexity here. This runs (cold) in
    // ~60ms on my machine. In case of exponentiality it should become
    // _extremely_ slow.
    ist(Date.now() - t0 < 500)
    ist(ast.toString(), "T(⚠)")
  })
})

describe("sequences", () => {
  let p1 = p(`
    @top T { (X | Y)+ }
    @skip { C }
    C { "c" }
    X { "x" }
    Y { "y" ";"* }`)

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

  it("balancing doesn't get confused by skipped nodes", () => {
    let ast = p1().parse("xc".repeat(1000), {strict: true, bufferLength: 10})
    let d = depth(ast), b = breadth(ast)
    ist(d, 6, "<=")
    ist(d, 4, ">=")
    ist(b, 5, ">=")
    ist(b, 10, "<=")
  })

  it("caches parts of sequences", () => {
    let doc = "x".repeat(1000), p = p1()
    let ast = p.parse(doc, {bufferLength: 10})
    let full = p.parse(doc, {fragments: TreeFragment.addTree(ast), bufferLength: 10})
    ist(shared(ast, full), 99, ">")
    let front = p.parse(doc, {fragments: fragments(ast, [900, 1000]), bufferLength: 10})
    ist(shared(ast, front), 50, ">")
    let back = p.parse(doc, {fragments: fragments(ast, [0, 100]), bufferLength: 10})
    ist(shared(ast, back), 50, ">")
    let middle = p.parse(doc, {fragments: fragments(ast, [0, 100], [900, 1000]), bufferLength: 10})
    ist(shared(ast, middle), 50, ">")
    let sides = p.parse(doc, {fragments: fragments(ast, [450, 550]), bufferLength: 10})
    ist(shared(ast, sides), 50, ">")
  })

  it("assigns the right positions to sequences", () => {
    let doc = "x".repeat(100) + "y;;;;;;;;;" + "x".repeat(90)
    let parser = p1()
    let ast = parser.parse(doc, {bufferLength: 10})
    let i = 0
    ast.iterate({enter(type, start, end) {
      if (i == 0) {
        ist(type.name, "T")
      } else if (i == 101) {
        ist(type.name, "Y")
        ist(start, 100)
        ist(end, 110)
      } else {
        ist(type.name, "X")
        ist(end, start + 1)
        ist(start, i <= 100 ? i - 1 : i + 8)
      }
      i++
    }})
  })
})

describe("multiple tops", () => {
  it("parses named tops", () => {
    let parser = buildParser(`
@top X { FOO C }
@top Y { B C }
FOO { B }
B { "b" }
C { "c" }
`)

    testTree(parser.parse("bc"), "X(FOO(B), C)")
    testTree(parser.configure({top: "X"}).parse("bc"), "X(FOO(B), C)")
    testTree(parser.configure({top: "Y"}).parse("bc"), "Y(B, C)")
  })

  it("parses first top as default", () => {
    let parser = buildParser(`
@top X { FOO C }
@top Y { B C }
FOO { B }
B { "b" }
C { "c" }
`)

    testTree(parser.parse("bc"), "X(FOO(B), C)")
    testTree(parser.configure({top: "Y"}).parse("bc"), "Y(B, C)")
  })
})

describe("nesting", () => {
  it("can nest grammars", () => {
    let inner = buildParser(`
@top I { expr+ }
expr { B { Open{"("} expr+ Close{")"} } | Dot{"."} }`)
    let outer = buildParser(`
@external grammar inner from "."
@top O { expr+ }
expr { "[[" nest.inner "]]" | Bang{"!"} }
@tokens { "[["[@name=Start] "]]"[@name=End] }
`, {nestedParser() { return inner }})

    testTree(outer.parse("![[((.).)]][[.]]"), 'O(Bang,Start,I(B(Open,B(Open,Dot,Close),Dot,Close)),End,Start,I(Dot),End)')
    testTree(outer.parse("[[/\]]"), 'O(Start,I(⚠),End)')
  })

  it("supports conditional nesting and end token predicates", () => {
    let outer = buildParser(`
@external grammar inner from "."
@top T { Tag }
Tag { Open nest.inner<"</" name ">", Tag*> Close }
Open { "<" name ">" }
Close { "</" name ">" }
@tokens { name { std.asciiLetter+ } }
Text[@export] {}`, {
    nestedParser(_, terms) {
      return (stream: Input, stack: Stack) => {
        let tag = /<(\w+)>$/.exec(stream.read(stack.ruleStart, stack.pos))
        if (!tag || !["textarea", "script", "style"].includes(tag[1])) return null
        return {
          filterEnd: (token: string) => token == "</" + tag![1] + ">",
          wrapType: terms.Text
        }
      }
    }})

    testTree(outer.parse("<foo><bar></baz></foo>"),
             "T(Tag(Open,Tag(Open,Close),Close))")
    testTree(outer.parse("<textarea><bar></baz></textarea>"),
             "T(Tag(Open,Text,Close))")
  })

  it("allows updating the nested grammars for a parser", () => {
    let inner1 = buildParser(`@top T { A { "x" }+ }`)
    let inner2 = buildParser(`@top U { B { "x" }+ }`)
    let outer = buildParser(`@external grammar inner from "." @top V { "[" nest.inner "]" } @tokens { "["[@name=O] "]"[@name=C] }`,
                            {nestedParser() { return inner1 }})
    testTree(outer.parse("[x]"), "V(O,T(A),C)")
    testTree(outer.configure({nested: {inner: {parser: inner2}}}).parse("[x]"), "V(O,U(B),C)")
  })

  it("supports tag-less nesting", () => {
    let inner = buildParser(`@top U { X{"x"} }`)
    let outer = buildParser(`@external grammar x from "." @top T { "&" nest.x "&" } @tokens { "&"[@name=And] }`, {
      nestedParser() { return inner }
    })
    testTree(outer.parse("&x&"), 'T(And,U(X),And)')
  })

  it("skips ranges with missing nested parsers", () => {
    let outer = buildParser(`@external grammar inner empty @top T { "[" nest.inner "]" } @tokens { "["[@name=O] "]"[@name=C] }`)
    testTree(outer.parse("[lfkdsajfa]"), 'T(O,C)')
  })
})
