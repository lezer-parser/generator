import {buildParser, BuildOptions} from ".."
import {LRParser, Tree} from "lezer"
import {TreeFragment, NodeProp, NodeType, InputGap, parse} from "lezer-tree"
// @ts-ignore
import {testTree} from "../dist/test.cjs"
import ist from "ist"

function p(text: string, options?: BuildOptions): () => LRParser {
  let value: LRParser | null = null
  return () => value || (value = buildParser(text, Object.assign({}, {warn(e: string) { throw new Error(e) }}, options)))
}

function shared(a: Tree, b: Tree) {
  let inA = new Set<Tree>(), shared = 0
  ;(function register(t: any) {
    if (t instanceof Tree) {
      let mounted = t.prop(NodeProp.mountedTree)
      if (mounted) t = mounted
      t.children.forEach(register)
    }
    inA.add(t)
  })(a)
  ;(function scan(t: any) {
    if (inA.has(t)) shared += t.length
    else if (t instanceof Tree) (t.prop(NodeProp.mountedTree) || t).children.forEach(scan)
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
    let ast = parse(p1().configure({bufferLength: 2}), {input: doc})
    let expected = "T(Cond(Var,Block(Call(Var,Num),Var)),Loop(Var,Block(Cond(Num,Call(Var,Var,Num,Num,Num)))))"
    testTree(ast, expected)
    ist(ast.length, 70)
    let pos = doc.indexOf("false"), doc2 = doc.slice(0, pos) + "x" + doc.slice(pos + 5)
    let ast2 = parse(p1().configure({bufferLength: 2}), {
      input: doc2,
      fragments: fragments(ast, [pos, pos + 5, pos, pos + 1])
    })
    testTree(ast2, expected)
    ist(shared(ast, ast2), 40, ">")
    ist(ast2.length, 66)
  })

  it("assigns the correct node positions", () => {
    let doc = "if 1 { while 2 { foo(bar(baz bug)); } }"
    let ast = parse(p1().configure({bufferLength: 10, strict: true}), {input: doc})
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
    let ast = parse(p1().configure({strict: true, bufferLength}), {input: resolveDoc})

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
    let ast = parse(parser.configure({strict: true, bufferLength}), {input: iterDoc})
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
    let ast = parse(p1(), {input: "foo(baz(baz), bug(quux)"})
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
    let p = parser.configure({strict: true, bufferLength: 2})
    let ast = parse(p, {input: "x + x + x"})
    testTree(ast, "T(Bin(Bin(Var,Plus,Var),Plus,Var))")
    let ast2 = parse(p, {input: "x * x + x + x", fragments: fragments(ast, [0, 0, 0, 4])})
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
    let ast = parse(comments.configure({bufferLength: 10, strict: true}), {input: doc})
    let ast2 = parse(comments.configure({bufferLength: 10}), {input: doc.slice(1), fragments: fragments(ast, [0, 1, 0, 0])})
    ist(shared(ast, ast2), 80, ">")
  })

  it("doesn't get slow on long invalid input", () => {
    let t0 = Date.now()
    let ast = parse(p1(), {input: "#".repeat(2000)})
    // Testing for timing is always dodgy, but I'm trying to ensure
    // there's no exponential complexity here. This runs (cold) in
    // ~60ms on my machine. In case of exponentiality it should become
    // _extremely_ slow.
    ist(Date.now() - t0 < 500)
    ist(ast.toString(), "T(⚠)")
  })


  it("supports input gaps", () => {
    let placeholder = NodeType.define({id: 1, name: "Gap"})
    let tree = parse(p1(), {
      input: `if 1{{x}}0{{y}}0 foo {{z}};`,
      gaps: [new InputGap(4, 9, new Tree(placeholder, [], [], 5)),
             new InputGap(10, 15, new Tree(placeholder, [], [], 5)),
             new InputGap(21, 26, new Tree(placeholder, [], [], 5))]
    })
    ist(tree.toString(), "T(Cond(Num(Gap,Gap),Var,Gap))")
    let num = tree.resolve(3, 1)
    ist(num.name, "Num")
    ist(num.from, 3)
    ist(num.to, 16)
    ist(num.firstChild!.name, "Gap")
    ist(num.firstChild!.from, 4)
    ist(num.lastChild!.name, "Gap")
    ist(num.lastChild!.from, 10)
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
    let ast = parse(p1().configure({strict: true, bufferLength: 10}), {input: "x".repeat(1000)})
    let d = depth(ast), b = breadth(ast)
    ist(d, 6, "<=")
    ist(d, 4, ">=")
    ist(b, 5, ">=")
    ist(b, 10, "<=")
  })

  it("creates a tree for long content-less repeats", () => {
    let p = buildParser(`
@top T { (A | B { "[" b+ "]" })+ }
@tokens {
  A { "a" }
  b { "b" }
}`).configure({bufferLength: 10})
    let tree = parse(p, {input: "a[" + "b".repeat(500) + "]"})
    ist(tree.toString(), "T(A,B)")
    ist(depth(tree), 5, ">=")
  })

  it("balancing doesn't get confused by skipped nodes", () => {
    let ast = parse(p1().configure({strict: true, bufferLength: 10}), {input: "xc".repeat(1000)})
    let d = depth(ast), b = breadth(ast)
    ist(d, 6, "<=")
    ist(d, 4, ">=")
    ist(b, 5, ">=")
    ist(b, 10, "<=")
  })

  it("caches parts of sequences", () => {
    let doc = "x".repeat(1000), p = p1().configure({bufferLength: 10})
    let ast = parse(p, {input: doc})
    let full = parse(p, {input: doc, fragments: TreeFragment.addTree(ast)})
    ist(shared(ast, full), 99, ">")
    let front = parse(p, {input: doc, fragments: fragments(ast, [900, 1000])})
    ist(shared(ast, front), 50, ">")
    let back = parse(p, {input: doc, fragments: fragments(ast, [0, 100])})
    ist(shared(ast, back), 50, ">")
    let middle = parse(p, {input: doc, fragments: fragments(ast, [0, 100], [900, 1000])})
    ist(shared(ast, middle), 50, ">")
    let sides = parse(p, {input: doc, fragments: fragments(ast, [450, 550])})
    ist(shared(ast, sides), 50, ">")
  })

  it("assigns the right positions to sequences", () => {
    let doc = "x".repeat(100) + "y;;;;;;;;;" + "x".repeat(90)
    let ast = parse(p1().configure({bufferLength: 10}), {input: doc})
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

    testTree(parse(parser, {input: "bc"}), "X(FOO(B), C)")
    testTree(parse(parser.configure({top: "X"}), {input: "bc"}), "X(FOO(B), C)")
    testTree(parse(parser.configure({top: "Y"}), {input: "bc"}), "Y(B, C)")
  })

  it("parses first top as default", () => {
    let parser = buildParser(`
@top X { FOO C }
@top Y { B C }
FOO { B }
B { "b" }
C { "c" }
`)

    testTree(parse(parser, {input: "bc"}), "X(FOO(B), C)")
    testTree(parse(parser.configure({top: "Y"}), {input: "bc"}), "Y(B, C)")
  })
})

function getTerm(parser: LRParser, name: string) {
  return (parser as any).termTable[name]
}

describe("nesting", () => {
  it("can nest grammars", () => {
    let inner = buildParser(`
@top I { expr+ }
expr { B { Open{"("} expr+ Close{")"} } | Dot{"."} }`)
    let outer = buildParser(`
@top O { expr+ }
expr { "[[" NestContent "]]" | Bang{"!"} }
@tokens {
  NestContent[@export] { ![\\]]+ }
  "[["[@name=Start] "]]"[@name=End]
}
`)
    outer = outer.configure({
      nested: {[getTerm(outer, "NestContent")]: () => inner}
    })

    testTree(parse(outer, {input: "![[((.).)]][[.]]"}),
             'O(Bang,Start,I(B(Open,B(Open,Dot,Close),Dot,Close)),End,Start,I(Dot),End)')
    testTree(parse(outer, {input: "[[/\]]"}), 'O(Start,I(⚠),End)')

    let tree = parse(outer, {input: "[[(.)]]"})
    let innerNode = tree.topNode.childAfter(2)!
    ist(innerNode.name, "I")
    ist(innerNode.from, 2)
    ist(innerNode.to, 5)
    ist(innerNode.firstChild!.from, 2)
    ist(innerNode.firstChild!.to, 5)
  })

  it("supports conditional nesting", () => {
    let inner = buildParser(`@top Script { any } @tokens { any { ![]+ } }`)
    let outer = buildParser(`
@top T { Tag }
Tag { Open Content? Close }
Open { "<" name ">" }
Close { "</" name ">" }
@tokens {
  name { std.asciiLetter+ }
  Content { ![<]+ }
}`)
    outer = outer.configure({
      nested: {
        [getTerm(outer, "Content")]: (input, from) => {
          let off = Math.max(0, from - 20), text = input.chunk(off)
          let tag = /<([^>]+)>$/.exec(text.slice(0, from - off))
          return tag && tag[1] == "script" ? inner : null
        }
      }
    })
    testTree(parse(outer, {input: "<foo>bar</foo>"}),
             "T(Tag(Open,Content,Close))")
    testTree(parse(outer, {input: "<script>hello</script>"}),
             "T(Tag(Open,Script,Close))")
  })

  it("can parse incrementally across nesting", () => {
    let inner = buildParser(`@top Blob { "b"* }`)
    let outer = buildParser(`
@top Program { (Nest | Name)* }
@skip { space }
@skip {} {
  Nest { "{" Nested "}" }
  Nested { nestedChar* }
}
@tokens {
  space { std.whitespace+ }
  nestedChar { ![}] }
  Name { $[a-z]+ }
}`)
    outer = outer.configure({
      bufferLength: 10,
      nested: {[getTerm(outer, "Nested")]: () => inner}
    })
    let base = "hello {bbbb} "
    let doc = base.repeat(500) + "{" + "b".repeat(1000) + "} " + base.repeat(500), off = base.length * 500 + 500
    let ast1 = parse(outer, {input: doc})
    let ast2 = parse(outer, {
      input: doc.slice(0, off) + "bbb" + doc.slice(off),
      fragments: TreeFragment.applyChanges(TreeFragment.addTree(ast1), [{fromA: off, toA: off, fromB: off, toB: off + 3}])
    })
    ist(ast1.toString(), ast2.toString())
    ist(shared(ast1, ast2), 90, ">")
  })

  it("allows a nested parse to materialize the replaced node", () => {
    let inner = buildParser(`@top X { "x"+ }`)
    let outer = buildParser(`@top Y { Z } Z { "x"+ }`)
    outer = outer.configure({nested: {
      [getTerm(outer, "Z")]: () => (node) => {
        ist(node.toString(), "Z")
        ist(node.length, 5)
        return inner
      }
    }})
    ist(parse(outer, {input: "xxxxx"}).toString(), "Y(X)")
  })
})
