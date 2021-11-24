import {buildParser, BuildOptions} from "../dist/index.js"
import {Tree, TreeFragment, NodeProp, parseMixed, SyntaxNode} from "@lezer/common"
import {LRParser, ParserConfig} from "@lezer/lr"
// @ts-ignore
import {testTree} from "../dist/test.js"
import ist from "ist"

function p(text: string, options?: BuildOptions, config?: ParserConfig): () => LRParser {
  let value: LRParser | null = null
  return () => {
    if (!value) {
      value = buildParser(text, Object.assign({}, {warn(e: string) { throw new Error(e) }}, options))
      if (config) value = value.configure(config)
    }
    return value
  }
}

function shared(a: Tree, b: Tree) {
  let inA = new Set<Tree>(), shared = 0
  ;(function register(t: any) {
    if (t instanceof Tree) {
      let mounted = t.prop(NodeProp.mounted)
      if (mounted && !mounted.overlay) t = mounted.tree
      t.children.forEach(register)
    }
    inA.add(t)
  })(a)
  ;(function scan(t: any) {
    if (inA.has(t)) {
      shared += t.length
    } else if (t instanceof Tree) {
      let mounted = t.prop(NodeProp.mounted)
      ;(mounted && !mounted.overlay ? mounted.tree : t).children.forEach(scan)
    }
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
    let doc = "if true { print(1); hello; } while false { if 1 do(something 1 2 3); }".repeat(10)
    let ast = p1().configure({bufferLength: 2}).parse(doc)
    let content = "Cond(Var,Block(Call(Var,Num),Var)),Loop(Var,Block(Cond(Num,Call(Var,Var,Num,Num,Num))))"
    let expected = "T(" + (content + ",").repeat(9) + content + ")"
    testTree(ast, expected)
    ist(ast.length, 700)
    let pos = doc.indexOf("false"), doc2 = doc.slice(0, pos) + "x" + doc.slice(pos + 5)
    let ast2 = p1().configure({bufferLength: 2}).parse(doc2, fragments(ast, [pos, pos + 5, pos, pos + 1]))
    testTree(ast2, expected)
    ist(shared(ast, ast2), 40, ">")
    ist(ast2.length, 696)
  })

  it("assigns the correct node positions", () => {
    let doc = "if 1 { while 2 { foo(bar(baz bug)); } }"
    let ast = p1().configure({bufferLength: 10, strict: true}).parse(doc)
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
    let ast = p1().configure({strict: true, bufferLength}).parse(resolveDoc)

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
    let ast = parser.configure({strict: true, bufferLength}).parse(iterDoc)
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
    let p = parser.configure({strict: true, bufferLength: 2})
    let ast = p.parse("x + x + x")
    testTree(ast, "T(Bin(Bin(Var,Plus,Var),Plus,Var))")
    let ast2 = p.parse("x * x + x + x", fragments(ast, [0, 0, 0, 4]))
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
    let ast = comments.configure({bufferLength: 10, strict: true}).parse(doc)
    let ast2 = comments.configure({bufferLength: 10}).parse(doc.slice(1), fragments(ast, [0, 1, 0, 0]))
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


  it("supports input ranges", () => {
    let tree = p1().parse(`if 1{{x}}0{{y}}0 foo {{z}};`,
                          [],
                          [{from: 0, to: 4}, {from: 9, to: 10}, {from: 15, to: 21}, {from: 26, to: 27}])
    ist(tree.toString(), "T(Cond(Num,Var))")
  })

  it("doesn't reuse nodes whose tokens looked ahead beyond the unchanged fragments", () => {
    let comments = buildParser(`
@top Top { (Group | Char)* }
@tokens {
  Group { "(" ![)]* ")" }
  Char { _ }
}`).configure({bufferLength: 10})
    let doc = "xxx(" + "x".repeat(996)
    let tree1 = comments.parse(doc)
    let tree2 = comments.parse(
      doc + ")",
      TreeFragment.applyChanges(TreeFragment.addTree(tree1), [{fromA: 1000, toA: 1000, fromB: 1000, toB: 1001}])
    )
    ist(tree2.toString(), "Top(Char,Char,Char,Group)")
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
    let ast = p1().configure({strict: true, bufferLength: 10}).parse("x".repeat(1000))
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
    let tree = p.parse("a[" + "b".repeat(500) + "]")
    ist(tree.toString(), "T(A,B)")
    ist(depth(tree), 5, ">=")
  })

  it("balancing doesn't get confused by skipped nodes", () => {
    let ast = p1().configure({strict: true, bufferLength: 10}).parse("xc".repeat(1000))
    let d = depth(ast), b = breadth(ast)
    ist(d, 6, "<=")
    ist(d, 4, ">=")
    ist(b, 5, ">=")
    ist(b, 10, "<=")
  })

  it("caches parts of sequences", () => {
    let doc = "x".repeat(1000), p = p1().configure({bufferLength: 10})
    let ast = p.parse(doc)
    let full = p.parse(doc, TreeFragment.addTree(ast))
    ist(shared(ast, full), 99, ">")
    let front = p.parse(doc, fragments(ast, [900, 1000]))
    ist(shared(ast, front), 50, ">")
    let back = p.parse(doc, fragments(ast, [0, 100]))
    ist(shared(ast, back), 50, ">")
    let middle = p.parse(doc, fragments(ast, [0, 100], [900, 1000]))
    ist(shared(ast, middle), 50, ">")
    let sides = p.parse(doc, fragments(ast, [450, 550]))
    ist(shared(ast, sides), 50, ">")
  })

  it("assigns the right positions to sequences", () => {
    let doc = "x".repeat(100) + "y;;;;;;;;;" + "x".repeat(90)
    let ast = p1().configure({bufferLength: 10}).parse(doc)
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

describe("mixed languages", () => {
  const blob = p(`@top Blob { ch* } @tokens { ch { _ } }`, undefined, {bufferLength: 10})

  it("can mix grammars", () => {
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
    `).configure({
      wrap: parseMixed(node => {
        if (node.name == "NestContent") return {parser: inner}
        return null
      })
    })

    testTree(outer.parse("![[((.).)]][[.]]"),
             'O(Bang,Start,I(B(Open,B(Open,Dot,Close),Dot,Close)),End,Start,I(Dot),End)')
    testTree(outer.parse("[[/\]]"), 'O(Start,I(⚠),End)')

    let tree = outer.parse("[[(.)]]")
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
      }
    `).configure({
      wrap: parseMixed((node, input) => {
        if (node.name == "Content") {
          let open = node.node.parent!.firstChild!
          if (input.read(open.from, open.to) == "<script>")
            return {parser: inner}
        }
        return null
      })
    })
    testTree(outer.parse("<foo>bar</foo>"),
             "T(Tag(Open,Content,Close))")
    testTree(outer.parse("<script>hello</script>"),
             "T(Tag(Open,Script,Close))")
  })

  it("can parse incrementally across nesting", () => {
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
      }
    `).configure({
      bufferLength: 10,
      wrap: parseMixed(node => node.name == "Nested" ? {parser: blob()} : null)
    })
    let base = "hello {bbbb} "
    let doc = base.repeat(500) + "{" + "b".repeat(1000) + "} " + base.repeat(500), off = base.length * 500 + 500
    let ast1 = outer.parse(doc)
    let ast2 = outer.parse(
      doc.slice(0, off) + "bbb" + doc.slice(off),
      TreeFragment.applyChanges(TreeFragment.addTree(ast1), [{fromA: off, toA: off, fromB: off, toB: off + 3}])
    )
    ist(ast1.toString(), ast2.toString())
    ist(shared(ast1, ast2), 90, ">")
  })

  it("can create overlays", () => {
    let outer = buildParser(`
      @top Doc { (Dir | Content)* }
      Dir { "{{" Word "}}" }
      @tokens {
        Content { (![{] | "{" ![{])+ }
        Word { $[a-z]+ }
      }
    `)
    let mix = outer.configure({
      wrap: parseMixed(node => {
        return node.name == "Doc" ? {
          parser: blob(),
          overlay: node => node.name == "Content"
        } : null
      })
    })
    let tree = mix.parse("foo{{bar}}baz{{bug}}")
    ist(tree.toString(), "Doc(Content,Dir(Word),Content,Dir(Word))")
    let c1 = tree.resolveInner(1)
    ist(c1.name, "Blob")
    ist(c1.from, 0)
    ist(c1.to, 13)
    ist(c1.parent!.name, "Doc")
    ist(tree.resolveInner(10, 1).name, "Blob")

    let mix2 = outer.configure({
      wrap: parseMixed(node => {
        return node.name == "Doc" ? {
          parser: blob(),
          overlay: [{from: 5, to: 7}]
        } : null
      })
    })
    let tree2 = mix2.parse("{{a}}bc{{d}}")
    let c2 = tree2.resolveInner(6)
    ist(c2.name, "Blob")
    ist(c2.from, 5)
    ist(c2.to, 7)
  })

  it("reuses ranges from previous parses", () => {
    let outer = buildParser(`
      @top Doc { expr* }
      expr {
        Paren { "(" expr* ")" } |
        Array { "[" expr* "]" } |
        Number |
        String
      }
      @skip { space }
      @tokens {
        Number { $[0-9]+ }
        String { "'" ![']* "'" }
        space { $[ \n]+ }
      }
    `).configure({
      bufferLength: 2,
      wrap: parseMixed(node => {
        return node.name == "Array" ? {
          parser: blob(),
          overlay: node => {
            if (node.name == "String") {
              queried.push(node.from)
              return true
            }
            return false
          }
        } : null
      })
    })
    let queried: number[] = []

    let doc = " (100) (() [50] 123456789012345678901234 ((['one' 123456789012345678901234 (('two'))]) ['three'])) "
    let tree = outer.parse(doc)
    ist(tree.toString(),
        "Doc(Paren(Number),Paren(Paren,Array(Number),Number,Paren(Paren(Array(String,Number,Paren(Paren(String)))),Array(String))))")
    let inOne = tree.resolveInner(45)
    ist(inOne.name, "Blob")
    ist(inOne.from, 44)
    ist(inOne.to, 82)
    ist(inOne.nextSibling, null)
    ist(inOne.prevSibling, null)
    ist(inOne.parent!.name, "Array")
    ist(tree.resolveInner(89).name, "Blob")
    ist(queried.join(), "44,77,88")
    queried.length = 0

    let tree2 = outer.parse(doc.slice(0, 45) + "x" + doc.slice(46), fragments(tree, [45, 46]))
    ist(queried.join(), "44")
    ist(shared(tree, tree2), 20, ">")
  })

  it("properly handles fragment offsets", () => {
    let inner = buildParser(`@top Text { (Word | " ")* } @tokens { Word { ![ ]+ } }`).configure({bufferLength: 2})
    let outer = buildParser(`
      @top Doc { expr* }
      expr { Wrap { "(" expr* ")" } | Templ { "[" expr* "]" } | Number | String }
      @skip { space }
      @tokens {
        Number { $[0-9]+ }
        String { "'" ![']* "'" }
        space { $[ \n]+ }
      }
    `).configure({
      bufferLength: 2,
      wrap: parseMixed(node => {
        return node.name == "Templ" ? {
          parser: inner,
          overlay: node => node.name == "String" ? {from: node.from + 1, to: node.to - 1} : false
        } : null
      })
    })

    let doc = " 0123456789012345678901234 (['123456789 123456789 12345 stuff' 123456789 (('123456789 123456789 12345 other' 4))] 200)"
    let tree = outer.parse(doc)

    // Verify that mounts inside reused nodes don't get re-parsed
    let tree1 = outer.parse("88" + doc, fragments(tree, [0, 0, 0, 2]))
    ist(tree.resolveInner(50).tree, tree1.resolveInner(52).tree)

    // Verify that content inside the nested parse gets accurately reused
    let tree2 = outer.parse("88" + doc.slice(0, 30) + doc.slice(31), fragments(tree, [0, 0, 0, 2], [30, 31, 32, 32]))
    ist(shared(tree, tree2), 20, ">")
    ist(shared(tree.resolveInner(49).tree!, tree2.resolveInner(50).tree!), 20, ">")
    let other = tree2.resolveInner(103, 1)
    ist(other.from, 103)
    ist(other.to, 108)
  })

  it("supports nested overlays", () => {
    let outer = buildParser(`
      @top Doc { expr* }
      expr {
        Paren { "(" expr* ")" } |
        Array { "[" expr* "]" } |
        Number |
        String
      }
      @skip { space }
      @tokens {
        Number { $[0-9]+ }
        String { "'" ![']* "'" }
        space { $[ \n]+ }
      }
    `).configure({
      bufferLength: 2,
    });

    function testMixed(parser: LRParser) {
      let tree = parser.parse("['x' 100 (['xxx' 20 ('xx')] 'xxx')]")
      let blob1 = tree.resolveInner(2, 1)
      ist(blob1.name, "Blob")
      ist(blob1.from, 2)
      ist(blob1.to, 32)
      let blob2 = tree.resolveInner(12, 1)
      ist(blob2.name, "Blob")
      ist(blob2.from, 12)
      ist(blob2.to, 24)
    }

    testMixed(outer.configure({
      wrap: parseMixed(node => {
        return node.name == "Array" ? {
          parser: blob(),
          overlay: node => node.name == "String" ? {from: node.from + 1, to: node.to - 1} : false
        } : null
      })
    }))

    testMixed(outer.configure({
      wrap: parseMixed(node => {
        if (node.name != "Array") return null
        let ranges: {from: number, to: number}[] = []
        let scan = (node: SyntaxNode) => {
          if (node.name == "String") ranges.push({from: node.from + 1, to: node.to - 1})
          else for (let ch = node.firstChild; ch; ch = ch.nextSibling) if (ch.name != "Array") scan(ch)
        }
        scan(node.node)
        return {parser: blob(), overlay: ranges}
      })
    }))
  })

  it("re-parses cut-off inner parses even if the outer tree was finished", () => {
    let inner = buildParser(`@top Phrase { "<" ch* ">" } @tokens { ch { ![>] } }`).configure({bufferLength: 2})
    let parser = buildParser(`
      @top Doc { Section* }
      Section { "{" SectionContent? "}" }
      @tokens { SectionContent { ![}]+ } }
    `).configure({
      bufferLength: 2,
      wrap: parseMixed(node => node.name == "SectionContent" ? {parser: inner} : null)
    })
    let input = `{<${"x".repeat(100)}>}{<xxxx>}`, tree1
    let parse = parser.startParse(input)
    while (parse.parsedPos < 50) parse.advance()
    parse.stopAt(parse.parsedPos)
    while (!(tree1 = parse.advance())) {}
    ist(tree1.toString(), "Doc(Section(Phrase(⚠)),Section(Phrase(⚠)))")
    let tree2 = parser.parse(input, TreeFragment.addTree(tree1))
    ist(tree2.toString(), "Doc(Section(Phrase),Section(Phrase))")
  })
})
