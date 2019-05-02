const {Parser, StringStream, Tokenizer} = require("lezer")
const {buildParser} = require("../..")
const ist = require("ist")

let fs = require("fs"), path = require("path")
let caseDir = __dirname

function compressAST(ast, file) {
  let token = /\s*($|[(),]|\"(?:\\.|[^"])*\"|[\w⚠]+)/gy
  let result = ""
  for (;;) {
    let m = token.exec(ast)
    if (!m) throw new Error("Invalid AST spec in " + file)
    if (!m[1]) break
    result += m[1]
  }
  return result
}

function externalTokenizer(name, terms) {
  if (name != "insertSemicolon") throw new Error("Unexpected external tokenizer name " + name)
  const newline = /[\n\u2028\u2029]/, brace = "}".charCodeAt(0)
  return new Tokenizer((input, stack) => {
    let next = input.next()
    if (next == brace || next == -1 || newline.test(input.read(stack.pos, input.pos - 1)))
      input.accept(terms.insertSemi, input.pos - 1)
  })
}

let parser = null

let force = () => {
  if (!parser) {
    let text = fs.readFileSync(path.join(__dirname, "../src/javascript.grammar"), "utf8")
    parser = buildParser(text, {fileName: "javascript.grammar", externalTokenizer})
  }
  return parser
}

describe("JavaScript cases", () => {
  for (let file of fs.readdirSync(caseDir)) {
    if (!/\.txt$/.test(file)) continue
    let name = /^[^\.]*/.exec(file)[0]
    let content = fs.readFileSync(path.join(caseDir, file), "utf8")
    let caseExpr = /#\s*(.*)\n([^]*?)==+>([^]*?)\n+(?=#|$)/gy
    for (;;) {
      let m = caseExpr.exec(content)
      if (!m) throw new Error("Unexpected file format in " + file)
      it(m[1], () => {
        let text = m[2].trim(), expected = compressAST(m[3])
        let strict = expected.indexOf("⚠") < 0, parser = force()
        let result = parser.parse(new StringStream(text.trim()), {strict})
        let parsed = result.toString(parser)
        if (parsed != expected) {
          if (parsed.length > 76) {
            let mis = 0
            while (parsed[mis] == expected[mis]) mis++
            if (mis > 30) {
              parsed = "…" + parsed.slice(mis - 30)
              expected = "…" + expected.slice(mis - 30)
            }
          }
          if (parsed.length > 76) parsed = parsed.slice(0, 75) + "…"
          if (expected.length > 76) expected = expected.slice(0, 75) + "…"
          throw new Error(`Output mismatch, got\n  ${parsed}\nexpected\n  ${expected}`)
        }
      })
      if (m.index + m[0].length == content.length) break
    }
  }
})
