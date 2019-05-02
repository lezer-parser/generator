import {buildParser} from ".."
import {Parser, InputStream, StringStream, Tokenizer} from "lezer"
const ist = require("ist")

let fs = require("fs"), path = require("path")
let caseDir = path.join(__dirname, "cases")

function compressAST(ast: string, file: string) {
  let token = /\s*($|[(),]|"(?:\\.|[^"])*"|[\p{Alphabetic}\d_$⚠]+)/gyu
  let result = ""
  for (;;) {
    let m = token.exec(ast)
    if (!m) throw new Error("Invalid AST spec in " + file)
    if (!m[1]) break
    result += m[1]
  }
  return result
}

function externalTokenizer(name: string, terms: {[name: string]: number}) {
  if (name == "ext1") return new Tokenizer((input: InputStream) => {
    let next = input.next()
    if (next == "{".charCodeAt(0)) input.accept(terms.braceOpen)
    else if (next == "}".charCodeAt(0)) input.accept(terms.braceClose)
    else if (next == ".".charCodeAt(0)) input.accept(terms.dot)
  })
  throw new Error("Undefined external tokenizer " + name)
}

describe("Cases", () => {
  for (let file of fs.readdirSync(caseDir)) {
    let match = /^(.*)\.txt$/.exec(file)
    if (!match) continue
    let name = match[1], fileName = path.join(caseDir, file)
    let content = fs.readFileSync(fileName, "utf8")
    let parts = content.split(/\n---+\n/), grammarText = parts.shift()
    let parser: Parser | null = null
    let force = () => {
      if (!parser) parser = buildParser(grammarText, {fileName, externalTokenizer})
      return parser
    }

    let expectedErr = /\/\/! (.*)/.exec(grammarText)
    if (expectedErr) it(`case ${name}`, () => {
      ist.throws(force, (e: Error) => e.message.toLowerCase().indexOf(expectedErr![1].trim().toLowerCase()) >= 0)
    })

    if (parts.length == 0 && !expectedErr)
      throw new Error("Test with neither expected errors nor input cases (" + file + ")")

    for (let i = 0; i < parts.length; i++) it(`case ${name}:${i + 1}`, () => {
      let [text, ast] = parts[i].split(/\n==+>/)
      if (!ast) throw new Error(`Missing syntax tree in ${name}:${i + 1}`)
      let expected = compressAST(ast, file)
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
  }
})
