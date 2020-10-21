import {buildParser} from ".."
import {Parser, ExternalTokenizer, InputStream, Token, NodeProp} from "lezer"
// @ts-ignore
import {fileTests} from "../dist/test.cjs"

const ist = require("ist")

let fs = require("fs"), path = require("path")
let caseDir = path.join(__dirname, "cases")

function externalTokenizer(name: string, terms: {[name: string]: number}) {
  if (name == "ext1") return new ExternalTokenizer((input: InputStream, token: Token) => {
    let pos = token.start
    let next = input.get(pos++)
    if (next == "{".charCodeAt(0)) token.accept(terms.braceOpen, pos)
    else if (next == "}".charCodeAt(0)) token.accept(terms.braceClose, pos)
    else if (next == ".".charCodeAt(0)) token.accept(terms.Dot, pos)
  })
  throw new Error("Undefined external tokenizer " + name)
}

function externalSpecializer(name: string, terms: {[name: string]: number}) {
  if (name == "spec1") return (value: string) => value == "one" ? terms.one : value == "two" ? terms.two : -1
  throw new Error("Undefined external specialize " + name)
}

function externalProp() {
  return NodeProp.string()
}

describe("Cases", () => {
  for (let file of fs.readdirSync(caseDir)) {
    let match = /^(.*)\.txt$/.exec(file)
    if (!match) continue
    let name = match[1], fileName = path.join(caseDir, file)
    let content = fs.readFileSync(fileName, "utf8")
    let grammar = /^([^]*?)($|\n# )/.exec(content)!
    content = content.slice(grammar[1].length)
    let parser: Parser | null = null
    let force = () => {
      if (!parser) parser = buildParser(grammar[1], {
        fileName,
        externalTokenizer,
        externalSpecializer,
        externalProp,
        warn(msg) { throw new Error(msg) }
      })
      return parser
    }

    let expectedErr = /\/\/! (.*)/.exec(grammar[1])
    let noCases = !/\S/.test(content)
    if (noCases && !expectedErr)
      throw new Error("Test with neither expected errors nor input cases (" + file + ")")

    if (expectedErr) it(`${name} fails`, () => {
      ist.throws(force, (e: Error) => e.message.toLowerCase().indexOf(expectedErr![1].trim().toLowerCase()) >= 0)
    })
    if (!noCases)
      for (let {name: n, run} of fileTests(content, file))
        it(name + "/" + n, () => run(force()))
  }
})
