import {buildGrammar} from "../src/grammar/build"
import {Grammar} from "../src/grammar/grammar"
import {parse} from "../src/parse"
const ist = require("ist")

let print = process.env.LOG || ""
let printTokens = /\btokens\b/.test(print)
let printSkip = /\bskip\b/.test(print)
let printGrammar = /\bgrammar\b/.test(print)
let printLR = /\blr\b/.test(print)
let printParse = /\bparse\b/.test(print)

let fs = require("fs"), path = require("path")
let caseDir = path.join(__dirname, "cases")

function compressAST(ast: string, file: string) {
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

class Delay {
  _grammar: Grammar | null = null

  constructor(readonly fileName: string, readonly grammarText: string, readonly caseText: string[]) {}

  get grammar() { return this._grammar || buildGrammar(this.grammarText, this.fileName) }
}

describe("Cases", () => {
  for (let file of fs.readdirSync(caseDir)) {
    let name = /^[^\.]*/.exec(file)[0]
    let content = fs.readFileSync(path.join(caseDir, file), "utf8")
    let parts = content.split(/\n---+\n/), grammarText = parts.shift()
    let grammar: Grammar | null = null
    let force = () => {
      if (!grammar) {
        grammar = buildGrammar(grammarText, file)
        if (printSkip || printTokens) {
          let seen: any[] = []
          for (let tokens of grammar.tokenTable) {
            for (let tokenizer of tokens) {
              if (!seen.includes(tokenizer)) {
                if (printSkip && tokenizer.skip) console.log(tokenizer.skip.toString())
                if (printTokens) console.log(tokenizer.startState.toString())
                seen.push(tokenizer)
              }
            }
          }
        }
        if (printGrammar) console.log(grammar.rules.join("\n"))
        if (printLR) console.log(grammar.table.join("\n"))
      }
      return grammar
    }

    let expectedErr = /\/\/! (.*)/.exec(grammarText)
    if (expectedErr) it(`case ${name}`, () => {
      ist.throws(force, e => e.message.toLowerCase().indexOf(expectedErr[1].trim().toLowerCase()) >= 0)
    })

    if (parts.length == 0 && !expectedErr)
      throw new Error("Test with neither expected errors nor input cases (" + file + ")")

    for (let i = 0; i < parts.length; i++) it(`case ${name}:${i + 1}`, () => {
      let [text, ast] = parts[i].split(/\n==+>/)
      if (!ast) throw new Error(`Missing syntax tree in ${name}:${i + 1}`)
      let expected = compressAST(ast, file)
      let strict = expected.indexOf("⚠") < 0
      let parsed = parse(text.trim(), force(), {verbose: printParse, strict}).toString()
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
