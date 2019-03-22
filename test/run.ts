import {buildGrammar} from "../src/grammar/build"
import {buildAutomaton} from "../src/grammar/automaton"
import {parse} from "../src/parse"

let filter = process.argv[3], filterN = process.argv[4] ? +process.argv[4] : -1

let fs = require("fs"), path = require("path")
let caseDir = path.join(__dirname, "cases")

let cases = 0, failures = 0

function fail(message: string, file: string, n: number = -1) {
  failures++
  console.log(`${file}${n > -1 ? "#" + n : ""}: ${message}`)
}

function compressAST(ast: string, file: string) {
  let token = /\s*($|[(),]|\"(?:\\.|[^"])*\"|\w+)/gy
  let result = ""
  for (;;) {
    let m = token.exec(ast)
    if (!m) throw new Error("Invalid AST spec in " + file)
    if (!m[1]) break
    result += m[1]
  }
  return result
}

for (let file of fs.readdirSync(caseDir)) {
  if (filter && file.indexOf(filter) < 0) continue
  let content = fs.readFileSync(path.join(caseDir, file), "utf8")
  let parts = content.split(/\n---+\n/)

  let grammar, table
  try {
    grammar = buildGrammar(parts[0], file)
    table = buildAutomaton(grammar)
  } catch (e) {
    fail(e.message, file)
    if (!(e instanceof SyntaxError)) console.log(e.stack)
    continue
  }

  for (let i = 1; i < parts.length; i++) {
    if (filterN > -1 && filterN != i) continue
    let [text, ast] = parts[i].split(/\n==+>/)
    cases++
    if (!ast) {
      fail("Missing syntax tree", file, i)
      continue
    }
    let expected = compressAST(ast, file), parsed
    try {
      parsed = parse(text.trim(), grammar, table)!.toString()
    } catch (e) {
      fail(e.message, file, i)
      if (!(e instanceof SyntaxError)) console.log(e.stack)
      continue
    }
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
      fail(`Output mismatch, got\n  ${parsed}\nexpected\n  ${expected}`, file, i)
    }
  }
}

console.log(`Ran ${cases} tests. ${failures || "No"} failures.`)
process.exit(failures ? 1 : 0)
