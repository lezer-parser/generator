import {buildGrammar} from "../src/grammar/build"
import {parse} from "../src/parse"

let filter = null, filterN = -1, printTokens = false, printSkip = false, printGrammar = false, printLR = false, printParse = false

for (let i = 2; i < process.argv.length; i++) {
  let arg = process.argv[i]
  if (arg == "--tokens") printTokens = true
  else if (arg == "--skip") printSkip = true
  else if (arg == "--grammar") printGrammar = true
  else if (arg == "--lr") printLR = true
  else if (arg == "--parse") printParse = true
  else if (filter) filterN = +arg
  else filter = arg
}

let fs = require("fs"), path = require("path")
let caseDir = path.join(__dirname, "cases")

let cases = 0, failures = 0

function fail(message: string, file: string, n: number = -1) {
  failures++
  console.log(`${file}${n > -1 ? "#" + n : ""}: ${message}`)
}

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

for (let file of fs.readdirSync(caseDir)) {
  if (filter && file.indexOf(filter) < 0) continue
  let content = fs.readFileSync(path.join(caseDir, file), "utf8")
  let parts = content.split(/\n---+\n/)

  let expectedErr = /\/\/! (.*)/.exec(parts[0]), grammar
  if (expectedErr) cases++
  try {
    grammar = buildGrammar(parts[0], file)
  } catch (e) {
    if (!(e instanceof SyntaxError)) console.log(e.stack)
    if (!expectedErr)
      fail(e.message, file)
    else if (e.message.toLowerCase().indexOf(expectedErr[1].trim().toLowerCase()) < 0)
      fail("Wrong error raised: " + e.message, file)
    continue
  }
  if (expectedErr) {
    fail("Failed to raise expected grammar error", file)
    continue
  }

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

  if (parts.length == 1) throw new Error("Test with neither expected errors nor input cases (" + file + ")")
  for (let i = 1; i < parts.length; i++) {
    if (filterN > -1 && filterN != i) continue
    let [text, ast] = parts[i].split(/\n==+>/)
    cases++
    if (!ast) {
      fail("Missing syntax tree", file, i)
      continue
    }
    let expected = compressAST(ast, file), parsed
    let strict = expected.indexOf("⚠") < 0
    try {
      parsed = parse(text.trim(), grammar, undefined, printParse, strict).toString()
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
