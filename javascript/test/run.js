const {Parser, StringStream, ExternalTokenizer} = require("lezer")
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

const newline = [10, 13, 8232, 8233]
const space = [9, 11, 12, 32, 133, 160, 5760, 8192, 8193, 8194, 8195, 8196, 8197, 8198, 8199, 8200, 8201, 8202, 8239, 8287, 12288]

// FIXME this should technically enter block comments
function newlineBefore(input, pos) {
  for (let i = pos - 1; i >= 0; i--) {
    let prev = input.peek(i)
    if (newline.includes(prev)) return true
    if (!space.includes(prev)) break
  }
  return false
}

const slash = "/".charCodeAt(0)

// FIXME also doesn't do block comments yet
function newlineAfter(input, pos) {
  for (let i = pos; i < input.length; i++) {
    let next = input.peek(i)
    if (newline.includes(next)) return true
    if (next == slash && input.peek(i + 1) == slash) return true
    if (!space.includes(next)) break
  }
  return false
}

function externalTokenizer(name, terms) {
  const brace = "}".charCodeAt(0), semicolon = ";".charCodeAt(0)
  if (name == "insertSemicolon") {
    return new ExternalTokenizer((input, stack) => {
      let start = input.pos, next = input.next()
      if ((next == brace || next == -1 || newlineBefore(input, start)) && stack.canShift(terms.insertSemi))
        input.accept(terms.insertSemi, start)
    })
  } else if (name == "noSemicolon") {
    return new ExternalTokenizer((input, stack) => {
      let start = input.pos, next = input.next()
      if (next != brace && next != semicolon && next != -1 &&
          !(newlineAfter(input, start) || newlineBefore(input, start)) && stack.canShift(terms.noSemi))
        input.accept(terms.noSemi, start)
    })
  } else if (name == "postfix") {
    const plus = "+".charCodeAt(0), minus = "-".charCodeAt(0)
    return new ExternalTokenizer((input, stack) => {
      let next = input.next()
      if ((next == plus || next == minus) && next == input.next() &&
          !newlineBefore(input, input.pos - 2) && stack.canShift(terms.postfixOp))
        input.accept(terms.postfixOp)
    })
  } else if (name == "template") {
    const [dollar, backtick, backslash, brace] = ["$", "`", "\\", "{"].map(ch => ch.charCodeAt(0))
    return new ExternalTokenizer((input, stack) => {
      let start = input.pos, afterDollar = false
      for (;;) {
        let next = input.next()
        if (next < 0) {
          if (input.pos > start) input.accept(terms.templateContent, input.pos)
          break
        } else if (next == backtick) {
          if (input.pos == start + 1) input.accept(terms.templateEnd)
          else input.accept(terms.templateContent, input.pos - 1)
          break
        } else if (next == brace && afterDollar) {
          if (input.pos == start + 2) input.accept(terms.templateDollarBrace)
          else input.accept(terms.templateContent, input.pos - 2)
          break
        } else if (next == backslash) {
          input.next()
        }
        afterDollar = next == dollar
      }
    })
  } else {
    throw new Error("Unexpected external tokenizer name " + name)
  }
}

let parser = null

let force = () => {
  if (!parser) {
    let text = fs.readFileSync(path.join(__dirname, "../src/javascript.grammar"), "utf8")
    parser = buildParser(text, {fileName: "javascript.grammar", externalTokenizer})
  }
  return parser
}

for (let file of fs.readdirSync(caseDir)) {
  if (!/\.txt$/.test(file)) continue
  let name = /^[^\.]*/.exec(file)[0]
  let content = fs.readFileSync(path.join(caseDir, file), "utf8")
  let caseExpr = /#\s*(.*)\n([^]*?)==+>([^]*?)\n+(?=#|$)/gy
  describe(file.replace(/\.txt/, ""), () => {
    for (;;) {
      let m = caseExpr.exec(content)
      if (!m) throw new Error("Unexpected file format in " + file)
      it(m[1], () => {
        let text = m[2].trim(), expected = compressAST(m[3], file)
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
  })
}

