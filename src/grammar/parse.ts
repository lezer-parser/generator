import {GrammarDeclaration, RuleDeclaration, PrecDeclaration, TokenGroupDeclaration,
        Identifier, Expression,
        NamedExpression, ChoiceExpression, SequenceExpression, LiteralExpression,
        RepeatExpression, SetExpression, AnyExpression, MarkedExpression} from "./node"

const wordChar = /[\w_$]/ // FIXME international

export class Input {
  type = "sof"
  value: any = null
  start = 0
  end = 0
  
  constructor(readonly string: string,
              readonly fileName: string | null = null) {
    this.next()
  }

  lineInfo(pos: number) {
    for (let line = 1, cur = 0;;) {
      let next = this.string.indexOf("\n", cur)
      if (next > -1 && next < pos) {
        ++line
        cur = next + 1
      } else {
        return {line, ch: pos - cur}
      }
    }
  }

  raise(msg: string, pos: number = -1): never {
    let posInfo = this.fileName || ""
    if (pos > -1) {
      let info = this.lineInfo(pos)
      posInfo += (posInfo ? " " : "") + info.line + ":" + info.ch
    }
    throw new SyntaxError(posInfo ? msg + ` (${posInfo})` : msg)
  }

  match(pos: number, re: RegExp) {
    let match = re.exec(this.string.slice(pos))
    return match ? pos + match[0].length : -1
  }

  next() {
    let start = this.match(this.end, /^(\s|\/\/.*|\/\*[^]*?\*\/)*/)
    if (start == this.string.length) return this.set("eof", null, start, start)

    let next = this.string[start]
    if (next == '"') {
      let end = this.match(start + 1, /^(\\.|[^"])*"/)
      if (end == -1) this.raise("Unterminated string literal", start)
      return this.set("string", readString(this.string.slice(start, end)), start, end)
    } else if (next == "'") {
      let end = this.match(start + 1, /^(\\.|[^'])*'/)
      if (end == -1) this.raise("Unterminated string literal", start)
      return this.set("string", readString(this.string.slice(start, end)), start, end)
    } else if (next == "[") {
      let end = this.match(start + 1, /^(?:\\.|[^\]])*\]/)
      if (end == -1) this.raise("Unterminated character set", start)
      return this.set("set", this.string.slice(start + 1, end - 1), start, end)
    } else if (/[()!+*?{}<>\.,|]/.test(next)) {
      return this.set(next, null, start, start + 1)
    } else if (wordChar.test(next)) {
      let end = start + 1
      while (end < this.string.length && wordChar.test(this.string[end])) end++
      return this.set("id", this.string.slice(start, end), start, end)
    } else {
      this.raise("Unexpected character " + JSON.stringify(next), start)
    }
  }

  set(type: string, value: any, start: number, end: number) {
    this.type = type
    this.value = value
    this.start = start
    this.end = end
  }

  eat(type: string, value: any = null) {
    if (this.type == type && (value == null || this.value === value)) {
      this.next()
      return true
    } else {
      return false
    }
  }

  unexpected() {
    this.raise(`Unexpected token '${this.string.slice(this.start, this.end)}'`, this.start)
  }

  expect(type: string, value: any = null) {
    if (!this.eat(type, value)) this.unexpected()
  }

  parse() {
    return parseTop(this)
  }
}

function parseTop(input: Input) {
  let start = input.start
  let rules: RuleDeclaration[] = []
  let prec: PrecDeclaration | null = null
  let tokens: TokenGroupDeclaration | null = null

  while (input.type != "eof") {
    if (input.type == "id" && input.value == "tokens") {
      if (tokens) input.raise(`Multiple tokens declaractions`, input.start)
      else tokens = parseTokenGroup(input)
    } else if (input.type == "id" && input.value == "prec") {
      if (prec) input.raise(`Multiple prec declarations`, input.start)
      else prec = parsePrec(input)
    } else {
      rules.push(parseRule(input))
    }
  }
  return new GrammarDeclaration(start, rules, tokens, prec)
}

function parseRule(input: Input) {
  let id = parseIdent(input), params: Identifier[] = []
  let start = input.start

  if (input.eat("<")) while (!input.eat(">")) {
    if (params.length) input.expect(",")
    params.push(parseIdent(input))
  }
  input.expect("{")
  let expr = parseExprChoice(input)
  input.expect("}")
  return new RuleDeclaration(start, id, params, expr)
}

const SET_MARKER = "\ufdda" // (Invalid unicode character)

function parseExprInner(input: Input): Expression {
  let start = input.start
  if (input.eat("(")) {
    let expr = parseExprChoice(input)
    input.expect(")")
    return expr
  }

  if (input.type == "string") {
    let value = input.value
    input.next()
    if (value.length == 0) return new SequenceExpression(start, [])
    return new LiteralExpression(start, value)
  } else if (input.eat("id", "_")) {
    return new AnyExpression(start)
  } else if (input.type == "set") {
    let content = input.value, invert = false
    if (/^\^/.test(content)) {
      invert = true
      content = content.slice(1)
    }
    let unescaped = readString('"' + content.replace(/\\.|-|"/g, (m: string) => {
      return m == "-" ? SET_MARKER : m == '"' ? '\\"' : m
    }) + '"') as string
    let ranges: [number, number][] = []
    function addRange(from: number, to: number) {
      if (!ranges.every(([a, b]) => b <= from || a >= to))
        input.raise("Overlapping character range", input.start)
      ranges.push([from, to])
    }
    for (let pos = 0; pos < unescaped.length;) {
      let code = unescaped.codePointAt(pos)!
      pos += code > 0xffff ? 2 : 1
      if (pos < unescaped.length - 1 && unescaped[pos] == SET_MARKER) {
        let end = unescaped.codePointAt(pos + 1)!
        pos += end > 0xffff ? 3 : 2
        if (end < code) input.raise("Invalid character range", input.start)
        addRange(code, end + 1)
      } else {
        addRange(code, code + 1)
      }
    }
    input.next()
    return new SetExpression(start, ranges.sort((a, b) => a[0] - b[0]), invert)
  } else {
    let id = parseIdent(input), namespace = null
    if (input.eat(".")) {
      namespace = id
      id = parseIdent(input)
    }
    let args = []
    if (input.eat("<")) while (!input.eat(">")) {
      if (args.length) input.expect(",")
      args.push(parseExprChoice(input))
    }
    return new NamedExpression(start, namespace, id, args)
  }
}

function parseExprSuffix(input: Input): Expression {
  let start = input.start
  let expr = parseExprInner(input), kind = input.type
  if (kind == "*" || kind == "?" || kind == "+") {
    input.next()
    return new RepeatExpression(start, expr, kind)
  }
  return expr
}

function endOfSequence(input: Input) {
  return input.type == "}" || input.type == ")" || input.type == "|" || input.type == "/" ||
    input.type == "/\\" || input.type == "{" || input.type == "," || input.type == ">"
}

function parseExprSequence(input: Input) {
  let start = input.start, first = parseExprPrec(input)
  if (endOfSequence(input)) return first
  let exprs: Expression[] = [first]
  do { exprs.push(parseExprPrec(input)) }
  while (!endOfSequence(input))
  return new SequenceExpression(start, exprs)
}

function parseExprPrec(input: Input) {
  let start = input.start
  if (!input.eat("!")) return parseExprSuffix(input)
  let id = parseIdent(input), namespace = null
  if (input.eat(".")) {
    namespace = id
    id = parseIdent(input)
  }
  let expr = parseExprSuffix(input)
  return new MarkedExpression(start, namespace, id, expr)
}

function parseExprChoice(input: Input) {
  let start = input.start, left = parseExprSequence(input)
  if (!input.eat("|")) return left
  let exprs: Expression[] = [left]
  do { exprs.push(parseExprSequence(input)) }
  while (input.eat("|"))
  return new ChoiceExpression(start, exprs)
}

function parseIdent(input: Input) {
  if (input.type != "id") input.unexpected()
  let start = input.start, name = input.value
  input.next()
  return new Identifier(start, name)
}

function parsePrec(input: Input) {
  let start = input.start
  input.next()
  input.expect("{")
  let assoc: ("left" | "right" | null)[] = [], names = []
  while (!input.eat("}")) {
    if (names.length) input.expect(",")
    names.push(parseIdent(input))
    assoc.push(input.eat("id", "left") ? "left" : input.eat("id", "right") ? "right" : null)
  }
  return new PrecDeclaration(start, assoc, names)
}
      
function parseTokenGroup(input: Input) {
  let start = input.start
  input.next()
  input.expect("{")
  let tokenRules: RuleDeclaration[] = [], subGroups: TokenGroupDeclaration[] = []
  while (!input.eat("}")) {
    if (input.type == "id" && input.value == "group") subGroups.push(parseTokenGroup(input))
    else tokenRules.push(parseRule(input))
  }
  return new TokenGroupDeclaration(start, tokenRules, subGroups)
}

function readString(string: string) {
  // Can't use JSON.parse because it has too limited support for
  // escape sequences, can't be bothered to write a custom reader
  return (1,eval)(string)
}
