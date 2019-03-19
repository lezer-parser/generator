import {expression, GrammarDeclaration, RuleDeclaration, PrecDeclaration, Identifier, Expression} from "./node"

export function parseGrammar(file: string, fileName?: string) {
  return parseTop(new Input(file, fileName))
}

const wordChar = /[\w_$]/

class Input {
  type = "sof"
  value: any = null
  start = 0
  end = 0
  lastEnd = 0
  
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
        return {line, ch: pos - cur, fileName: this.fileName}
      }
    }
  }

  raise(msg: string, pos: number) {
    let info = this.lineInfo(pos)
    throw new SyntaxError(`${msg} (${info.fileName ? info.fileName + " " : ""}${info.line}:${info.ch})`)
  }

  match(pos: number, re: RegExp) {
    let match = re.exec(this.string.slice(pos))
    return match ? pos + match[0].length : -1
  }

  next() {
    this.lastEnd = this.end
    let start = this.match(this.end, /^(\s|\/\/.*|\/\*[^]*?\*\/)*/)
    if (start == this.string.length) return this.set("eof", null, start, start)

    let next = this.string[start]
    if (next == '"') {
      let end = this.match(start + 1, /^(\\.|[^"])*"/)
      if (end == -1) this.raise("Unterminated string literal", start)
      return this.set("string", JSON.parse(this.string.slice(start, end)), start, end)
    } else if (next == "'") {
      let end = this.match(start + 1, /^(\\.|[^'])*'/)
      if (end == -1) this.raise("Unterminated string literal", start)
      return this.set("string", JSON.parse(this.string.slice(start, end)), start, end)
    } else if (/[()&~!\-+*?{}<>\.,=|]/.test(next)) {
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
}

function parseTop(input: Input) {
  let start = input.start, rules: RuleDeclaration[] = [], precs: PrecDeclaration[] = []

  while (input.type != "eof") {
    if (input.eat("id", "tokens")) {
      input.expect("{")
      while (!input.eat("}"))
        rules.push(parseRule(input, true))
    } else if (input.type == "id" && input.value == "prec") {
      precs.push(parsePrec(input))
    } else {
      rules.push(parseRule(input, false))
    }
  }
  return new GrammarDeclaration(start, input.lastEnd, rules)
}

function parseRule(input: Input, isToken: boolean) {
  let id = parseIdent(input), params: Identifier[] = [], assoc: null | "left" | "right" = null
  let start = input.start

  if (input.eat("<")) while (!input.eat(">")) {
    if (params.length) input.expect(",")
    params.push(parseIdent(input))
  }
  for (;;) {
    if (input.eat("id", "left") && assoc == null) assoc = "left"
    else if (input.eat("id", "right") && assoc == null) assoc = "right"
    else break
  }
  input.expect("{")
  let expr = parseExprChoice(input)
  input.expect("}")
  return new RuleDeclaration(start, input.lastEnd, isToken, id, params, expr)
}

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
    if (value.length == 1 && input.eat("-")) {
      if (input.type != "string" || input.value.length != 1) input.unexpected()
      let to = input.value
      input.next()
      return expression.characterRange(value, to, start, input.lastEnd)
    } else {
      if (value.length == 0) return expression.sequence([], start, input.lastEnd)
      return expression.literal(value, start, input.lastEnd)
    }
  } else if (input.eat("id", "_")) {
    return expression.any(start, input.lastEnd)
  } else if (input.eat("id", "prec")) {
    let id = parseIdent(input)
    input.expect(".")
    let value = parseIdent(input)
    let expr = parseExprSuffix(input)
    return expression.prec(id, value, expr, start, input.lastEnd)
  } else {
    let id = parseIdent(input)
    let args = []
    if (input.eat("<")) while (!input.eat(">")) {
      if (args.length) input.expect(",")
      args.push(parseExprChoice(input))
    }
    return expression.named(id, args, start, input.lastEnd)
  }
}

function parseExprSuffix(input: Input): Expression {
  let start = input.start
  let expr = parseExprInner(input), kind = input.type
  if (kind == "*" || kind == "?" || kind == "+") {
    input.next()
    return expression.repeat(expr, kind, start, input.lastEnd)
  }
  return expr
}

function endOfSequence(input: Input) {
  return input.type == "}" || input.type == ")" || input.type == "|" || input.type == "/" ||
    input.type == "/\\" || input.type == "{" || input.type == ","
}

function parseExprSequence(input: Input) {
  let start = input.start, first = parseExprSuffix(input)
  if (endOfSequence(input)) return first
  let exprs: Expression[] = [first]
  do { exprs.push(parseExprSuffix(input)) }
  while (!endOfSequence(input))
  return expression.sequence(exprs, start, input.lastEnd)
}

function parseExprChoice(input: Input) {
  let start = input.start, left = parseExprSequence(input)
  if (!input.eat("|")) return left
  let exprs: Expression[] = [left]
  do { exprs.push(parseExprSequence(input)) }
  while (input.eat("|"))
  return expression.choice(exprs, start, input.lastEnd)
}

function parseIdent(input: Input) {
  if (input.type != "id") input.unexpected()
  let start = input.start, name = input.value
  input.next()
  return expression.identifier(name, start, input.lastEnd)
}

function parsePrec(input: Input) {
  let start = input.start
  input.next()
  let baseAssoc: ("left" | "right" | null) = input.eat("id", "left") ? "left" : input.eat("id", "right") ? "right" : null
  let id = parseIdent(input)
  input.expect("{")
  let assoc: ("left" | "right" | null)[] = [], names = []
  while (!input.eat("}")) {
    if (names.length) input.expect(",")
    assoc.push(input.eat("id", "left") ? "left" : input.eat("id", "right") ? "right" : baseAssoc)
    names.push(parseIdent(input))
  }
  return new PrecDeclaration(start, input.lastEnd, id, assoc, names)
}
      
