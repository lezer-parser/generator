import {GrammarDeclaration, RuleDeclaration, PrecDeclaration, TokenGroupDeclaration,
        Identifier, Expression,
        NamedExpression, ChoiceExpression, SequenceExpression, LiteralExpression,
        RepeatExpression, CharacterRangeExpression, AnyExpression} from "./node"

const wordChar = /[\w_$]/ // FIXME international

export class Input {
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

  parse() {
    return parseTop(this)
  }
}

function parseTop(input: Input) {
  let start = input.start
  let rules: RuleDeclaration[] = []
  let precs: PrecDeclaration[] = []
  let tokenGroups: TokenGroupDeclaration[] = []

  while (input.type != "eof") {
    if (input.type == "id" && input.value == "tokens")
      tokenGroups.push(parseTokenGroup(input))
    else if (input.type == "id" && input.value == "prec")
      precs.push(parsePrec(input))
    else
      rules.push(parseRule(input))
  }
  return new GrammarDeclaration(start, input.lastEnd, rules, tokenGroups, precs)
}

function parseRule(input: Input) {
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
  return new RuleDeclaration(start, input.lastEnd, id, params, expr)
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
    if (value.length == 1 && input.eat("-")) { // FIXME astral chars
      if (input.type != "string" || input.value.length != 1) input.unexpected()
      let to = input.value
      input.next()
      return new CharacterRangeExpression(start, input.lastEnd, value, to)
    } else {
      if (value.length == 0) return new SequenceExpression(start, input.lastEnd, [])
      return new LiteralExpression(start, input.lastEnd, value)
    }
  } else if (input.eat("id", "_")) {
    return new AnyExpression(start, input.lastEnd)
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
    return new NamedExpression(start, input.lastEnd, namespace, id, args)
  }
}

function parseExprSuffix(input: Input): Expression {
  let start = input.start
  let expr = parseExprInner(input), kind = input.type
  if (kind == "*" || kind == "?" || kind == "+") {
    input.next()
    return new RepeatExpression(start, input.lastEnd, expr, kind)
  }
  return expr
}

function endOfSequence(input: Input) {
  return input.type == "}" || input.type == ")" || input.type == "|" || input.type == "/" ||
    input.type == "/\\" || input.type == "{" || input.type == "," || input.type == ">"
}

function parseExprSequence(input: Input) {
  let start = input.start, first = parseExprSuffix(input)
  if (endOfSequence(input)) return first
  let exprs: Expression[] = [first]
  do { exprs.push(parseExprSuffix(input)) }
  while (!endOfSequence(input))
  return new SequenceExpression(start, input.lastEnd, exprs)
}

function parseExprChoice(input: Input) {
  let start = input.start, left = parseExprSequence(input)
  if (!input.eat("|")) return left
  let exprs: Expression[] = [left]
  do { exprs.push(parseExprSequence(input)) }
  while (input.eat("|"))
  return new ChoiceExpression(start, input.lastEnd, exprs)
}

function parseIdent(input: Input) {
  if (input.type != "id") input.unexpected()
  let start = input.start, name = input.value
  input.next()
  return new Identifier(start, input.lastEnd, name)
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
      
function parseTokenGroup(input: Input) {
  let start = input.start
  input.next()
  input.expect("{")
  let tokenRules: RuleDeclaration[] = []
  while (!input.eat("}"))
    tokenRules.push(parseRule(input))
  return new TokenGroupDeclaration(start, input.lastEnd, tokenRules)
}
