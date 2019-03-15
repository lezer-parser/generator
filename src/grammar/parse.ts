export function parseGrammar(file: string, fileName?: string) {
  return parseTop(new Input(file, fileName))
}

export interface Node {
  type: string,
  start: number,
  end: number
}

export interface GrammarDeclaration extends Node {
  type: "GrammarDeclaration"
  rules: {[name: string]: RuleDeclaration}
}

export interface RuleDeclaration extends Node {
  type: "RuleDeclaration"
  isToken: boolean
  id: Identifier
  params: Identifier[]
  expr: Expression
}

export interface Identifier extends Node {
  type: "Identifier"
  name: string
}

export interface Expression extends Node {}

export interface NamedExpression extends Expression {
  type: "NamedExpression"
  id: Identifier
  arguments: Expression[]
}

export interface ChoiceExpression extends Expression {
  type: "ChoiceExpression"
  exprs: Expression[]
}

export interface SequenceExpression extends Expression {
  type: "SequenceExpression"
  exprs: Expression[]
}

export interface RepeatExpression extends Expression {
  type: "RepeatExpression"
  expr: Expression,
  kind: string
}

export interface LiteralExpression extends Expression {
  type: "LiteralExpression"
  value: string
}

export interface CharacterRangeExpression extends Expression {
  type: "CharacterRangeExpression"
  from: string
  to: string
}

export interface AnyExpression extends Expression {
  type: "AnyExpression"
}

class BaseNode implements Node {
  constructor(public type: string,
              public start: number,
              public end: number = start) {}

  static make<T>(type: string, start: number, end: number, props: Partial<T> | null): T {
    let node = new BaseNode(type, start, end) as any as T
    if (props) for (let prop in props) (node as any)[prop] = props[prop]
    return node
  }
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
    } else if (/[()|&~!\-+*?{}<>\.,=]/.test(next)) {
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

  node<T = Node>(start: number, type: string = "", props: Partial<T> = null as any as T, end = this.end) {
    return BaseNode.make<T>(type, start, end, props)
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
}

function parseTop(input: Input) {
  let start = input.start, rules: {[name: string]: RuleDeclaration} = Object.create(null)

  while (input.type != "eof") {
    if (input.eat("id", "tokens")) {
      if (!input.eat("{")) input.unexpected()
      while (!input.eat("}"))
        parseRule(input, rules, true)
    } else {
      parseRule(input, rules, false)
    }
  }
  return input.node<GrammarDeclaration>(start, "GrammarDeclaration", {rules})
}

function parseRule(input: Input, rules: {[name: string]: RuleDeclaration}, isToken: boolean) {
  let id = parseIdent(input), params: Identifier[] = []
  let start = input.start
  if (id.name in rules)
    input.raise(`Duplicate rule declaration '${id.name}'`, id.start)

  if (input.eat("<")) while (!input.eat(">")) {
    if (params.length && !input.eat(",")) input.unexpected()
    params.push(parseIdent(input))
  }
  if (!input.eat("{")) input.unexpected()
  let expr = parseExprChoice(input)
  if (!input.eat("}")) input.unexpected()
  return rules[id.name] = input.node<RuleDeclaration>(start, "RuleDeclaration",
                                                      {isToken, id, params, expr})
}

function parseExprInner(input: Input): Expression {
  let start = input.start
  if (input.eat("(")) {
    let expr = parseExprChoice(input)
    if (!input.eat(")")) input.unexpected()
    return expr
  }

  if (input.type == "string") {
    let value = input.value
    input.next()
    if (value.length == 1 && input.eat("-")) {
      if (input.type != "string" || input.value.length != 1) input.unexpected()
      let to = input.value
      input.next()
      return input.node<CharacterRangeExpression>(start, "CharacterRange", {from: value, to})
    } else {
      if (value.length == 0) return input.node<SequenceExpression>(start, "SequenceExpression", {exprs: []})
      return input.node<LiteralExpression>(start, "LiteralExpression", {value})
    }
  } else if (input.eat("id", "_")) {
    return input.node<AnyExpression>(start, "AnyExpression")
  } else {
    let id = parseIdent(input)
    let args = []
    if (input.eat("<")) while (!input.eat(">")) {
      if (args.length && !input.eat(",")) input.unexpected()
      args.push(parseExprChoice(input))
    }
    return input.node<NamedExpression>(start, "NamedExpression", {arguments: args, id})
  }
}

function parseExprSuffix(input: Input): Expression {
  let start = input.start
  let expr = parseExprInner(input), kind = input.type
  if (kind == "*" || kind == "?" || kind == "+") {
    input.next()
    return input.node<RepeatExpression>(start, "RepeatExpression", {expr, kind})
  }
  return expr
}

function endOfSequence(input: Input) {
  return input.type == "}" || input.type == ")" || input.type == "|" || input.type == "{" || input.type == ","
}

function parseExprSequence(input: Input) {
  let start = input.start, first = parseExprSuffix(input)
  if (endOfSequence(input)) return first
  let exprs: Expression[] = []
  do { exprs.push(parseExprSuffix(input)) }
  while (!endOfSequence(input))
  return input.node<SequenceExpression>(start, "SequenceExpression", {exprs})
}

function parseExprChoice(input: Input) {
  let start = input.start, left = parseExprSequence(input)
  if (!input.eat("|")) return left
  let exprs: Expression[] = []
  do { exprs.push(parseExprSequence(input)) }
  while (input.eat("|"))
  return input.node<ChoiceExpression>(start, "ChoiceExpression", {exprs})
}

function parseIdent(input: Input) {
  if (input.type != "id") input.unexpected()
  let start = input.start, name = input.value
  input.next()
  return input.node<Identifier>(start, "Identifier", {name})
}
