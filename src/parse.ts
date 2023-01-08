import {GrammarDeclaration, RuleDeclaration, PrecDeclaration,
        TokenPrecDeclaration, TokenConflictDeclaration, TokenDeclaration, LocalTokenDeclaration,
        LiteralDeclaration, ContextDeclaration, ExternalTokenDeclaration, ExternalPropSourceDeclaration,
        ExternalSpecializeDeclaration, ExternalPropDeclaration, Identifier,
        Expression, NameExpression, ChoiceExpression, SequenceExpression, LiteralExpression,
        RepeatExpression, SetExpression, InlineRuleExpression, Prop, PropPart,
        SpecializeExpression, AnyExpression, ConflictMarker, CharClasses, CharClass} from "./node"
import {GenError} from "./error"

// Note that this is the parser for grammar files, not the generated parser

let word = /[\w_-]+/gy
// Some engines (specifically SpiderMonkey) have still not implemented \p
try { word = /[\p{Alphabetic}\d_-]+/ugy } catch (_) {}

const none: readonly any[] = []

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

  message(msg: string, pos: number = -1): string {
    let posInfo = this.fileName || ""
    if (pos > -1) {
      let info = this.lineInfo(pos)
      posInfo += (posInfo ? " " : "") + info.line + ":" + info.ch
    }
    return posInfo ? msg + ` (${posInfo})` : msg
  }

  raise(msg: string, pos: number = -1): never {
    throw new GenError(this.message(msg, pos))
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
      let end = this.match(start + 1, /^(\\.|[^"\\])*"/)
      if (end == -1) this.raise("Unterminated string literal", start)
      return this.set("string", readString(this.string.slice(start + 1, end - 1)), start, end)
    } else if (next == "'") {
      let end = this.match(start + 1, /^(\\.|[^'\\])*'/)
      if (end == -1) this.raise("Unterminated string literal", start)
      return this.set("string", readString(this.string.slice(start + 1, end - 1)), start, end)
    } else if (next == "@") {
      word.lastIndex = start + 1
      let m = word.exec(this.string)
      if (!m) return this.raise("@ without a name", start)
      return this.set("at", m[0], start, start + 1 + m[0].length)
    } else if ((next == "$" || next == "!") && this.string[start + 1] == "[") {
      let end = this.match(start + 2, /^(?:\\.|[^\]\\])*\]/)
      if (end == -1) this.raise("Unterminated character set", start)
      return this.set("set", this.string.slice(start + 2, end - 1), start, end)
    } else if (/[\[\]()!~+*?{}<>\.,|:$=]/.test(next)) {
      return this.set(next, null, start, start + 1)
    } else {
      word.lastIndex = start
      let m = word.exec(this.string)
      if (!m) return this.raise("Unexpected character " + JSON.stringify(next), start)
      return this.set("id", m[0], start, start + m[0].length)
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

  unexpected(): never {
    return this.raise(`Unexpected token '${this.string.slice(this.start, this.end)}'`, this.start)
  }

  expect(type: string, value: any = null) {
    let val = this.value
    if (this.type != type || !(value == null || val === value)) this.unexpected()
    this.next()
    return val
  }

  parse() {
    return parseGrammar(this)
  }
}

function parseGrammar(input: Input) {
  let start = input.start
  let rules: RuleDeclaration[] = []
  let prec: PrecDeclaration | null = null
  let tokens: TokenDeclaration | null = null
  let localTokens: LocalTokenDeclaration[] = []
  let mainSkip: Expression | null = null
  let scopedSkip: {expr: Expression, topRules: readonly RuleDeclaration[], rules: readonly RuleDeclaration[]}[] = []
  let dialects: Identifier[] = []
  let context: ContextDeclaration | null = null
  let external: ExternalTokenDeclaration[] = []
  let specialized: ExternalSpecializeDeclaration[] = []
  let props: ExternalPropDeclaration[] = []
  let propSources: ExternalPropSourceDeclaration[] = []
  let tops: RuleDeclaration[] = []
  let sawTop = false
  let autoDelim = false

  while (input.type != "eof") {
    let start = input.start
    if (input.eat("at", "top")) {
      if (input.type as any != "id") input.raise(`Top rules must have a name`, input.start)
      tops.push(parseRule(input, parseIdent(input)))
      sawTop = true
    } else if (input.type == "at" && input.value == "tokens") {
      if (tokens) input.raise(`Multiple @tokens declaractions`, input.start)
      else tokens = parseTokens(input)
    } else if (input.eat("at", "local")) {
      input.expect("id", "tokens")
      localTokens.push(parseLocalTokens(input, start))
    } else if (input.eat("at", "context")) {
      if (context) input.raise(`Multiple @context declarations`, start)
      let id = parseIdent(input)
      input.expect("id", "from")
      let source = input.expect("string")
      context = new ContextDeclaration(start, id, source)
    } else if (input.eat("at", "external")) {
      if (input.eat("id", "tokens")) external.push(parseExternalTokens(input, start))
      else if (input.eat("id", "prop")) props.push(parseExternalProp(input, start))
      else if (input.eat("id", "extend")) specialized.push(parseExternalSpecialize(input, "extend", start))
      else if (input.eat("id", "specialize")) specialized.push(parseExternalSpecialize(input, "specialize", start))
      else if (input.eat("id", "propSource")) propSources.push(parseExternalPropSource(input, start))
      else input.unexpected()
    } else if (input.eat("at", "dialects")) {
      input.expect("{")
      for (let first = true; !input.eat("}"); first = false) {
        if (!first) input.eat(",")
        dialects.push(parseIdent(input))
      }
    } else if (input.type == "at" && input.value == "precedence") {
      if (prec) input.raise(`Multiple precedence declarations`, input.start)
      prec = parsePrecedence(input)
    } else if (input.eat("at", "detectDelim")) {
      autoDelim = true
    } else if (input.eat("at", "skip")) {
      let skip = parseBracedExpr(input)
      if (input.type == "{") {
        input.next()
        let rules = [], topRules = []
        while (!input.eat("}")) {
          if (input.eat("at", "top")) {
            topRules.push(parseRule(input, parseIdent(input)))
            sawTop = true
          } else {
            rules.push(parseRule(input))
          }
        }
        scopedSkip.push({expr: skip, topRules, rules})
      } else {
        if (mainSkip) input.raise(`Multiple top-level skip declarations`, input.start)
        mainSkip = skip
      }
    } else {
      rules.push(parseRule(input))
    }
  }
  if (!sawTop) return input.raise(`Missing @top declaration`)
  return new GrammarDeclaration(start, rules, tops, tokens, localTokens, context, external, specialized, propSources,
                                prec, mainSkip, scopedSkip, dialects, props, autoDelim)
}

function parseRule(input: Input, named?: Identifier) {
  let start = named ? named.start : input.start
  let id = named || parseIdent(input)
  let props = parseProps(input)
  let params: Identifier[] = []
  if (input.eat("<")) while (!input.eat(">")) {
    if (params.length) input.expect(",")
    params.push(parseIdent(input))
  }
  let expr = parseBracedExpr(input)
  return new RuleDeclaration(start, id, props, params, expr)
}

function parseProps(input: Input) {
  if (input.type != "[") return none
  let props = []
  input.expect("[")
  while (!input.eat("]")) {
    if (props.length) input.expect(",")
    props.push(parseProp(input))
  }
  return props
}

function parseProp(input: Input) {
  let start = input.start, value = [], name = input.value, at = input.type == "at"
  if (!input.eat("at") && !input.eat("id")) input.unexpected()
  if (input.eat("=")) for (;;) {
    if (input.type == "string" || input.type == "id") {
      value.push(new PropPart(input.start, input.value, null))
      input.next()
    } else if (input.eat(".")) {
      value.push(new PropPart(input.start, ".", null))
    } else if (input.eat("{")) {
      value.push(new PropPart(input.start, null, input.expect("id")))
      input.expect("}")
    } else {
      break
    }
  }
  return new Prop(start, at, name, value)
}

function parseBracedExpr(input: Input): Expression {
  input.expect("{")
  let expr = parseExprChoice(input)
  input.expect("}")
  return expr
}

const SET_MARKER = "\ufdda" // (Invalid unicode character)

function parseExprInner(input: Input): Expression {
  let start = input.start
  if (input.eat("(")) {
    if (input.eat(")"))
      return new SequenceExpression(start, none, [none, none])
    let expr = parseExprChoice(input)
    input.expect(")")
    return expr
  } else if (input.type == "string") {
    let value = input.value
    input.next()
    if (value.length == 0) return new SequenceExpression(start, none, [none, none])
    return new LiteralExpression(start, value)
  } else if (input.eat("id", "_")) {
    return new AnyExpression(start)
  } else if (input.type == "set") {
    let content = input.value, invert = input.string[input.start] == "!"
    let unescaped = readString(content.replace(/\\.|-|"/g, (m: string) => {
      return m == "-" ? SET_MARKER : m == '"' ? '\\"' : m
    }))
    let ranges: [number, number][] = []
    for (let pos = 0; pos < unescaped.length;) {
      let code = unescaped.codePointAt(pos)!
      pos += code > 0xffff ? 2 : 1
      if (pos < unescaped.length - 1 && unescaped[pos] == SET_MARKER) {
        let end = unescaped.codePointAt(pos + 1)!
        pos += end > 0xffff ? 3 : 2
        if (end < code) input.raise("Invalid character range", input.start)
        addRange(input, ranges, code, end + 1)
      } else {
        if (code == SET_MARKER.charCodeAt(0)) code = 45
        addRange(input, ranges, code, code + 1)
      }
    }
    input.next()
    return new SetExpression(start, ranges.sort((a, b) => a[0] - b[0]), invert)
  } else if (input.type == "at" && (input.value == "specialize" || input.value == "extend")) {
    let {start, value} = input
    input.next()
    let props = parseProps(input)
    input.expect("<")
    let token = parseExprChoice(input), content
    if (input.eat(",")) {
      content = parseExprChoice(input)
    } else if (token instanceof LiteralExpression) {
      content = token
    } else {
      input.raise(`@${value} requires two arguments when its first argument isn't a literal string`)
    }
    input.expect(">")
    return new SpecializeExpression(start, value, props, token, content)
  } else if (input.type == "at" && CharClasses.hasOwnProperty(input.value)) {
    let cls = new CharClass(input.start, input.value)
    input.next()
    return cls
  } else if (input.type == "[") {
    let rule = parseRule(input, new Identifier(start, "_anon"))
    if (rule.params.length) input.raise(`Inline rules can't have parameters`, rule.start)
    return new InlineRuleExpression(start, rule)
  } else {
    let id = parseIdent(input)
    if (input.type == "[" || input.type == "{") {
      let rule = parseRule(input, id)
      if (rule.params.length) input.raise(`Inline rules can't have parameters`, rule.start)
      return new InlineRuleExpression(start, rule)
    } else {
      if (input.eat(".") && id.name == "std" && CharClasses.hasOwnProperty(input.value)) {
        let cls = new CharClass(start, input.value)
        input.next()
        return cls
      }
      return new NameExpression(start, id, parseArgs(input))
    }
  }
}

function parseArgs(input: Input) {
  let args = []
  if (input.eat("<")) while (!input.eat(">")) {
    if (args.length) input.expect(",")
    args.push(parseExprChoice(input))
  }
  return args
}

function addRange(input: Input, ranges: [number, number][], from: number, to: number) {
  if (!ranges.every(([a, b]) => b <= from || a >= to))
    input.raise("Overlapping character range", input.start)
  ranges.push([from, to])
}

function parseExprSuffix(input: Input): Expression {
  let start = input.start
  let expr = parseExprInner(input)
  for (;;) {
    let kind = input.type
    if (input.eat("*") || input.eat("?") || input.eat("+"))
      expr = new RepeatExpression(start, expr, kind as "*" | "+" | "?")
    else
      return expr
  }
}

function endOfSequence(input: Input) {
  return input.type == "}" || input.type == ")" || input.type == "|" || input.type == "/" ||
    input.type == "/\\" || input.type == "{" || input.type == "," || input.type == ">"
}

function parseExprSequence(input: Input) {
  let start = input.start, exprs: Expression[] = [], markers = [none]
  do {
    // Add markers at this position
    for (;;) {
      let localStart = input.start, markerType!: "ambig" | "prec"
      if (input.eat("~")) markerType = "ambig"
      else if (input.eat("!")) markerType = "prec"
      else break
      markers[markers.length - 1] =
        markers[markers.length - 1].concat(new ConflictMarker(localStart, parseIdent(input), markerType))
    }
    if (endOfSequence(input)) break
    exprs.push(parseExprSuffix(input))
    markers.push(none)
  } while (!endOfSequence(input))
  if (exprs.length == 1 && markers.every(ms => ms.length == 0)) return exprs[0]
  return new SequenceExpression(start, exprs, markers, !exprs.length)
}

function parseExprChoice(input: Input) {
  let start = input.start, left = parseExprSequence(input)
  if (!input.eat("|")) return left
  let exprs: Expression[] = [left]
  do { exprs.push(parseExprSequence(input)) }
  while (input.eat("|"))
  let empty = exprs.find(s => s instanceof SequenceExpression && s.empty)
  if (empty) input.raise("Empty expression in choice operator. If this is intentional, use () to make it explicit.", empty.start)
  return new ChoiceExpression(start, exprs)
}

function parseIdent(input: Input) {
  if (input.type != "id") input.unexpected()
  let start = input.start, name = input.value
  input.next()
  return new Identifier(start, name)
}

function parsePrecedence(input: Input) {
  let start = input.start
  input.next()
  input.expect("{")
  let items: {id: Identifier, type: "left" | "right" | "cut" | null}[] = []
  while (!input.eat("}")) {
    if (items.length) input.eat(",")
    items.push({
      id: parseIdent(input),
      type: input.eat("at", "left") ? "left" : input.eat("at", "right") ? "right" : input.eat("at", "cut") ? "cut" : null
    })
  }
  return new PrecDeclaration(start, items)
}

function parseTokens(input: Input) {
  let start = input.start
  input.next()
  input.expect("{")
  let tokenRules: RuleDeclaration[] = []
  let literals: LiteralDeclaration[] = []
  let precedences: TokenPrecDeclaration[] = []
  let conflicts: TokenConflictDeclaration[] = []
  while (!input.eat("}")) {
    if (input.type == "at" && input.value == "precedence") {
      precedences.push(parseTokenPrecedence(input))
    } else if (input.type == "at" && input.value == "conflict") {
      conflicts.push(parseTokenConflict(input))
    } else if (input.type == "string") {
      literals.push(new LiteralDeclaration(input.start, input.expect("string"), parseProps(input)))
    } else {
      tokenRules.push(parseRule(input))
    }
  }
  return new TokenDeclaration(start, precedences, conflicts, tokenRules, literals)
}

function parseLocalTokens(input: Input, start: number) {
  input.expect("{")
  let tokenRules: RuleDeclaration[] = []
  let precedences: TokenPrecDeclaration[] = []
  let fallback: {id: Identifier, props: readonly Prop[]} | null = null
  while (!input.eat("}")) {
    if (input.type == "at" && input.value == "precedence") {
      precedences.push(parseTokenPrecedence(input))
    } else if (input.eat("at", "else") && !fallback) {
      fallback = {id: parseIdent(input), props: parseProps(input)}
    } else {
      tokenRules.push(parseRule(input))
    }
  }
  return new LocalTokenDeclaration(start, precedences, tokenRules, fallback)
}

function parseTokenPrecedence(input: Input) {
  let start = input.start
  input.next()
  input.expect("{")
  let tokens: (LiteralExpression | NameExpression)[] = []
  while (!input.eat("}")) {
    if (tokens.length) input.eat(",")
    let expr = parseExprInner(input)
    if (expr instanceof LiteralExpression || expr instanceof NameExpression)
      tokens.push(expr)
    else
      input.raise(`Invalid expression in token precedences`, expr.start)
  }
  return new TokenPrecDeclaration(start, tokens)
}

function parseTokenConflict(input: Input) {
  let start = input.start
  input.next()
  input.expect("{")
  let a = parseExprInner(input)
  if (!(a instanceof LiteralExpression || a instanceof NameExpression))
    input.raise(`Invalid expression in token conflict`, a.start)
  input.eat(",")
  let b = parseExprInner(input)
  if (!(b instanceof LiteralExpression || b instanceof NameExpression))
    input.raise(`Invalid expression in token conflict`, b.start)
  input.expect("}")
  return new TokenConflictDeclaration(start, a, b)
}

function parseExternalTokenSet(input: Input) {
  let tokens: {id: Identifier, props: readonly Prop[]}[] = []
  input.expect("{")
  while (!input.eat("}")) {
    if (tokens.length) input.eat(",")
    let id = parseIdent(input)
    let props = parseProps(input)
    tokens.push({id, props})
  }
  return tokens
}

function parseExternalTokens(input: Input, start: number) {
  let id = parseIdent(input)
  input.expect("id", "from")
  let from = input.expect("string")
  return new ExternalTokenDeclaration(start, id, from, parseExternalTokenSet(input))
}

function parseExternalSpecialize(input: Input, type: "extend" | "specialize", start: number) {
  let token = parseBracedExpr(input)
  let id = parseIdent(input)
  input.expect("id", "from")
  let from = input.expect("string")
  return new ExternalSpecializeDeclaration(start, type, token, id, from, parseExternalTokenSet(input))
}

function parseExternalPropSource(input: Input, start: number) {
  let id = parseIdent(input)
  input.expect("id", "from")
  return new ExternalPropSourceDeclaration(start, id, input.expect("string"))
}

function parseExternalProp(input: Input, start: number) {
  let externalID = parseIdent(input)
  let id = input.eat("id", "as") ? parseIdent(input) : externalID
  input.expect("id", "from")
  let from = input.expect("string")
  return new ExternalPropDeclaration(start, id, externalID, from)
}

function readString(string: string) {
  let point = /\\(?:u\{([\da-f]+)\}|u([\da-f]{4})|x([\da-f]{2})|([ntbrf0])|(.))|[^]/yig
  let out = "", m
  while (m = point.exec(string)) {
    let [all, u1, u2, u3, single, unknown] = m
    if (u1 || u2 || u3)
      out += String.fromCodePoint(parseInt(u1 || u2 || u3, 16))
    else if (single)
      out += single == "n" ? "\n" : single == "t" ? "\t" : single == "0" ? "\0" : single == "r" ? "\r" : single == "f" ? "\f" : "\b"
    else if (unknown)
      out += unknown
    else
      out += all
  }
  return out
}
