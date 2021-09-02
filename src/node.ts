export class Node {
  constructor(readonly start: number) {}
}

export class GrammarDeclaration extends Node {
  constructor(start: number,
              readonly rules: readonly RuleDeclaration[],
              readonly topRules: readonly RuleDeclaration[],
              readonly tokens: TokenDeclaration | null,
              readonly context: ContextDeclaration | null,
              readonly externalTokens: readonly ExternalTokenDeclaration[],
              readonly externalSpecializers: readonly ExternalSpecializeDeclaration[],
              readonly precedences: PrecDeclaration | null,
              readonly mainSkip: Expression | null,
              readonly scopedSkip: readonly {
                expr: Expression,
                topRules: readonly RuleDeclaration[],
                rules: readonly RuleDeclaration[]
              }[],
              readonly dialects: readonly Identifier[],
              readonly externalProps: readonly ExternalPropDeclaration[],
              readonly autoDelim: boolean) {
    super(start)
  }
  toString() { return Object.values(this.rules).join("\n") }
}

export class RuleDeclaration extends Node {
  constructor(start: number,
              readonly id: Identifier,
              readonly props: readonly Prop[],
              readonly params: readonly Identifier[],
              readonly expr: Expression) {
    super(start)
  }
  toString() {
    return this.id.name + (this.params.length ? `<${this.params.join()}>` : "") + " -> " + this.expr
  }
}

export class PrecDeclaration extends Node {
  constructor(start: number,
              readonly items: readonly {id: Identifier, type: "left" | "right" | "cut" | null}[]) {
    super(start)
  }
}

export class TokenPrecDeclaration extends Node {
  constructor(start: number,
              readonly items: readonly (NameExpression | LiteralExpression)[]) {
    super(start)
  }
}

export class TokenConflictDeclaration extends Node {
  constructor(start: number,
              readonly a: NameExpression | LiteralExpression,
              readonly b: NameExpression | LiteralExpression) {
    super(start)
  }
}

export class TokenDeclaration extends Node {
  constructor(start: number,
              readonly precedences: readonly TokenPrecDeclaration[],
              readonly conflicts: readonly TokenConflictDeclaration[],
              readonly rules: readonly RuleDeclaration[],
              readonly literals: readonly LiteralDeclaration[]) {
    super(start)
  }
}

export class LiteralDeclaration extends Node {
  constructor(start: number,
              readonly literal: string,
              readonly props: readonly Prop[]) { super(start) }
}

export class ContextDeclaration extends Node {
  constructor(start: number,
              readonly id: Identifier,
              readonly source: string) { super(start) }
}

export class ExternalTokenDeclaration extends Node {
  constructor(start: number,
              readonly id: Identifier,
              readonly source: string,
              readonly tokens: readonly {id: Identifier, props: readonly Prop[]}[]) {
    super(start)
  }
}

export class ExternalSpecializeDeclaration extends Node {
  constructor(start: number,
              readonly type: "extend" | "specialize",
              readonly token: Expression,
              readonly id: Identifier,
              readonly source: string,
              readonly tokens: readonly {id: Identifier, props: readonly Prop[]}[]) {
    super(start)
  }
}

export class ExternalPropDeclaration extends Node {
  constructor(start: number,
              readonly id: Identifier,
              readonly externalID: Identifier,
              readonly source: string) {
    super(start)
  }
}

export class Identifier extends Node {
  constructor(start: number, readonly name: string) {
    super(start)
  }
  toString() { return this.name }
}

export class Expression extends Node {
  walk(f: (expr: Expression) => Expression): Expression { return f(this) }
  eq(_other: Expression): boolean { return false }
  prec!: number
}

Expression.prototype.prec = 10

export class NameExpression extends Expression {
  constructor(start: number, readonly namespace: Identifier | null, readonly id: Identifier, readonly args: readonly Expression[]) {
    super(start)
  }
  toString() { return this.id.name + (this.args.length ? `<${this.args.join()}>` : "") }
  eq(other: NameExpression) {
    return (this.namespace ? other.namespace != null && other.namespace.name == this.namespace.name : !other.namespace) &&
      this.id.name == other.id.name && exprsEq(this.args, other.args)
  }
  walk(f: (expr: Expression) => Expression): Expression {
    let args = walkExprs(this.args, f)
    return f(args == this.args ? this : new NameExpression(this.start, this.namespace, this.id, args))
  }
}

export class SpecializeExpression extends Expression {
  constructor(start: number, readonly type: string, readonly props: readonly Prop[],
              readonly token: Expression, readonly content: Expression) { super(start) }
  toString() { return `@${this.type}[${this.props.join(",")}]<${this.token}, ${this.content}>` }
  eq(other: SpecializeExpression) {
    return this.type == other.type && Prop.eqProps(this.props, other.props) && exprEq(this.token, other.token) &&
      exprEq(this.content, other.content)
  }
  walk(f: (expr: Expression) => Expression): Expression {
    let token = this.token.walk(f), content = this.content.walk(f)
    return f(token == this.token && content == this.content ? this : new SpecializeExpression(this.start, this.type, this.props, token, content))
  }
}

export class InlineRuleExpression extends Expression {
  constructor(start: number, readonly rule: RuleDeclaration) { super(start) }

  toString() {
    let rule = this.rule
    return `${rule.id}${rule.props.length ? `[${rule.props.join(",")}]` : ""} { ${rule.expr} }`
  }
  eq(other: InlineRuleExpression) {
    let rule = this.rule, oRule = other.rule
    return exprEq(rule.expr, oRule.expr) && rule.id.name == oRule.id.name && Prop.eqProps(rule.props, oRule.props)
  }
  walk(f: (expr: Expression) => Expression): Expression {
    let rule = this.rule, expr = rule.expr.walk(f)
    return f(expr == rule.expr ? this :
             new InlineRuleExpression(this.start, new RuleDeclaration(rule.start, rule.id, rule.props, [], expr)))
  }
}

export class ChoiceExpression extends Expression {
  constructor(start: number, readonly exprs: readonly Expression[]) {
    super(start)
  }
  toString() { return this.exprs.map(e => maybeParens(e, this)).join(" | ") }
  eq(other: ChoiceExpression) {
    return exprsEq(this.exprs, other.exprs)
  }
  walk(f: (expr: Expression) => Expression): Expression {
    let exprs = walkExprs(this.exprs, f)
    return f(exprs == this.exprs ? this : new ChoiceExpression(this.start, exprs))
  }
}

ChoiceExpression.prototype.prec = 1

export class SequenceExpression extends Expression {
  constructor(
    start: number,
    readonly exprs: readonly Expression[],
    readonly markers: readonly (readonly ConflictMarker[])[],
    readonly empty = false
  ) {
    super(start)
  }
  toString() { return this.empty ? "()" : this.exprs.map(e => maybeParens(e, this)).join(" ") }
  eq(other: SequenceExpression) {
    return exprsEq(this.exprs, other.exprs) && this.markers.every((m, i) => {
      let om = other.markers[i]
      return m.length == om.length && m.every((x, i) => x.eq(om[i]))
    })
  }
  walk(f: (expr: Expression) => Expression): Expression {
    let exprs = walkExprs(this.exprs, f)
    return f(exprs == this.exprs ? this : new SequenceExpression(this.start, exprs, this.markers, this.empty && !exprs.length))
  }
}

SequenceExpression.prototype.prec = 2

export class ConflictMarker extends Node {
  constructor(start: number, readonly id: Identifier, readonly type: "ambig" | "prec") {
    super(start)
  }

  toString() { return (this.type == "ambig" ? "~" : "!") + this.id.name }

  eq(other: ConflictMarker) { return this.id.name == other.id.name && this.type == other.type }
}

export class RepeatExpression extends Expression {
  constructor(start: number, readonly expr: Expression, readonly kind: "?" | "*" | "+") {
    super(start)
  }
  toString() { return maybeParens(this.expr, this) + this.kind }
  eq(other: RepeatExpression) {
    return exprEq(this.expr, other.expr) && this.kind == other.kind
  }
  walk(f: (expr: Expression) => Expression): Expression {
    let expr: Expression = this.expr.walk(f)
    return f(expr == this.expr ? this : new RepeatExpression(this.start, expr, this.kind))
  }
}

RepeatExpression.prototype.prec = 3

export class LiteralExpression extends Expression {
  // value.length is always > 0
  constructor(start: number, readonly value: string) {
    super(start)
  }
  toString() { return JSON.stringify(this.value) }
  eq(other: LiteralExpression) { return this.value == other.value }
}

export class SetExpression extends Expression {
  constructor(start: number, readonly ranges: [number, number][], readonly inverted: boolean) {
    super(start)
  }
  toString() {
    return `[${this.inverted ? "^" : ""}${this.ranges.map(([a, b]) => {
      return String.fromCodePoint(a) + (b == a + 1 ? "" : "-" + String.fromCodePoint(b))
    })}]`
  }
  eq(other: SetExpression) {
    return this.inverted == other.inverted && this.ranges.length == other.ranges.length &&
      this.ranges.every(([a, b], i) => { let [x, y] = other.ranges[i]; return a == x && b == y })
  }
}

export class AnyExpression extends Expression {
  constructor(start: number) {
    super(start)
  }
  toString() { return "_" }
  eq() { return true }
}

function walkExprs(exprs: readonly Expression[], f: (expr: Expression) => Expression): readonly Expression[] {
  let result: Expression[] | null = null
  for (let i = 0; i < exprs.length; i++) {
    let expr = exprs[i].walk(f)
    if (expr != exprs[i] && !result) result = exprs.slice(0, i)
    if (result) result.push(expr)
  }
  return result || exprs
}

export function exprEq(a: Expression, b: Expression): boolean {
  return a.constructor == b.constructor && a.eq(b as any)
}

export function exprsEq(a: readonly Expression[], b: readonly Expression[]) {
  return a.length == b.length && a.every((e, i) => exprEq(e, b[i]))
}

export class Prop extends Node {
  constructor(start: number, readonly at: boolean, readonly name: string, readonly value: readonly PropPart[]) { super(start) }

  eq(other: Prop) {
    return this.name == other.name && this.value.length == other.value.length &&
      this.value.every((v, i) => v.value == other.value[i].value && v.name == other.value[i].name)
  }

  toString() {
    let result = (this.at ? "@" : "") + this.name
    if (this.value.length) {
      result += "="
      for (let {name, value} of this.value)
        result += name ? `{${name}}` : /[^\w-]/.test(value!) ? JSON.stringify(value) : value
    }
    return result
  }

  static eqProps(a: readonly Prop[], b: readonly Prop[]) {
    return a.length == b.length && a.every((p, i) => p.eq(b[i]))
  }
}

export class PropPart extends Node {
  constructor(start: number, readonly value: string | null, readonly name: string | null) { super(start) }
}

function maybeParens(node: Expression, parent: Expression) {
  return node.prec < parent.prec ? "(" + node.toString() + ")" : node.toString()
}
