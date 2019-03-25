export class Node {
  constructor(readonly start: number) {}
}

type A<T> = ReadonlyArray<T>

export class GrammarDeclaration extends Node {
  constructor(start: number,
              readonly rules: A<RuleDeclaration>,
              readonly tokens: TokenGroupDeclaration | null,
              readonly precedences: A<PrecDeclaration>) {
    super(start)
  }
  toString() { return Object.values(this.rules).join("\n") }
}

export class RuleDeclaration extends Node {
  constructor(start: number,
              readonly id: Identifier,
              readonly params: A<Identifier>,
              readonly expr: Expression) {
    super(start)
  }
  toString() {
    return this.id.name + (this.params.length ? `<${this.params.join()}>` : "") + " -> " + this.expr
  }
}

export class PrecDeclaration extends Node {
  constructor(start: number,
              readonly id: Identifier,
              readonly assoc: ("left" | "right" | null)[], readonly names: A<Identifier>) {
    super(start)
  }
}

export class TokenGroupDeclaration extends Node {
  constructor(start: number,
              readonly rules: A<RuleDeclaration>,
              readonly groups: A<TokenGroupDeclaration>) {
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
  eq(other: Expression): boolean { return false }
}

export class NamedExpression extends Expression {
  constructor(start: number, readonly namespace: Identifier | null, readonly id: Identifier, readonly args: A<Expression>) {
    super(start)
  }
  toString() { return this.id.name + (this.args.length ? `<${this.args.join()}>` : "") }
  eq(other: NamedExpression) {
    return (this.namespace ? other.namespace != null && other.namespace.name == this.namespace.name : !other.namespace) &&
      this.id.name == other.id.name
  }
  walk(f: (expr: Expression) => Expression): Expression {
    let args = walkExprs(this.args, f)
    return f(args == this.args ? this : new NamedExpression(this.start, this.namespace, this.id, args))
  }
}

export class ChoiceExpression extends Expression {
  constructor(start: number, readonly exprs: A<Expression>) {
    super(start)
  }
  toString() { return this.exprs.join(" | ") }
  eq(other: ChoiceExpression) {
    return exprsEq(this.exprs, other.exprs)
  }
  walk(f: (expr: Expression) => Expression): Expression {
    let exprs = walkExprs(this.exprs, f)
    return f(exprs == this.exprs ? this : new ChoiceExpression(this.start, exprs))
  }
}

export class SequenceExpression extends Expression {
  constructor(start: number, readonly exprs: A<Expression>) {
    super(start)
  }
  toString() { return this.exprs.join(" ") }
  eq(other: SequenceExpression) {
    return exprsEq(this.exprs, other.exprs)
  }
  walk(f: (expr: Expression) => Expression): Expression {
    let exprs = walkExprs(this.exprs, f)
    return f(exprs == this.exprs ? this : new SequenceExpression(this.start, exprs))
  }
}

export class RepeatExpression extends Expression {
  constructor(start: number, readonly expr: Expression, readonly kind: "?" | "*" | "+") {
    super(start)
  }
  toString() { return this.expr + this.kind }
  eq(other: RepeatExpression) {
    return exprEq(this.expr, other.expr) && this.kind == other.kind
  }
  walk(f: (expr: Expression) => Expression): Expression {
    let expr: Expression = this.expr.walk(f)
    return f(expr == this.expr ? this : new RepeatExpression(this.start, expr, this.kind))
  }
}

export class LiteralExpression extends Expression {
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

function walkExprs(exprs: A<Expression>, f: (expr: Expression) => Expression): A<Expression> {
  let result: Expression[] | null = null
  for (let i = 0; i < exprs.length; i++) {
    let expr = f(exprs[i])
    if (expr != exprs[i] && !result) result = exprs.slice(0, i)
    if (result) result.push(expr)
  }
  return result || exprs
}

export function exprEq(a: Expression, b: Expression): boolean {
  return a.constructor == b.constructor && a.eq(b as any)
}

export function exprsEq(a: A<Expression>, b: A<Expression>) {
  return a.length == b.length && a.every((e, i) => exprEq(e, b[i]))
}
