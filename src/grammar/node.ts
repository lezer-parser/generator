export class Node {
  constructor(readonly type: string, readonly start: number, readonly end: number) {}
}

type A<T> = ReadonlyArray<T>

export class GrammarDeclaration extends Node {
  type!: "GrammarDeclaration"
  constructor(start: number, end: number,
              readonly rules: A<RuleDeclaration>,
              readonly tokenGroups: A<TokenGroupDeclaration>,
              readonly precedences: A<PrecDeclaration>) {
    super("GrammarDeclaration", start, end)
  }
  toString() { return Object.values(this.rules).join("\n") }
}

export class RuleDeclaration extends Node {
  type!: "RuleDeclaration"
  constructor(start: number, end: number,
              readonly id: Identifier,
              readonly params: A<Identifier>,
              readonly expr: Expression) {
    super("RuleDeclaration", start, end)
  }
  toString() {
    return this.id.name + (this.params.length ? `<${this.params.join()}>` : "") + " -> " + this.expr
  }
}

export class PrecDeclaration extends Node {
  type!: "PrecDeclaration"
  constructor(start: number, end: number,
              readonly id: Identifier,
              readonly assoc: ("left" | "right" | null)[], readonly names: A<Identifier>) {
    super("PrecDeclaration", start, end)
  }
}

export class TokenGroupDeclaration extends Node {
  type!: "TokenGroupDeclaration"
  constructor(start: number, end: number, readonly rules: A<RuleDeclaration>) {
    super("TokenGroupDeclaration", start, end)
  }
}

export class Identifier extends Node {
  type!: "Identifier"
  constructor(start: number, end: number, readonly name: string) {
    super("Identifier", start, end)
  }
  toString() { return this.name }
}

export class NamedExpression extends Node {
  type!: "NamedExpression"
  constructor(start: number, end: number, readonly namespace: Identifier | null, readonly id: Identifier, readonly args: A<Expression>) {
    super("NamedExpression", start, end)
  }
  toString() { return this.id.name + (this.args.length ? `<${this.args.join()}>` : "") }
  eq(other: NamedExpression) {
    return (this.namespace ? other.namespace != null && other.namespace.name == this.namespace.name : !other.namespace) &&
      this.id.name == other.id.name
  }
  containsNames(names: ReadonlyArray<string>): boolean {
    return this.namespace == null && names.includes(this.id.name)
  }
}

export class ChoiceExpression extends Node {
  type!: "ChoiceExpression"
  constructor(start: number, end: number, readonly exprs: A<Expression>) {
    super("ChoiceExpression", start, end)
  }
  toString() { return this.exprs.join(" | ") }
  eq(other: ChoiceExpression) {
    return exprsEq(this.exprs, other.exprs)
  }
  containsNames(names: ReadonlyArray<string>): boolean {
    return this.exprs.some(e => e.containsNames(names))
  }
}

export class SequenceExpression extends Node {
  type!: "SequenceExpression"
  constructor(start: number, end: number, readonly exprs: A<Expression>) {
    super("SequenceExpression", start, end)
  }
  toString() { return this.exprs.join(" ") }
  eq(other: SequenceExpression) {
    return exprsEq(this.exprs, other.exprs)
  }
  containsNames(names: ReadonlyArray<string>): boolean {
    return this.exprs.some(e => e.containsNames(names))
  }
}

export class RepeatExpression extends Node {
  type!: "RepeatExpression"
  constructor(start: number, end: number, readonly expr: Expression, readonly kind: "?" | "*" | "+") {
    super("RepeatExpression", start, end)
  }
  toString() { return this.expr + this.kind }
  eq(other: RepeatExpression) {
    return exprEq(this.expr, other.expr) && this.kind == other.kind
  }
  containsNames(names: ReadonlyArray<string>): boolean {
    return this.expr.containsNames(names)
  }
}

export class LiteralExpression extends Node {
  type!: "LiteralExpression"
  constructor(start: number, end: number, readonly value: string) {
    super("LiteralExpression", start, end)
  }
  toString() { return JSON.stringify(this.value) }
  eq(other: LiteralExpression) { return this.value == other.value }
  containsNames() { return false }
}

export class CharacterRangeExpression extends Node {
  type!: "CharacterRangeExpression"
  constructor(start: number, end: number, readonly from: string, readonly to: string) {
    super("CharacterRangeExpression", start, end)
  }
  toString() { return `${JSON.stringify(this.from)}-${JSON.stringify(this.to)}` }
  eq(other: CharacterRangeExpression) { return this.from == other.from && this.to == other.to }
  containsNames() { return false }
}

export class AnyExpression extends Node {
  type!: "AnyExpression"
  constructor(start: number, end: number) {
    super("AnyExpression", start, end)
  }
  toString() { return "_" }
  eq() { return true }
  containsNames() { return false }
}

export type Expression = NamedExpression | ChoiceExpression | SequenceExpression | LiteralExpression |
  RepeatExpression | CharacterRangeExpression | AnyExpression

export function exprEq(a: Expression, b: Expression): boolean {
  return a.type == b.type && a.eq(b as any)
}

export function exprsEq(a: A<Expression>, b: A<Expression>) {
  return a.length == b.length && a.every((e, i) => exprEq(e, b[i]))
}
