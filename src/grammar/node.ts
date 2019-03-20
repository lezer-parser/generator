export class Node {
  constructor(readonly type: string, readonly start: number, readonly end: number) {}
}

export class GrammarDeclaration extends Node {
  type!: "GrammarDeclaration"
  constructor(start: number, end: number, readonly rules: RuleDeclaration[]) {
    super("GrammarDeclaration", start, end)
  }
  toString() { return Object.values(this.rules).join("\n") }
}

export class RuleDeclaration extends Node {
  type!: "RuleDeclaration"
  constructor(start: number, end: number,
              readonly isToken: boolean,
              readonly id: Identifier,
              readonly params: Identifier[],
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
              readonly assoc: ("left" | "right" | null)[], readonly names: Identifier[]) {
    super("PrecDeclaration", start, end)
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
  constructor(start: number, end: number, readonly namespace: Identifier | null, readonly id: Identifier, readonly args: Expression[]) {
    super("NamedExpression", start, end)
  }
  toString() { return this.id.name + (this.args.length ? `<${this.args.join()}>` : "") }
}

export class ChoiceExpression extends Node {
  type!: "ChoiceExpression"
  constructor(start: number, end: number, readonly exprs: Expression[]) {
    super("ChoiceExpression", start, end)
  }
  toString() { return this.exprs.join(" | ") }
}

export class SequenceExpression extends Node {
  type!: "SequenceExpression"
  constructor(start: number, end: number, readonly exprs: Expression[]) {
    super("SequenceExpression", start, end)
  }
  toString() { return this.exprs.join(" ") }
}

export class RepeatExpression extends Node {
  type!: "RepeatExpression"
  constructor(start: number, end: number, readonly expr: Expression, readonly kind: "?" | "*" | "+") {
    super("RepeatExpression", start, end)
  }
  toString() { return this.expr + this.kind }
}

export class LiteralExpression extends Node {
  type!: "LiteralExpression"
  constructor(start: number, end: number, readonly value: string) {
    super("LiteralExpression", start, end)
  }
  toString() { return JSON.stringify(this.value) }
}

export class CharacterRangeExpression extends Node {
  type!: "CharacterRangeExpression"
  constructor(start: number, end: number, readonly from: string, readonly to: string) {
    super("CharacterRangeExpression", start, end)
  }
  toString() { return `${JSON.stringify(this.from)}-${JSON.stringify(this.to)}` }
}

export class AnyExpression extends Node {
  type!: "AnyExpression"
  constructor(start: number, end: number) {
    super("AnyExpression", start, end)
  }
  toString() { return "_" }
}

export type Expression = NamedExpression | ChoiceExpression | SequenceExpression | LiteralExpression |
  RepeatExpression | CharacterRangeExpression | AnyExpression

export const expression = {
  identifier(name: string, start: number, end: number) {
    return new Identifier(start, end, name)
  },
  named(namespace: Identifier | null, id: Identifier, args: Expression[] = [], start = id.start, end = args.length ? args[args.length - 1].end : id.end) {
    return new NamedExpression(start, end, namespace, id, args)
  },
  sequence(exprs: Expression[], start = exprs[0].start, end = exprs[exprs.length - 1].end) {
    if (exprs.some(e => e.type == "SequenceExpression"))
      exprs = exprs.reduce((a, e) => a.concat(e.type == "SequenceExpression" ? e.exprs : [e]), [] as Expression[])
    return new SequenceExpression(start, end, exprs)
  },
  repeat(expr: Expression, kind: "?" | "*" | "+", start = expr.start, end = expr.end + 1) {
    return new RepeatExpression(start, end, expr, kind)
  },
  choice(exprs: Expression[], start = Math.min(...exprs.map(e => e.start)), end = Math.max(...exprs.map(e => e.end))) {
    if (exprs.some(e => e.type == "ChoiceExpression"))
      exprs = exprs.reduce((a, e) => a.concat(e.type == "ChoiceExpression" ? e.exprs : [e]), [] as Expression[])
    return new ChoiceExpression(start, end, exprs)
  },
  literal(value: string, start: number, end: number) {
    return new LiteralExpression(start, end, value)
  },
  characterRange(from: string, to: string, start: number, end: number) {
    return new CharacterRangeExpression(start, end, from, to)
  },
  any(start: number, end: number) {
    return new AnyExpression(start, end)
  }
}

export function updateNode<T extends Node>(node: T, props: Partial<T>): T {
  let n = new (node.constructor as {new (start: number, end: number): T})(node.start, node.end) as T
  for (let key in node) (n as any)[key] = node[key]
  for (let key in props) (n as any)[key] = props[key]
  return n
}

export function walkExpr(expr: Expression, f: (expr: Expression, depth: number) => Expression, depth = 0): Expression {
  let update = null
  if (expr.type == "RepeatExpression") {
    let ex = walkExpr(expr.expr, f, depth + 1)
    if (ex != expr.expr) update = {expr: ex}
  } else if (expr.type == "ChoiceExpression" || expr.type == "SequenceExpression") {
    let exprs = walkExprs(expr.exprs, f, depth + 1)
    if (exprs != expr.exprs) update = {exprs}
  } else if (expr.type == "NamedExpression" && expr.args.length) {
    let args = walkExprs(expr.args, f, depth + 1)
    if (args != expr.args) update = {args}
  }
  return f(update ? updateNode(expr, update) : expr, depth)
}

function walkExprs(exprs: Expression[], f: (expr: Expression, depth: number) => Expression, depth: number) {
  let result = null
  for (let i = 0; i < exprs.length; i++) {
    let expr = exprs[i], walked = walkExpr(expr, f, depth)
    if (!result && expr != walked) result = exprs.slice(0, i)
    if (result) result.push(walked)
  }
  return result || exprs
}
