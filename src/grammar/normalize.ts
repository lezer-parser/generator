import {updateNode, expression, walkExpr, RuleDeclaration, Expression, GrammarDeclaration} from "./node"

export function normalizeGrammar(grammar: GrammarDeclaration): GrammarDeclaration {
  let rules: RuleDeclaration[] = []

  // FIXME check rule use (existence, arity), duplicate names
  // FIXME expand instances of parameterized rules
  for (let rule of grammar.rules) {
    let simple = simplifyRule(rule, rules)
    rules.push(simple == rule.expr ? rule : updateNode(rule, {expr: simple}))
  }

  return updateNode(grammar, {rules})
}

function simplifyRule(rule: RuleDeclaration, rules: RuleDeclaration[]): Expression {
  let counter = 1
  function newName(pos: number) {
    return expression.identifier(rule.id.name + "-" + counter++, pos, pos)
  }
  function lift(expr: Expression, id = newName(expr.start)): Expression {
    rules.push(new RuleDeclaration(expr.start, expr.start, false, id, [], expr))
    return expression.named(null, id)
  }
 
  return walkExpr(rule.expr, (expr, depth) => {
    if (expr.type == "RepeatExpression") {
      if (expr.kind == "?") {
        let choice = expression.choice([expr.expr, expression.sequence([], expr.start, expr.start)])
        return depth > 0 ? lift(choice) : choice
      }
      let inline = depth == 0 && expr.kind == "*"
      let id = inline ? expression.identifier(rule.id.name, expr.start, expr.start) : newName(expr.start)
      let choice = expression.choice([expression.sequence([expression.named(null, id), expr.expr]),
                                      expression.sequence([], expr.start, expr.start)])
      if (expr.kind == "*")
        return inline ? choice : lift(choice, id)
      else
        return expression.sequence([expr, lift(choice, id)])
    } else if (expr.type == "ChoiceExpression" && depth > 0) {
      return lift(expr)
    } else {
      return expr
    }
  })
}
