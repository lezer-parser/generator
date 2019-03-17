import {updateNode, expression, walkExpr, RuleDeclaration, Expression, GrammarDeclaration, ChoiceKind, Associativity} from "./node"

export function normalizeGrammar(grammar: GrammarDeclaration): GrammarDeclaration {
  let rules: RuleDeclaration[] = []

  // FIXME check rule use (existence, arity), duplicate names
  // FIXME expand instances of parameterized rules
  for (let rule of grammar.rules) {
    let simple = simplifyRuleExpr(rule.expr, rule.id.name, rules)
    rules.push(simple == rule.expr ? rule : updateNode(rule, {expr: simple}))
  }

  return updateNode(grammar, {rules})
}

function simplifyRuleExpr(ruleExpr: Expression, ruleName: string, rules: RuleDeclaration[]): Expression {
  let counter = 1
  function newName(pos: number) {
    return expression.identifier(ruleName + "-" + counter++, pos, pos)
  }
  function define(expr: Expression, id = newName(expr.start)): Expression {
    rules.push(new RuleDeclaration(expr.start, expr.start, false, id, [], Associativity.None, expr))
    return expression.named(id)
  }

  return walkExpr(ruleExpr, expr => {
    if (expr.type == "RepeatExpression") {
      if (expr.kind == "?") {
        let choice = expression.choice([expr.expr, expression.sequence([], expr.start, expr.start)], ChoiceKind.Plain)
        return ruleExpr == expr ? define(choice) : choice
      }
      let inline = ruleExpr == expr && expr.kind == "*"
      let id = inline ? expression.identifier(ruleName, expr.start, expr.start) : newName(expr.start)
      let choice = expression.choice([expression.sequence([expression.named(id), expr.expr]),
                                      expression.sequence([], expr.start, expr.start)], ChoiceKind.Plain)
      if (expr.kind == "*")
        return inline ? choice : define(choice, id)
      else
        return expression.sequence([expr, define(choice, id)])
    } else if (expr.type == "ChoiceExpression" && expr != ruleExpr) {
      return define(expr)
    } else {
      return expr
    }
  })
}
