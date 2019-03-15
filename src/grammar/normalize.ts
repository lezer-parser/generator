import {updateNode, expression, walkExpr, RuleDeclaration, Expression, GrammarDeclaration} from "./node"

export function normalizeGrammar(grammar: GrammarDeclaration): GrammarDeclaration {
  let rules: {[name: string]: RuleDeclaration} = Object.create(null)

  for (let name in grammar.rules) {
    let rule = grammar.rules[name]
    let simple = simplifyRuleExpr(rule.expr, rule.id.name, rules)
    rules[name] = simple == rule.expr ? rule : updateNode(rule, {expr: simple})
  }

  return updateNode(grammar, {rules})
}

function simplifyRuleExpr(ruleExpr: Expression, ruleName: string, rules: {[name: string]: RuleDeclaration}): Expression {
  let counter = 1
  function newName(pos: number) {
    return expression.identifier(ruleName + "-" + counter++, pos, pos)
  }
  function define(expr: Expression, id = newName(expr.start)): Expression {
    rules[id.name] = new RuleDeclaration(expr.start, expr.start, false, id, [], expr)
    return expression.named(id)
  }

  return walkExpr(ruleExpr, expr => {
    if (expr.type == "RepeatExpression") {
      if (expr.kind == "?") {
        let choice = expression.choice([expr.expr, expression.sequence([], expr.start, expr.start)])
        return ruleExpr == expr ? define(choice) : choice
      }
      let inline = ruleExpr == expr && expr.kind == "*"
      let id = inline ? expression.identifier(ruleName, expr.start, expr.start) : newName(expr.start)
      let choice = expression.choice([expression.sequence([expression.named(id), expr.expr]),
                                      expression.sequence([], expr.start, expr.start)])
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
