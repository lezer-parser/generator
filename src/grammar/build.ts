import {GrammarDeclaration, RuleDeclaration, TokenGroupDeclaration,
        Expression, Identifier, LiteralExpression, NamedExpression, SequenceExpression,
        ChoiceExpression, RepeatExpression, SetExpression, AnyExpression, MarkedExpression,
        exprsEq, exprEq} from "./node"
import {Term, TermSet, Precedence, Rule, Grammar} from "./grammar"
import {Edge, State, Tokenizer} from "./token"
import {Input} from "./parse"
import {buildAutomaton, State as LRState} from "./automaton"
import log from "../log" // FIXME

// FIXME add inlining other other grammar simplifications?

const none: ReadonlyArray<any> = []

class PrecTerm {
  constructor(readonly term: Term, readonly prec: Precedence[]) {}

  get terminal() { return this.term.terminal }
  get name() { return this.term.name }

  static from(term: Term$, prec: Precedence): Term$ {
    if (term instanceof PrecTerm) return new PrecTerm(term.term, [prec].concat(term.prec))
    else return new PrecTerm(term, [prec])
  }

  static onFirst(terms: Term$[], prec: Precedence): Term$[] {
    return terms.length ? [PrecTerm.from(terms[0], prec)].concat(terms.slice(1)) : terms
  }
}

type Term$ = Term | PrecTerm

class Context {
  constructor(readonly b: Builder,
              readonly rule: RuleDeclaration) {}

  newName(deco?: string) {
    return this.b.newName(this.rule.id.name + (deco ? "-" + deco : ""), deco ? true : null)
  }

  defineRule(name: Term, choices: Term$[][]) {
    for (let choice of choices) {
      let precedences = none as ReadonlyArray<Precedence>[]
      let terms = choice.map((term, i) => {
        if (!(term instanceof PrecTerm)) return term
        if (precedences == none) precedences = []
        for (let j = 0; j < i; j++) precedences.push(none)
        precedences[i] = term.prec
        return term.term
      })
      this.b.rules.push(new Rule(name, terms, precedences))
    }
    return [name]
  }

  resolve(expr: NamedExpression): Term[] {
    if (expr.namespace) {
      let ns = this.b.namespaces[expr.namespace.name]
      if (!ns)
        this.raise(`Reference to undefined namespace '${expr.namespace.name}'`, expr.start)
      return ns.resolve(expr, this)
    } else if (expr.id.name == "specialize") {
      return this.resolveSpecialization(expr)
    } else {
      for (let built of this.b.built) if (built.matches(expr)) return [built.term]

      for (let tokens of this.b.tokenGroups) {
        let found = tokens.getToken(expr, this)
        if (found) return [found]
      }

      let known = this.b.ast.rules.find(r => r.id.name == expr.id.name)
      if (!known)
        return this.raise(`Reference to undefined rule '${expr.id.name}'`, expr.start)
      if (known.params.length != expr.args.length)
        this.raise(`Wrong number or arguments for '${expr.id.name}'`, expr.start)
      return this.buildRule(known, expr.args)
    }
  }

  normalizeTopExpr(expr: Expression, self: Term): Term$[][] {
    if (expr instanceof RepeatExpression && expr.kind == "?") {
      return [[], ...this.normalizeTopExpr(expr.expr, self)]
    } else if (expr instanceof RepeatExpression && !self.tag) {
      return this.normalizeRepeat(expr, self)
    } else if (expr instanceof ChoiceExpression) {
      return expr.exprs.map(e => this.normalizeExpr(e))
    } else if (expr instanceof MarkedExpression) {
      return this.normalizeTopExpr(expr.expr, self).map(terms => PrecTerm.onFirst(terms, this.b.getPrecedence(expr)))
    } else {
      return [this.normalizeExpr(expr)]
    }
  }

  // For tree-balancing reasons, repeat expressions X* have to be
  // normalized to something like
  //
  //     Outer -> ε | Inner
  //     Inner -> X | Inner Inner
  //
  // (With the ε part gone for + expressions.)
  //
  // Returns the terms that make up the outer rule.
  normalizeRepeat(expr: RepeatExpression, outer: Term) {
    let inner
    let known = this.b.built.find(b => b.matchesRepeat(expr.expr))
    if (known) {
      inner = known.term
    } else {
      inner = expr.expr instanceof NamedExpression ? this.b.newName(expr.expr.id.name + expr.kind, true) : this.newName(expr.kind)
      inner.repeated = true
      this.b.built.push(new BuiltRule("-repeat", [expr.expr], inner))
      let top = this.normalizeTopExpr(expr.expr, inner)
      top.push([inner, PrecTerm.from(inner, new Precedence(false, Precedence.REPEAT, "left", null))])
      this.defineRule(inner, top)
    }
    outer.repeats = inner
    return expr.kind == "+" ? [[inner]] : [[], [inner]]
  }

  normalizeExpr(expr: Expression): Term$[] {
    if (expr instanceof RepeatExpression && expr.kind == "?") {
      let name = this.newName("?")
      return this.defineRule(name, [[] as Term$[]].concat(this.normalizeTopExpr(expr.expr, name)))
    } else if (expr instanceof RepeatExpression) {
      let outer = expr.expr instanceof NamedExpression ? this.b.newName(expr.expr.id.name + expr.kind + "-wrap", true)
        : this.newName("wrap-" + expr.kind)
      this.defineRule(outer, this.normalizeRepeat(expr, outer))
      return [outer]
    } else if (expr instanceof ChoiceExpression) {
      return this.defineRule(this.newName(), expr.exprs.map(e => this.normalizeExpr(e)))
    } else if (expr instanceof SequenceExpression) {
      return expr.exprs.reduce((a, e) => a.concat(this.normalizeExpr(e)), [] as Term$[])
    } else if (expr instanceof LiteralExpression) {
      return expr.value ? [this.b.tokenGroups[0].getLiteral(expr)] : []
    } else if (expr instanceof NamedExpression) {
      return this.resolve(expr)
    } else if (expr instanceof MarkedExpression) {
      return PrecTerm.onFirst(this.normalizeExpr(expr.expr), this.b.getPrecedence(expr))
    } else {
      return this.raise("This type of expression may not occur in non-token rules", expr.start)
    }
  }

  raise(message: string, pos: number = -1): never {
    return this.b.input.raise(message, pos)
  }

  buildRule(rule: RuleDeclaration, args: ReadonlyArray<Expression>): Term[] {
    let cx = new Context(this.b, rule)
    let expr = args.length ? this.b.substituteArgs(rule.expr, args, rule.params) : rule.expr
    let name = this.b.newName(rule.id.name, isTag(rule.id.name) || true)
    this.b.built.push(new BuiltRule(rule.id.name, args, name))
    return cx.defineRule(name, cx.normalizeTopExpr(expr, name))
  }

  resolveSpecialization(expr: NamedExpression) {
    if (expr.args.length != 2) this.raise(`'specialize' takes two arguments`, expr.start)
    if (!(expr.args[1] instanceof LiteralExpression))
      this.raise(`The second argument to 'specialize' must be a literal`, expr.args[1].start)
    let terminal = this.normalizeExpr(expr.args[0])
    if (terminal.length != 1 || !terminal[0].terminal)
      this.raise(`The first argument to 'specialize' must resolve to a token`, expr.args[0].start)
    let term = terminal[0].name, value = (expr.args[1] as LiteralExpression).value
    let table = this.b.specialized[term] || (this.b.specialized[term] = Object.create(null))
    let known = table[value]
    if (!known) known = table[value] =
      this.b.makeTerminal(term + "-" + JSON.stringify(value), null, this.b.tokens[term])
    return [known]
  }
}

function isTag(name: string) {
  let ch0 = name[0]
  return ch0.toUpperCase() == ch0 && ch0 != "_" ? name : null
}

class BuiltRule {
  constructor(readonly id: string,
              readonly args: ReadonlyArray<Expression>,
              readonly term: Term) {}

  matches(expr: NamedExpression) {
    return this.id == expr.id.name && exprsEq(expr.args, this.args)
  }

  matchesRepeat(expr: Expression) {
    return this.id == "-repeat" && exprEq(expr, this.args[0])
  }
}

let grammarID = 1

class Builder {
  ast: GrammarDeclaration
  input: Input
  grammarID = grammarID++
  terms = new TermSet(this.grammarID)
  tokenGroups: TokenGroup[] = []
  specialized: {[name: string]: {[value: string]: Term}} = Object.create(null)
  rules: Rule[] = []
  built: BuiltRule[] = []
  ruleNames: {[name: string]: boolean} = Object.create(null)
  namespaces: {[name: string]: Namespace} = Object.create(null)
  tokens: {[name: string]: TokenGroup} = Object.create(null)

  constructor(text: string, fileName: string | null = null) {
    this.input = new Input(text, fileName)
    this.ast = this.input.parse()

    if (this.ast.tokens) this.gatherTokenGroups(this.ast.tokens)
    else this.tokenGroups.push(new TokenGroup(this, none, null))

    this.defineNamespace("tag", new TagNamespace)

    for (let rule of this.ast.rules) {
      this.unique(rule.id)
      if (this.namespaces[rule.id.name])
        this.input.raise(`Rule name '${rule.id.name}' conflicts with a defined namespace`, rule.id.start)
      if (rule.id.name == "program") {
        if (rule.params.length) this.input.raise(`'program' rules should not take parameters`, rule.id.start)
        new Context(this, rule).buildRule(rule, [])
      }
    }
    if (!this.rules.length)
      this.input.raise(`Missing 'program' rule declaration`)
    for (let rule of this.ast.rules) {
      if (!this.rules.some(r => r.name.name == rule.id.name))
        // FIXME should probably be a warning
        this.input.raise(`Unused rule '${rule.id.name}'`, rule.start)
    }
    for (let tokens of this.tokenGroups) tokens.checkUnused()
  }

  unique(id: Identifier) {
    if (this.ruleNames[id.name])
      this.input.raise(`Duplicate definition of rule '${id.name}'`, id.start)
    if (id.name == "specialize") this.input.raise("The name 'specialize' is reserved for a built-in operator", id.start)
    this.ruleNames[id.name] = true
  }

  defineNamespace(name: string, value: Namespace, pos: number = 0) {
    if (this.namespaces[name]) this.input.raise(`Duplicate definition of namespace '${name}'`, pos)
    this.namespaces[name] = value
  }

  newName(base: string, tag: string | null | true = null): Term {
    for (let i = tag ? 0 : 1;; i++) {
      let name = i ? `${base}-${i}` : base
      if (!this.terms.nonTerminals.some(t => t.name == name))
        return this.terms.makeNonTerminal(name, tag === true ? null : tag)
    }
  }

  getGrammar() {
    let rules = simplifyRules(this.rules)
    let table = buildAutomaton(rules, this.terms)
    // FIXME merge equivalent skip directives
    let tokenizers: Tokenizer[] = []
    for (let group of this.tokenGroups) {
      let skip = group.skipState ? group.skipState.compile() : group.parent ? tokenizers[this.tokenGroups.indexOf(group.parent)].skip : null
      let tokenizer = new Tokenizer(skip, group.startState.compile(), this.specialized) // FIXME separate specialized per tokenizer
      if (tokenizer.startState.accepting)
        this.input.raise(`Grammar contains zero-length tokens (in '${tokenizer.startState.accepting.name}')`,
                         group.rules.find(r => r.id.name == tokenizer.startState.accepting!.name)!.start)
      tokenizers.push(tokenizer)
    }
    let tokenTable = table.map(state => this.tokensForState(state, tokenizers))
    if (log.grammar) console.log(rules.join("\n"))
    if (log.lr) console.log(table.join("\n"))
    return new Grammar(this.grammarID, rules, this.terms, table, tokenTable)
  }

  gatherTokenGroups(decl: TokenGroupDeclaration, parent: TokenGroup | null = null) {
    let group = new TokenGroup(this, decl.rules, parent)
    this.tokenGroups.push(group)
    for (let subGroup of decl.groups) this.gatherTokenGroups(subGroup, group)
  }

  makeTerminal(name: string, tag: string | null, group: TokenGroup) {
    for (let i = 0;; i++) {
      let cur = i ? `${name}-${i}` : name
      if (this.terms.terminals.some(t => t.name == cur)) continue
      this.tokens[cur] = group
      return this.terms.makeTerminal(cur, tag)
    }
  }

  tokensForState(state: LRState, tokenizers: ReadonlyArray<Tokenizer>) {
    let found: Tokenizer[] = []
    for (let action of state.terminals) {
      if (action.term.eof) return tokenizers // Try all possible whitespace before eof
      let group = this.tokens[action.term.name]
      let tokenizer = tokenizers[this.tokenGroups.indexOf(group)]
      if (!found.includes(tokenizer)) found.push(tokenizer)
    }
    return found
  }

  substituteArgs(expr: Expression, args: ReadonlyArray<Expression>, params: ReadonlyArray<Identifier>) {
    return expr.walk(expr => {
      let found
      if (expr instanceof NamedExpression && !expr.namespace &&
          (found = params.findIndex(p => p.name == expr.id.name)) > -1) {
        let arg = args[found]
        if (expr.args.length) {
          if (arg instanceof NamedExpression && !arg.args.length)
            return new NamedExpression(expr.start, arg.namespace, arg.id, expr.args)
          this.input.raise(`Passing arguments to a parameter that already has arguments`, expr.start)
        }
        return arg
      }
      return expr
    })
  }

  getPrecedence(expr: MarkedExpression): Precedence {
    if (!expr.namespace) {
      let precs = this.ast.precedences!
      let pos = precs ? precs.names.findIndex(id => id.name == expr.id.name) : -1
      if (pos < 0) this.input.raise(`Reference to unknown precedence: '${expr.id.name}'`, expr.start)
      return new Precedence(false, precs.names.length - pos, precs.assoc[pos], null)
    }
    if (expr.namespace.name != "ambig")
      this.input.raise(`Unrecognized conflict marker '!${expr.namespace.name}.${expr.id.name}'`, expr.start)
    return new Precedence(true, 0, null, expr.id.name)
  }
}

interface Namespace {
  resolve(expr: NamedExpression, cx: Context): Term[]
}

class TagNamespace implements Namespace {
  resolve(expr: NamedExpression, cx: Context): Term[] {
    if (expr.args.length != 1)
      cx.raise(`Tag wrappers take a single argument`, expr.start)
    let tag = expr.id.name
    let name = cx.b.newName(`tag.${tag}`, tag)
    return cx.defineRule(name, cx.normalizeTopExpr(expr.args[0], name))
  }
}

class TokenArg {
  constructor(readonly name: string, readonly expr: Expression, readonly scope: ReadonlyArray<TokenArg>) {}
}

class TokenGroup {
  startState: State = new State
  skipState: State | null = null
  built: BuiltRule[] = []
  used: {[name: string]: boolean} = Object.create(null)
  building: string[] = [] // Used for recursion check

  constructor(readonly b: Builder,
              readonly rules: ReadonlyArray<RuleDeclaration>,
              readonly parent: TokenGroup | null) {
    for (let rule of rules) if (rule.id.name != "skip") this.b.unique(rule.id)
    let skip = rules.find(r => r.id.name == "skip")
    if (skip) {
      this.used.skip = true
      if (skip.params.length) return this.raise("Skip rules should not take parameters", skip.params[0].start)
      this.skipState = new State
      let fin = new State(b.terms.eof)
      fin.connect(this.build(skip.expr, this.skipState, none))
    }
  }

  getToken(expr: NamedExpression, cx: Context) {
    for (let built of this.built) if (built.matches(expr)) return built.term
    let name = expr.id.name
    let rule = this.rules.find(r => r.id.name == name)
    if (!rule) return null
    let term = this.b.makeTerminal(name, isTag(name), this)
    let end = new State(term)
    end.connect(this.buildRule(rule, expr, this.startState))
    this.built.push(new BuiltRule(name, expr.args, term))
    return term
  }

  getLiteral(expr: LiteralExpression) {
    let id = JSON.stringify(expr.value)
    for (let built of this.built) if (built.id == id) return built.term
    let term = this.b.makeTerminal(id, null, this)
    let end = new State(term)
    end.connect(this.build(expr, this.startState, none))
    this.built.push(new BuiltRule(id, none, term))
    return term
  }

  defines(term: Term): boolean {
    return this.built.some(b => b.term == term)
  }

  raise(msg: string, pos: number = -1): never {
    return this.b.input.raise(msg, pos)
  }

  buildRule(rule: RuleDeclaration, expr: NamedExpression, from: State, args: ReadonlyArray<TokenArg> = none): Edge[] {
    let name = expr.id.name
    if (rule.params.length != expr.args.length)
      this.raise(`Incorrect number of arguments for token '${name}'`, expr.start)
    this.used[name] = true
    if (this.building.includes(name))
      this.raise(`Recursive token rules: ${this.building.slice(this.building.lastIndexOf(name)).join(" -> ")}`, expr.start)
    this.building.push(name)
    let result = this.build(this.b.substituteArgs(rule.expr, expr.args, rule.params), from,
                            expr.args.map((e, i) => new TokenArg(rule!.params[i].name, e, args)))
    this.building.pop()
    return result
  }

  build(expr: Expression, from: State, args: ReadonlyArray<TokenArg>): Edge[] {
    if (expr instanceof NamedExpression) {
      if (expr.namespace) {
        if (expr.namespace.name == "std") return this.buildStd(expr, from)
        this.b.input.raise(`Unknown namespace '${expr.namespace.name}'`, expr.start)
      }
      let name = expr.id.name, arg = args.find(a => a.name == name)
      if (arg) return this.build(arg.expr, from, arg.scope)
      let rule: RuleDeclaration | undefined = undefined
      for (let scope: TokenGroup | null = this; scope && !rule; scope = scope.parent)
        rule = scope.rules.find(r => r.id.name == name)
      if (!rule) return this.raise(`Reference to rule '${expr.id.name}', which isn't found in this token group`, expr.start)
      return this.buildRule(rule, expr, from, args)
    } else if (expr instanceof ChoiceExpression) {
      return expr.exprs.reduce((out, expr) => out.concat(this.build(expr, from, args)), [] as Edge[])
    } else if (expr instanceof SequenceExpression) {
      for (let i = 0;; i++) {
        let next = this.build(expr.exprs[i], from, args)
        if (i == expr.exprs.length - 1) return next
        from = new State
        from.connect(next)
      }
    } else if (expr instanceof RepeatExpression) {
      if (expr.kind == "*") {
        let loop = new State
        from.nullEdge(loop)
        loop.connect(this.build(expr.expr, loop, args))
        return [loop.nullEdge()]
      } else if (expr.kind == "+") {
        let loop = new State
        loop.connect(this.build(expr.expr, from, args))
        loop.connect(this.build(expr.expr, loop, args))
        return [loop.nullEdge()]
      } else { // expr.kind == "?"
        return [from.nullEdge()].concat(this.build(expr.expr, from, args))
      }
    } else if (expr instanceof SetExpression) {
      return (expr.inverted ? invertRanges(expr.ranges) : expr.ranges).map(([a, b]) => from.edge(a, b))
    } else if (expr instanceof LiteralExpression) {
      for (let i = 0;;) {
        let code = expr.value.codePointAt(i)!
        i += code <= 0xffff ? 1 : 2
        if (i < expr.value.length) {
          let next = new State
          from.edge(code, code + 1, next)
          from = next
        } else {
          return [from.edge(code, code + 1)]
        }
      }
    } else if (expr instanceof AnyExpression) {
      return [from.edge(0, 2e8)] // FIXME optimize out comparison in automaton
    } else {
      return this.raise(`Unrecognized expression type in token`, (expr as any).start)
    }
  }

  buildStd(expr: NamedExpression, from: State) {
    if (expr.args.length) this.raise(`'std.${expr.id.name}' does not take arguments`, expr.args[0].start)
    if (!STD_RANGES.hasOwnProperty(expr.id.name)) this.raise(`There is no builtin rule 'std.${expr.id.name}'`, expr.start)
    return STD_RANGES[expr.id.name].map(([a, b]) => from.edge(a, b)) 
  }

  checkUnused() {
    for (let rule of this.rules) if (!this.used[rule.id.name])
      // FIXME should probably be a warning
      this.raise(`Unused token rule '${rule.id.name}'`, rule.start)
  }
}

function invertRanges(ranges: [number, number][]) {
  let pos = 0, result: [number, number][] = []
  for (let [a, b] of ranges) {
    if (a > pos) result.push([pos, a])
    pos = b
  }
  if (pos < 2e8) result.push([pos, 2e8])
  return result
}

const STD_RANGES: {[name: string]: [number, number][]} = {
  asciiLetter: [[65, 91], [97, 123]],
  asciiLowercase: [[97, 123]],
  asciiUppercase: [[65, 91]],
  digit: [[48, 58]],
  whitespace: [[9, 14], [32, 33], [133, 134], [160, 161], [5760, 5761], [8192, 8203],
               [8232, 8234], [8239, 8240], [8287, 8288], [12288, 12289]]
}

function inlineRules(rules: ReadonlyArray<Rule>): ReadonlyArray<Rule> {
  for (;;) {
    let inlinable: {[name: string]: Rule} = Object.create(null), found
    for (let i = 0; i < rules.length; i++) {
      let rule = rules[i]
      if (!rule.name.interesting && !rule.parts.includes(rule.name) && rule.parts.length < 3 &&
          !rule.parts.some(p => !!inlinable[p.name]) &&
          !rules.some((r, j) => j != i && r.name == rule.name))
        found = inlinable[rule.name.name] = rule
    }
    if (!found) return rules
    let newRules = []
    for (let rule of rules) {
      if (inlinable[rule.name.name]) continue
      if (!rule.parts.some(p => !!inlinable[p.name])) {
        newRules.push(rule)
        continue
      }
      let prec = [], parts = []
      for (let i = 0; i < rule.parts.length; i++) {
        let replace = inlinable[rule.parts[i].name]
        if (!replace) {
          if (i < rule.precedence.length) {
            while (prec.length < parts.length) prec.push(none)
            prec.push(rule.precedence[i])
          }
          parts.push(rule.parts[i])
        } else {
          for (let j = 0; j < replace.parts.length; j++) {
            let partPrec = j ? replace.precAt(j) : Precedence.join(rule.precAt(i), replace.precAt(j))
            if (partPrec.length) {
              while (prec.length < parts.length) prec.push(none)
              prec.push(partPrec)
            }
            parts.push(replace.parts[j])
          }
        }
      }
      newRules.push(new Rule(rule.name, parts, prec))
    }
    rules = newRules
  }
}

function mergeRules(rules: ReadonlyArray<Rule>): ReadonlyArray<Rule> {
  let merged: {[name: string]: Term} = Object.create(null), found
  for (let i = 0; i < rules.length;) {
    let groupStart = i
    let name = rules[i++].name
    while (i < rules.length && rules[i].name == name) i++
    let size = i - groupStart
    if (name.interesting) continue
    for (let j = i; j < rules.length;) {
      let otherStart = j, otherName = rules[j++].name
      while (j < rules.length && rules[j].name == otherName) j++
      if (j - otherStart != size || otherName.interesting) continue
      let match = true
      for (let k = 0; k < size && match; k++) {
        let a = rules[groupStart + k], b = rules[otherStart + k]
        if (a.cmpNoName(b) > 0) match = false
      }
      if (match) found = merged[name.name] = otherName
    }
  }
  if (!found) return rules
  let newRules = []
  for (let rule of rules) if (!merged[rule.name.name]) {
    newRules.push(rule.parts.every(p => !merged[p.name]) ? rule :
                  new Rule(rule.name, rule.parts.map(p => merged[p.name] || p), rule.precedence))
  }
  return newRules
}

function simplifyRules(rules: ReadonlyArray<Rule>): ReadonlyArray<Rule> {
  return mergeRules(inlineRules(rules))
}

export function buildGrammar(text: string, fileName: string | null = null): Grammar {
  return new Builder(text, fileName).getGrammar()
}
