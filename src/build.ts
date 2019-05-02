import {GrammarDeclaration, RuleDeclaration, TokenGroupDeclaration, ExternalTokenGroupDeclaration,
        Expression, Identifier, LiteralExpression, NamedExpression, SequenceExpression,
        ChoiceExpression, RepeatExpression, SetExpression, AnyExpression, ConflictMarker,
        exprsEq, exprEq} from "./node"
import {Term, TermSet, PREC_REPEAT, Rule, Conflicts} from "./grammar"
import {State, MAX_CHAR} from "./token"
import {Input} from "./parse"
import {buildAutomaton, State as LRState, Shift, Reduce} from "./automaton"
import {Parser, ParseState, REDUCE_DEPTH_SIZE, noToken, Tokenizer,
        TERM_TAGGED, SPECIALIZE, REPLACE, EXTEND} from "lezer"

const none: readonly any[] = []

const verbose = (typeof process != "undefined" && process.env.LOG) || ""

class Parts {
  constructor(readonly terms: readonly Term[],
              readonly conflicts: null | readonly Conflicts[]) {}

  concat(other: Parts) {
    if (this == Parts.none) return other
    if (other == Parts.none) return this
    let conflicts: null | Conflicts[] = null
    if (this.conflicts || other.conflicts) {
      conflicts = this.conflicts ? this.conflicts.slice() : this.ensureConflicts() as Conflicts[]
      let otherConflicts = other.ensureConflicts()
      conflicts[conflicts.length - 1] = conflicts[conflicts.length - 1].join(otherConflicts[0])
      for (let i = 1; i < otherConflicts.length; i++) conflicts.push(otherConflicts[i])
    }
    return new Parts(this.terms.concat(other.terms), conflicts)
  }

  withConflicts(pos: number, conflicts: Conflicts) {
    if (conflicts == Conflicts.none) return this
    let array = this.conflicts ? this.conflicts.slice() : this.ensureConflicts() as Conflicts[]
    array[pos] = array[pos].join(conflicts)
    return new Parts(this.terms, array)
  }

  ensureConflicts() {
    if (this.conflicts) return this.conflicts
    let empty = []
    for (let i = 0; i <= this.terms.length; i++) empty.push(Conflicts.none)
    return empty
  }

  static none = new Parts(none, null)
}

function p(...terms: Term[]) { return new Parts(terms, null) }

const reserved = ["specialize", "replace", "extend"]

function isTag(name: string) {
  let ch0 = name[0]
  return ch0.toUpperCase() == ch0 && ch0 != "_" ? name : null
}

class BuiltRule {
  constructor(readonly id: string,
              readonly args: readonly Expression[],
              readonly term: Term) {}

  matches(expr: NamedExpression) {
    return this.id == expr.id.name && exprsEq(expr.args, this.args)
  }

  matchesRepeat(expr: RepeatExpression) {
    return this.id == expr.kind && exprEq(expr.expr, this.args[0])
  }
}

export type BuildOptions = {
  fileName?: string,
  warn?: (message: string) => void,
  includeNames?: boolean,
  moduleStyle?: string,
  externalTokenizer?: (name: string, terms: {[name: string]: number}) => Tokenizer
}

class Builder {
  ast: GrammarDeclaration
  input: Input
  terms = new TermSet
  tokenGroups: TokenSet[] = []
  specialized: {[name: string]: {value: string, term: Term, type: string}[]} = Object.create(null)
  rules: Rule[] = []
  built: BuiltRule[] = []
  ruleNames: {[name: string]: boolean} = Object.create(null)
  namespaces: {[name: string]: Namespace} = Object.create(null)
  tokens: {[name: string]: TokenSet} = Object.create(null)
  namedTerms: {[name: string]: Term} = Object.create(null)
  used: {[name: string]: boolean} = Object.create(null)

  constructor(text: string, readonly options: BuildOptions) {
    this.input = new Input(text, options.fileName)
    this.ast = this.input.parse()

    if (this.ast.tokens) this.gatherTokenGroups(this.ast.tokens)
    else this.tokenGroups.push(new TokenGroup(this, none, null, "t0", 2))

    this.defineNamespace("tag", new TagNamespace)

    for (let rule of this.ast.rules) {
      this.unique(rule.id)
      if (this.namespaces[rule.id.name])
        this.raise(`Rule name '${rule.id.name}' conflicts with a defined namespace`, rule.id.start)
      if (rule.id.name == "program") {
        if (rule.params.length) this.raise(`'program' rules should not take parameters`, rule.id.start)
        this.buildRule(rule, [])
      }
    }

    if (!this.rules.length)
      this.raise(`Missing 'program' rule declaration`)
    for (let rule of this.ast.rules) if (!this.used[rule.id.name])
      this.warn(`Unused rule '${rule.id.name}'`, rule.start)
    for (let rule of this.rules) if (rule.parts.length >= 64)
      this.raise(`Overlong rule (${rule.parts.length} > 63) in grammar`)
    for (let tokens of this.tokenGroups) tokens.checkUnused()
  }

  unique(id: Identifier) {
    if (this.ruleNames[id.name])
      this.raise(`Duplicate definition of rule '${id.name}'`, id.start)
    if (reserved.includes(id.name)) this.raise(`The name '${id.name}' is reserved for a built-in operator`, id.start)
    this.ruleNames[id.name] = true
  }

  defineNamespace(name: string, value: Namespace, pos: number = 0) {
    if (this.namespaces[name]) this.raise(`Duplicate definition of namespace '${name}'`, pos)
    this.namespaces[name] = value
  }

  newName(base: string, tag: string | null | true = null, repeats?: Term): Term {
    for (let i = tag ? 0 : 1;; i++) {
      let name = i ? `${base}-${i}` : base
      if (!this.terms.nonTerminals.some(t => t.name == name))
        return this.terms.makeNonTerminal(name, tag === true ? null : tag, repeats)
    }
  }

  getParserData() {
    let rules = simplifyRules(this.rules)
    let {tags, names, repeatInfo} = this.terms.finish(rules)
    if (/\bgrammar\b/.test(verbose)) console.log(rules.join("\n"))
    for (let rule of rules) rule.name.rules.push(rule)
    let table = buildAutomaton(this.terms)
    if (/\blr\b/.test(verbose)) console.log(table.join("\n"))
    let specialized = [], specializations = []
    for (let name in this.specialized) {
      specialized.push(this.terms.terminals.find(t => t.name == name)!.id)
      let table: {[value: string]: number} = {}
      for (let {value, term, type} of this.specialized[name]) {
        let code = type == "specialize" ? SPECIALIZE : type == "replace" ? REPLACE : EXTEND
        table[value] = (term.id << 2) | code
      }
      specializations.push(table)
    }
    let states = table.map(s => this.stateData(s))
    let {taggedGoto, untaggedGoto} = computeGotoTables(table)

    let terms: {[name: string]: number} = {}
    for (let prop in this.namedTerms) terms[prop] = this.namedTerms[prop].id

    return {rules, tags, names, repeatInfo, states, taggedGoto, untaggedGoto, specialized, specializations, terms}
  }

  getParser() {
    let {states, tags, names, repeatInfo, taggedGoto, untaggedGoto, specialized, specializations, terms} = this.getParserData()
    let evaluated: {[source: string]: Tokenizer} = Object.create(null)
    let getTokenizer = (tok: TokenSet | null, skip = false): Tokenizer => {
      if (!tok) return noToken
      let id = tok.id + (skip ? "s" : "")
      return evaluated[id] || (evaluated[id] = (skip ? tok.skip() : tok.tokenizer(terms)) || noToken)
    }

    let stateObjs = states.map((s, i) => {
      let {actions, recover, defaultReduce, forcedReduce, skip, tokenizers} = s
      let toks = tokenizers.map(t => getTokenizer(t)).filter(t => t != noToken)
      return new ParseState(i, actions, recover, defaultReduce, forcedReduce, getTokenizer(skip, true), toks)
    })
    return new Parser(stateObjs, tags, repeatInfo, taggedGoto, untaggedGoto, specialized, specializations, names)
  }

  // FIXME at some point compress the various tables into a single big
  // array, encode it as a string and decode into Uint16Array
  getParserString() {
    let {includeNames = false, moduleStyle = "CommonJS"} = this.options
    let {states, taggedGoto, untaggedGoto, specialized, specializations, tags, names, repeatInfo} = this.getParserData()
    let counts: {[key: string]: number} = Object.create(null)
    function count(value: any) { let key = "" + value; counts[key] = (counts[key] || 0) + 1 }

    function tokenizersToCode(toks: TokenSet[]) {
      return "[" + toks.map(t => t.id).join(",") + "]"
    }
    function numbersToCode(nums: number[]) {
      return "[" + nums.join(",") + "]"
    }

    for (let state of states) {
      count(numbersToCode(state.actions))
      count(numbersToCode(state.recover))
      count(tokenizersToCode(state.tokenizers))
    }

    let generated: {[key: string]: string} = Object.create(null)
    let varID = 0, varText = ""
    function reference(code: string) {
      let count = counts[code]
      if (count == 1) return code
      let name = generated[code]
      if (!name) {
        name = generated[code] = "v" + (varID++)
        varText += `let ${name} = ${code};\n`
      }
      return name
    }

    let stateText = []
    for (let state of states) {
      stateText.push(`s(${state.defaultReduce || reference(numbersToCode(state.actions))
                      }, ${state.forcedReduce}, ${
                      state.skip ? state.skip.id + "s" : "noToken"}, ${reference(tokenizersToCode(state.tokenizers))}${
                      state.recover.length ? ", " + reference(numbersToCode(state.recover)) : ""})`)
    }

    return "// This file was generated by the parser generator (FIXME)\n" +
      (moduleStyle == "es6" ? `import {s, Tokenizer, Parser${states.some(s => !s.skip) ? ", noToken" : ""}} from "lezer"\n`
       : `const {s, Tokenizer, Parser${states.some(s => !s.skip) ? ", noToken" : ""}} = require("lezer")\n`) +
      this.tokenGroups.reduce((s, group) => s + group.source(moduleStyle), "") +
      varText +
      "s.id = 0\n" +
      (moduleStyle == "es6" ? `export default` : `module.exports = `) +
      `new Parser([\n  ${stateText.join(",\n  ")}\n],\n${JSON.stringify(tags)},\n${
       JSON.stringify(repeatInfo)},\n${JSON.stringify(taggedGoto)},\n${JSON.stringify(untaggedGoto)},${
       JSON.stringify(specialized)},\n${JSON.stringify(specializations)
       }${includeNames ? `,\n${JSON.stringify(names)}` : ""})`
  }

  gatherTokenGroups(decl: TokenGroupDeclaration, parent: TokenGroup | null = null) {
    let group = new TokenGroup(this, decl.rules, parent, "t" + this.tokenGroups.length, decl.prec)
    this.tokenGroups.push(group)
    for (let subGroup of decl.groups) {
      if (subGroup instanceof TokenGroupDeclaration) this.gatherTokenGroups(subGroup, group)
      else this.tokenGroups.push(new ExternalTokenGroup(this, subGroup, parent!, "t" + this.tokenGroups.length, subGroup.prec))
    }
  }

  makeTerminal(name: string, tag: string | null, group: TokenSet) {
    for (let i = 0;; i++) {
      let cur = i ? `${name}-${i}` : name
      if (this.terms.terminals.some(t => t.name == cur)) continue
      this.tokens[cur] = group
      return this.terms.makeTerminal(cur, tag)
    }
  }

  stateData(state: LRState) {
    let actions = [], recover = [], forcedReduce = 0, defaultReduce = 0
    if (state.actions.length) {
      let first = state.actions[0] as Reduce
      if (state.actions.every(a => a instanceof Reduce && a.rule == first.rule))
        defaultReduce = reduce(first.rule)
    }
    for (let action of state.actions) {
      let value = action instanceof Shift ? -action.target.id : reduce(action.rule)
      if (value != defaultReduce) actions.push(action.term.id, value)
    }

    for (let action of state.recover)
      recover.push(action.term.id, action.target.id)
    let positions = state.set.filter(p => p.pos > 0)
    if (positions.length) {
      let defaultPos = positions.reduce((a, b) => a.pos - b.pos || b.rule.parts.length - a.rule.parts.length < 0 ? b : a)
      forcedReduce = (defaultPos.rule.name.id << REDUCE_DEPTH_SIZE) | (defaultPos.pos + 1)
    }
    let {skip, tokenizers: tok} = this.tokensForState(state)
    return {actions, recover, defaultReduce, forcedReduce, skip, tokenizers: tok}
  }

  tokensForState(state: LRState): {skip: TokenSet | null, tokenizers: TokenSet[]} {
    let found: TokenSet[] = [], skip: TokenSet | null = null
    let add = (group: TokenSet) => {
      let curSkip = group.hasSkip
      if (curSkip) {
        if (skip && skip != curSkip)
          this.raise(`Inconsistent skip rules for state ${state.set.filter(p => p.pos > 0).join() || "start"}`)
        skip = curSkip
      }
      if (!found.includes(group)) found.push(group)
    }
    for (let action of state.actions) {
      let group = this.tokens[action.term.name]
      if (group) add(group)
    }
    if (found.length == 0) add(this.tokenGroups[0])
    return {skip, tokenizers: found.sort((a, b) => b.prec - a.prec)}
  }

  substituteArgs(expr: Expression, args: readonly Expression[], params: readonly Identifier[]) {
    if (args.length == 0) return expr
    return expr.walk(expr => {
      let found
      if (expr instanceof NamedExpression && !expr.namespace &&
          (found = params.findIndex(p => p.name == expr.id.name)) > -1) {
        let arg = args[found]
        if (expr.args.length) {
          if (arg instanceof NamedExpression && !arg.args.length)
            return new NamedExpression(expr.start, arg.namespace, arg.id, expr.args)
          this.raise(`Passing arguments to a parameter that already has arguments`, expr.start)
        }
        return arg
      }
      return expr
    })
  }

  conflictsFor(markers: readonly ConflictMarker[]) {
    let here = Conflicts.none, atEnd = Conflicts.none
    for (let marker of markers) {
      if (marker.type == "ambig") {
        here = here.join(new Conflicts(0, [marker.id.name]))
      } else {
        let precs = this.ast.precedences!
        let index = precs ? precs.items.findIndex(item => item.id.name == marker.id.name) : -1
        if (index < 0) this.raise(`Reference to unknown precedence: '${marker.id.name}'`, marker.id.start)
        let prec = precs.items[index], value = precs.items.length - index
        if (prec.type == "cut") {
          here = here.join(new Conflicts(0, none, value))
        } else {
          here = here.join(new Conflicts(value << 2))
          atEnd = atEnd.join(new Conflicts((value << 2) + (prec.type == "left" ? 1 : prec.type == "right" ? -1 : 0)))
        }
      }
    }
    return {here, atEnd}
  }

  raise(message: string, pos = 1): never {
    return this.input.raise(message, pos)
  }

  warn(message: string, pos = -1) {
    let msg = this.input.message(message, pos)
    if (this.options.warn) this.options.warn(msg)
    else console.warn(msg)
  }

  defineRule(name: Term, choices: Parts[]) {
    for (let choice of choices)
      this.rules.push(new Rule(name, choice.terms, choice.ensureConflicts()))
    return name
  }

  resolve(expr: NamedExpression): Parts[] {
    if (expr.namespace) {
      let ns = this.namespaces[expr.namespace.name]
      if (!ns)
        this.raise(`Reference to undefined namespace '${expr.namespace.name}'`, expr.start)
      return ns.resolve(expr, this)
    } else if (expr.id.name == "specialize" || expr.id.name == "replace" || expr.id.name == "extend") {
      return [p(this.resolveSpecialization(expr, expr.id.name))]
    } else {
      for (let built of this.built) if (built.matches(expr)) return [p(built.term)]

      for (let tokens of this.tokenGroups) {
        let found = tokens.getToken(expr)
        if (found) return [p(found)]
      }

      let known = this.ast.rules.find(r => r.id.name == expr.id.name)
      if (!known)
        return this.raise(`Reference to undefined rule '${expr.id.name}'`, expr.start)
      if (known.params.length != expr.args.length)
        this.raise(`Wrong number or arguments for '${expr.id.name}'`, expr.start)
      return [p(this.buildRule(known, expr.args))]
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
  normalizeRepeat(expr: RepeatExpression) {
    let known = this.built.find(b => b.matchesRepeat(expr))
    if (known) return p(known.term)

    let name = expr.expr instanceof SequenceExpression || expr.expr instanceof ChoiceExpression ? `(${expr.expr})${expr.kind}` : expr.toString()
    let inner = this.newName(name + "-inner", true)
    inner.repeated = true
    let outer = this.newName(name, true, inner)
    this.built.push(new BuiltRule(expr.kind, [expr.expr], outer))

    let top = this.normalizeExpr(expr.expr)
    top.push(new Parts([inner, inner], [Conflicts.none, new Conflicts(PREC_REPEAT, none), Conflicts.none]))
    this.defineRule(inner, top)
    this.defineRule(outer, expr.kind == "+" ? [p(inner)] : [Parts.none, p(inner)])

    return p(outer)
  }

  normalizeSequence(expr: SequenceExpression) {
    let result: Parts[][] = expr.exprs.map(e => this.normalizeExpr(e))
    let builder = this
    function complete(start: Parts, from: number, endConflicts: Conflicts): Parts[] {
      let {here, atEnd} = builder.conflictsFor(expr.markers[from])
      if (from == result.length)
        return [start.withConflicts(start.terms.length, here.join(endConflicts))]
      let choices = []
      for (let choice of result[from]) {
        for (let full of complete(start.concat(choice).withConflicts(start.terms.length, here),
                                  from + 1, endConflicts.join(atEnd)))
          choices.push(full)
      }
      return choices
    }
    return complete(Parts.none, 0, Conflicts.none)
  }

  normalizeExpr(expr: Expression): Parts[] {
    if (expr instanceof RepeatExpression && expr.kind == "?") {
      return [Parts.none, ...this.normalizeExpr(expr.expr)]
    } else if (expr instanceof RepeatExpression) {
      return [this.normalizeRepeat(expr)]
    } else if (expr instanceof ChoiceExpression) {
      return expr.exprs.reduce((o, e) => o.concat(this.normalizeExpr(e)), [] as Parts[])
    } else if (expr instanceof SequenceExpression) {
      return this.normalizeSequence(expr)
    } else if (expr instanceof LiteralExpression) {
      return [p(this.tokenGroups[0].getLiteral(expr)!)]
    } else if (expr instanceof NamedExpression) {
      return this.resolve(expr)
    } else {
      return this.raise("This type of expression may not occur in non-token rules", expr.start)
    }
  }

  buildRule(rule: RuleDeclaration, args: readonly Expression[]): Term {
    let expr = this.substituteArgs(rule.expr, args, rule.params)
    this.used[rule.id.name] = true
    let name = this.newName(rule.id.name + (args.length ? "<" + args.join(",") + ">" : ""),
                            rule.tag ? rule.tag.name : isTag(rule.id.name) || true)
    if (args.length == 0) this.namedTerms[rule.id.name] = name
    this.built.push(new BuiltRule(rule.id.name, args, name))
    return this.defineRule(name, this.normalizeExpr(expr))
  }

  resolveSpecialization(expr: NamedExpression, type: string) {
    if (expr.args.length < 2 || expr.args.length > 3) this.raise(`'${type}' takes two or three arguments`, expr.start)
    if (!(expr.args[1] instanceof LiteralExpression))
      this.raise(`The second argument to '${type}' must be a literal`, expr.args[1].start)
    let tag = null
    if (expr.args.length == 3) {
      let tagArg = expr.args[2]
      if (!(tagArg instanceof NamedExpression) || tagArg.args.length)
        return this.raise(`The third argument to '${type}' must be a name (without arguments)`)
      tag = tagArg.id.name
    }
    let terminal = this.normalizeExpr(expr.args[0])
    if (terminal.length != 1 || terminal[0].terms.length != 1 || !terminal[0].terms[0].terminal)
      this.raise(`The first argument to '${type}' must resolve to a token`, expr.args[0].start)
    let term = terminal[0].terms[0], value = (expr.args[1] as LiteralExpression).value
    let table = this.specialized[term.name] || (this.specialized[term.name] = [])
    let known = table.find(sp => sp.value == value), token
    if (known == null) {
      token = this.makeTerminal(JSON.stringify(value), tag, this.tokens[term.name])
      table.push({value, term: token, type})
    } else {
      if (known.type != type)
        this.raise(`Conflicting specialization types for ${JSON.stringify(value)} of ${term.name} (${type} vs ${known.type})`, expr.start)
      token = known.term
    }
    return token
  }
}

function reduce(rule: Rule) {
  return (rule.name.id << REDUCE_DEPTH_SIZE) | (rule.parts.length + 1)
}

function computeGotoTables(states: readonly LRState[]) {
  let goto: {[term: number]: {[to: number]: number[]}} = {}
  for (let state of states)
    for (let entry of state.goto) {
      let set = goto[entry.term.id] || (goto[entry.term.id] = {})
      ;(set[entry.target.id] || (set[entry.target.id] = [])).push(state.id)
    }
  let taggedGoto: number[][] = [[]] // Empty spot for TERM_ERROR
  let untaggedGoto: number[][] = []
  for (let term in goto) {
    let entries = goto[term], max = -1
    for (let target in entries) {
      let sources = entries[target]
      if (max < 0 || sources.length > entries[max].length) max = +target
    }
    let assoc: number[] = []
    for (let target in entries) if (+target != max) {
      for (let source of entries[target]) assoc.push(+source, +target)
    }
    assoc.push(-1, max)
    let table = +term & TERM_TAGGED ? taggedGoto : untaggedGoto
    table[+term >> 1] = assoc
  }
  return {taggedGoto, untaggedGoto}
}

interface Namespace {
  resolve(expr: NamedExpression, builder: Builder): Parts[]
}

class TagNamespace implements Namespace {
  resolve(expr: NamedExpression, builder: Builder): Parts[] {
    if (expr.args.length != 1)
      builder.raise(`Tag wrappers take a single argument`, expr.start)
    let tag = expr.id.name
    let name = builder.newName(`tag.${tag}`, tag)
    return [p(builder.defineRule(name, builder.normalizeExpr(expr.args[0])))]
  }
}

class TokenArg {
  constructor(readonly name: string, readonly expr: Expression, readonly scope: readonly TokenArg[]) {}
}

class BuildingRule {
  constructor(readonly name: string, readonly start: State, readonly to: State, readonly args: readonly Expression[]) {}
}

abstract class TokenSet {
  constructor(readonly parent: TokenGroup | null, readonly id: string, readonly prec: number) {}
  abstract getToken(expr: NamedExpression): Term | null
  getLiteral(expr: LiteralExpression): Term | null { return null }
  checkUnused() {}
  get hasSkip(): TokenSet | null { return null }
  skip(): Tokenizer | null { return null }
  abstract tokenizer(terms: {[name: string]: number}): Tokenizer | null
  abstract source(moduleStyle: string): string
}

class TokenGroup extends TokenSet {
  startState: State = new State
  skipState: State | null = null
  built: BuiltRule[] = []
  used: {[name: string]: boolean} = Object.create(null)
  building: BuildingRule[] = [] // Used for recursion check

  constructor(readonly b: Builder,
              readonly rules: readonly RuleDeclaration[],
              parent: TokenGroup | null, id: string, prec: number) {
    super(parent, id, prec)
    for (let rule of rules) if (rule.id.name != "skip") this.b.unique(rule.id)
    let skip = rules.find(r => r.id.name == "skip")
    if (skip) {
      this.used.skip = true
      if (skip.params.length) return this.b.raise("Skip rules should not take parameters", skip.params[0].start)
      this.skipState = new State
      let nameless = new State(b.terms.eof)
      for (let choice of skip.expr instanceof ChoiceExpression ? skip.expr.exprs : [skip.expr]) {
        let tag = null
        if (choice instanceof NamedExpression) {
          let rule = this.rules.find(r => r.id.name == (choice as NamedExpression).id.name)
          if (rule) tag = rule.tag ? rule.tag.name : isTag(rule.id.name)
        }
        this.build(choice, this.skipState, tag ? new State(this.b.makeTerminal(tag, tag, this)) : nameless, none)
      }
    }
  }

  getToken(expr: NamedExpression) {
    for (let built of this.built) if (built.matches(expr)) return built.term
    let name = expr.id.name
    let rule = this.rules.find(r => r.id.name == name)
    if (!rule) return null
    let term = this.b.makeTerminal(expr.toString(), rule.tag ? rule.tag.name : isTag(name), this)
    if (expr.args.length == 0) this.b.namedTerms[expr.id.name] = term
    this.buildRule(rule, expr, this.startState, new State(term))
    this.built.push(new BuiltRule(name, expr.args, term))
    return term
  }

  getLiteral(expr: LiteralExpression) {
    let id = JSON.stringify(expr.value)
    for (let built of this.built) if (built.id == id) return built.term
    let term = this.b.makeTerminal(id, null, this)
    this.build(expr, this.startState, new State(term), none)
    this.built.push(new BuiltRule(id, none, term))
    return term
  }

  defines(term: Term): boolean {
    return this.built.some(b => b.term == term)
  }

  buildRule(rule: RuleDeclaration, expr: NamedExpression, from: State, to: State, args: readonly TokenArg[] = none) {
    let name = expr.id.name
    if (rule.params.length != expr.args.length)
      this.b.raise(`Incorrect number of arguments for token '${name}'`, expr.start)
    let building = this.building.find(b => b.name == name && exprsEq(expr.args, b.args))
    if (building) {
      if (building.to == to) {
        from.nullEdge(building.start)
        return
      }
      let lastIndex = this.building.length - 1
      while (this.building[lastIndex].name != name) lastIndex--
      this.b.raise(`Invalid (non-tail) recursion in token rules: ${
        this.building.slice(lastIndex).map(b => b.name).join(" -> ")}`, expr.start)
    }
    this.used[name] = true
    let start = new State
    from.nullEdge(start)
    this.building.push(new BuildingRule(name, start, to, expr.args))
    this.build(this.b.substituteArgs(rule.expr, expr.args, rule.params), start, to,
               expr.args.map((e, i) => new TokenArg(rule!.params[i].name, e, args)))
    this.building.pop()
  }

  build(expr: Expression, from: State, to: State, args: readonly TokenArg[]): void {
    if (expr instanceof NamedExpression) {
      if (expr.namespace) {
        if (expr.namespace.name == "std") return this.buildStd(expr, from, to)
        this.b.raise(`Unknown namespace '${expr.namespace.name}'`, expr.start)
      }
      let name = expr.id.name, arg = args.find(a => a.name == name)
      if (arg) return this.build(arg.expr, from, to, arg.scope)
      let rule: RuleDeclaration | undefined = undefined
      for (let scope: TokenGroup | null = this; scope && !rule; scope = scope.parent)
        rule = scope.rules.find(r => r.id.name == name)
      if (!rule) return this.b.raise(`Reference to rule '${expr.id.name}', which isn't found in this token group`, expr.start)
      this.buildRule(rule, expr, from, to, args)
    } else if (expr instanceof ChoiceExpression) {
      for (let choice of expr.exprs) this.build(choice, from, to, args)
    } else if (expr instanceof SequenceExpression && !expr.exprs.length) {
      from.nullEdge(to)
    } else if (expr instanceof SequenceExpression) {
      let conflict = expr.markers.find(c => c.length > 0)
      if (conflict) this.b.raise("Conflict marker in token expression", conflict[0].start)
      for (let i = 0; i < expr.exprs.length; i++) {
        let next = i == expr.exprs.length - 1 ? to : new State
        this.build(expr.exprs[i], from, next, args)
        from = next
      }
    } else if (expr instanceof RepeatExpression) {
      if (expr.kind == "*") {
        let loop = new State
        from.nullEdge(loop)
        this.build(expr.expr, loop, loop, args)
        loop.nullEdge(to)
      } else if (expr.kind == "+") {
        let loop = new State
        this.build(expr.expr, from, loop, args)
        this.build(expr.expr, loop, loop, args)
        loop.nullEdge(to)
      } else { // expr.kind == "?"
        from.nullEdge(to)
        this.build(expr.expr, from, to, args)
      }
    } else if (expr instanceof SetExpression) {
      for (let [a, b] of expr.inverted ? invertRanges(expr.ranges) : expr.ranges)
        rangeEdges(from, to, a, b)
    } else if (expr instanceof LiteralExpression) {
      for (let i = 0; i < expr.value.length; i++) {
        let ch = expr.value.charCodeAt(i)
        let next = i == expr.value.length - 1 ? to : new State
        from.edge(ch, ch + 1, next)
        from = next
      }
    } else if (expr instanceof AnyExpression) {
      from.edge(0, MAX_CHAR + 1, to)
    } else {
      return this.b.raise(`Unrecognized expression type in token`, (expr as any).start)
    }
  }

  buildStd(expr: NamedExpression, from: State, to: State) {
    if (expr.args.length) this.b.raise(`'std.${expr.id.name}' does not take arguments`, expr.args[0].start)
    if (!STD_RANGES.hasOwnProperty(expr.id.name)) this.b.raise(`There is no builtin rule 'std.${expr.id.name}'`, expr.start)
    for (let [a, b] of STD_RANGES[expr.id.name]) from.edge(a, b, to)
  }

  checkUnused() {
    for (let rule of this.rules) if (!this.used[rule.id.name])
      this.b.warn(`Unused token rule '${rule.id.name}'`, rule.start)
  }

  get hasSkip(): TokenSet | null {
    return this.skipState != null ? this : this.parent ? this.parent.hasSkip : null
  }

  get skipSource(): string | null {
    if (!this.skipState) return this.parent ? this.parent.skipSource : null
    let compiled = this.skipState.compile(), source = compiled.toSource()
    if (!source) return null
    if (/\bskip\b/.test(verbose)) console.log(compiled.toString())
    return source
  }

  skip() {
    let src = this.skipSource
    return src ? new Tokenizer((1, eval)("(" + src + ")")) : null
  }

  get tokenizerSource() {
    let startState = this.startState.compile()
    let source = startState.toSource()
    if (!source) return null
    if (startState.accepting)
      this.b.raise(`Grammar contains zero-length tokens (in '${startState.accepting.name}')`,
                   this.rules.find(r => r.id.name == startState.accepting!.name)!.start)
    if (/\btokens\b/.test(verbose)) console.log(startState.toString())
    return source
  }

  tokenizer() {
    let src = this.tokenizerSource
    return src ? new Tokenizer((1, eval)("(" + src + ")")).withPrec(this.prec) : null
  }

  source() {
    let skip = this.skipSource, tok = this.tokenizerSource
    return (skip ? `const ${this.id}s = new Tokenizer(${skip})\n` : "") +
      (tok ? `const ${this.id} = new Tokenizer(${tok})${this.prec != 2 ? `.withPrec(${this.prec})` : ""}\n` : "")
  }
}

class ExternalTokenGroup extends TokenSet {
  name: string
  from: string
  terms: Term[] = []

  constructor(readonly b: Builder,
              decl: ExternalTokenGroupDeclaration,
              parent: TokenGroup, id: string, prec: number) {
    super(parent, id, prec)
    this.name = decl.id.name
    this.from = decl.source
    for (let item of decl.items) {
      b.unique(item)
      this.terms.push(b.namedTerms[item.name] = b.makeTerminal(item.name, null, this))
    }
  }

  getToken(expr: NamedExpression) {
    let found = this.terms.find(t => t.name == expr.id.name)
    if (!found) return null
    if (expr.args.length) this.b.raise(`External tokens can't take arguments`, expr.start)
    return found
  }

  tokenizer(terms: {[name: string]: number}) {
    if (!this.b.options.externalTokenizer) throw new Error("No externalTokenizer option given")
    return this.b.options.externalTokenizer(this.name, terms).withPrec(this.prec)
  }

  source(style: string) {
    let iName = this.prec != 2 ? this.id + "i" : this.id
    let importLine = style == "es6" ? `import {${this.name} as ${iName}} from ${JSON.stringify(this.from)}\n`
      : `const {${this.name}: ${iName}} = require(${JSON.stringify(this.from)})\n`
    return this.prec == 2 ? importLine : importLine + `const ${this.id} = ${this.id}i.withPrec(${this.prec})\n`
  }
}

function invertRanges(ranges: [number, number][]) {
  let pos = 0, result: [number, number][] = []
  for (let [a, b] of ranges) {
    if (a > pos) result.push([pos, a])
    pos = b
  }
  if (pos <= MAX_CODE) result.push([pos, MAX_CODE + 1])
  return result
}

const ASTRAL = 0x10000, GAP_START = 0xd800, GAP_END = 0xe000, MAX_CODE = 0x10ffff
const LOW_SURR_B = 0xdc00, HIGH_SURR_B = 0xdfff

// Create intermediate states for astral characters in a range, if
// necessary, since the tokenizer acts on UTF16 characters
function rangeEdges(from: State, to: State, low: number, hi: number) {
  if (low < GAP_START && hi == MAX_CODE + 1) {
    from.edge(low, MAX_CHAR + 1, to)
    return
  }

  if (low < ASTRAL) {
    if (low < GAP_START) from.edge(low, Math.min(hi, GAP_START), to)
    if (hi > GAP_END) from.edge(Math.max(low, GAP_END), Math.min(hi, MAX_CHAR + 1), to)
    low = ASTRAL
  }
  if (hi < ASTRAL) return

  let lowStr = String.fromCodePoint(low), hiStr = String.fromCodePoint(hi - 1)
  let lowA = lowStr.charCodeAt(0), lowB = lowStr.charCodeAt(1)
  let hiA = hiStr.charCodeAt(0), hiB = hiStr.charCodeAt(1)
  if (lowA == hiA) { // Share the first char code
    let hop = new State
    from.edge(lowA, lowA + 1, hop)
    hop.edge(lowB, hiB + 1, to)
  } else {
    let midStart = lowA, midEnd = hiA
    if (lowB > LOW_SURR_B) {
      midStart++
      let hop = new State
      from.edge(lowA, lowA + 1, hop)
      hop.edge(lowB, HIGH_SURR_B + 1, to)
    }
    if (hiB < HIGH_SURR_B) {
      midEnd--
      let hop = new State
      from.edge(hiA, hiA + 1, hop)
      hop.edge(LOW_SURR_B, hiB + 1, to)
    }
    if (midStart <= midEnd) {
      let hop = new State
      from.edge(midStart, midEnd + 1, hop)
      hop.edge(LOW_SURR_B, HIGH_SURR_B + 1, to)
    }
  }
}

const STD_RANGES: {[name: string]: [number, number][]} = {
  asciiLetter: [[65, 91], [97, 123]],
  asciiLowercase: [[97, 123]],
  asciiUppercase: [[65, 91]],
  digit: [[48, 58]],
  whitespace: [[9, 14], [32, 33], [133, 134], [160, 161], [5760, 5761], [8192, 8203],
               [8232, 8234], [8239, 8240], [8287, 8288], [12288, 12289]]
}

// FIXME maybe add a pass that, if there's a tagless token whole only
// use is in a tagged single-term rule, move the tag to the token and
// collapse the rule.

function inlineRules(rules: readonly Rule[]): readonly Rule[] {
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
      let conflicts = [rule.conflicts[0]], parts = []
      for (let i = 0; i < rule.parts.length; i++) {
        let replace = inlinable[rule.parts[i].name]
        if (replace) {
          conflicts[conflicts.length - 1] = conflicts[conflicts.length - 1].join(replace.conflicts[0])
          for (let j = 0; j < replace.parts.length; j++) {
            parts.push(replace.parts[j])
            conflicts.push(replace.conflicts[j + 1])
          }
          conflicts[conflicts.length - 1] = conflicts[conflicts.length - 1].join(rule.conflicts[i + 1])
        } else {
          parts.push(rule.parts[i])
          conflicts.push(rule.conflicts[i + 1])
        }
      }
      newRules.push(new Rule(rule.name, parts, conflicts))
    }
    rules = newRules
  }
}

function mergeRules(rules: readonly Rule[]): readonly Rule[] {
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
        if (a.cmpNoName(b) != 0) match = false
      }
      if (match) found = merged[name.name] = otherName
    }
  }
  if (!found) return rules
  let newRules = []
  for (let rule of rules) if (!merged[rule.name.name]) {
    newRules.push(rule.parts.every(p => !merged[p.name]) ? rule :
                  new Rule(rule.name, rule.parts.map(p => merged[p.name] || p), rule.conflicts))
  }
  return newRules
}

function simplifyRules(rules: readonly Rule[]): readonly Rule[] {
  return mergeRules(inlineRules(rules))
}

export function buildParser(text: string, options: BuildOptions = {}): Parser {
  return new Builder(text, options).getParser()
}

export function buildParserFile(text: string, options: BuildOptions = {}): string {
  return new Builder(text, options).getParserString()
}
