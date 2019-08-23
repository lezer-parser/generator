import {GrammarDeclaration, RuleDeclaration, TokenDeclaration, ExternalTokenDeclaration,
        Expression, Identifier, LiteralExpression, NameExpression, SequenceExpression,
        ChoiceExpression, RepeatExpression, SetExpression, AnyExpression, ConflictMarker,
        InlineRuleExpression, SpecializeExpression, Prop, PropPart,
        exprsEq, exprEq} from "./node"
import {Term, TermSet, PREC_REPEAT, Rule, Conflicts, Props, noProps} from "./grammar"
import {State, MAX_CHAR} from "./token"
import {Input} from "./parse"
import {computeFirstSets, buildFullAutomaton, finishAutomaton, State as LRState, Shift, Reduce} from "./automaton"
import {encodeArray} from "./encode"
import {Parser, TokenGroup as LezerTokenGroup, ExternalTokenizer,
        NestedGrammar, InputStream, Token, Stack, NodeGroup, NodeProp, NodeType} from "lezer"
import {Action, Specialize, StateFlag, Term as T, Seq, ParseState} from "lezer/src/constants"

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

class BuiltRule {
  constructor(readonly id: string,
              readonly args: readonly Expression[],
              readonly term: Term) {}

  matches(expr: NameExpression) {
    return this.id == expr.id.name && exprsEq(expr.args, this.args)
  }

  matchesRepeat(expr: RepeatExpression) {
    return this.id == expr.kind && exprEq(expr.expr, this.args[0])
  }
}

export type BuildOptions = {
  /// The name of the grammar file
  fileName?: string,
  /// A function that should be called with warnings. The default is
  /// to call `console.warn`.
  warn?: (message: string) => void,
  /// Whether to include term names in the output file. Defaults to
  /// false.
  includeNames?: boolean,
  /// Determines the module system used by the output file. Can be
  /// either `"cjs"` (CommonJS) or `"es"` (ES2015 module), defaults to
  /// `"es"`.
  moduleStyle?: string,
  /// The name of the export that holds the parser in the output file.
  /// Defaults to `"parser"`.
  exportName?: string,
  /// When calling `buildParser`, this can be used to provide
  /// placeholders for external tokenizers.
  externalTokenizer?: (name: string, terms: {[name: string]: number}) => ExternalTokenizer
  /// Only relevant when using `buildParser`. Provides placeholders
  /// for nested grammars.
  nestedGrammar?: (name: string, terms: {[name: string]: number}) => NestedGrammar
  /// If given, will be used to initialize external props in the parser
  /// returned by `buildParser`.
  externalProp?: (name: string) => NodeProp<any>
}

class Builder {
  ast: GrammarDeclaration
  input: Input
  terms = new TermSet
  tokens: TokenSet
  externalTokens: ExternalTokenSet[]
  nestedGrammars: NestedGrammarSpec[] = []
  specialized: {[name: string]: {value: string, term: Term, type: string}[]} = Object.create(null)
  tokenOrigins: {[name: string]: Term | ExternalTokenSet} = Object.create(null)
  rules: Rule[] = []
  built: BuiltRule[] = []
  ruleNames: {[name: string]: Identifier | null} = Object.create(null)
  namespaces: {[name: string]: Namespace} = Object.create(null)
  namedTerms: {[name: string]: Term} = Object.create(null)
  termTable: {[name: string]: number} = Object.create(null)
  knownProps: {[name: string]: {prop: NodeProp<any>, source: {name: string, from: string | null}}} = Object.create(null)

  astRules: {skip: Term, rule: RuleDeclaration}[] = []
  currentSkip: Term[] = []
  noSkip: Term
  skipRules: Term[] = []

  constructor(text: string, readonly options: BuildOptions) {
    this.input = new Input(text, options.fileName)
    this.ast = this.input.parse()

    let NP: {[key: string]: any} = NodeProp
    for (let prop in NP) {
      if (NP[prop] instanceof NodeProp) this.knownProps[prop] = {prop: NP[prop], source: {name: prop, from: null}}
    }
    for (let prop of this.ast.externalProps) {
      this.knownProps[prop.id.name] = {
        prop: this.options.externalProp ? this.options.externalProp(prop.id.name) : NodeProp.string(),
        source: {name: prop.externalID.name, from: prop.source}
      }
    }

    this.tokens = new TokenSet(this, this.ast.tokens)
    this.externalTokens = this.ast.externalTokens.map(ext => new ExternalTokenSet(this, ext))

    this.defineNamespace("nest", new NestNamespace)

    this.noSkip = this.newName("%noskip", true)
    this.defineRule(this.noSkip, [])

    for (let grammar of this.ast.grammars) {
      if (this.ast.grammars.some(g => g != grammar && g.id.name == grammar.id.name))
        this.raise(`Duplicate external grammar name '${grammar.id.name}'`, grammar.id.start)
    }

    let mainSkip = this.ast.mainSkip ? this.newName("%mainskip", true) : this.noSkip
    let scopedSkip: Term[] = []
    for (let rule of this.ast.rules) this.astRules.push({skip: mainSkip, rule})
    for (let scoped of this.ast.scopedSkip) {
      let skip = this.noSkip, found = this.ast.scopedSkip.findIndex((sc, i) => i < scopedSkip.length && exprEq(sc.expr, scoped.expr))
      if (found > -1) skip = scopedSkip[found]
      else if (this.ast.mainSkip && exprEq(scoped.expr, this.ast.mainSkip)) skip = mainSkip
      else if (!isEmpty(scoped.expr)) skip = this.newName("%skip", true)
      scopedSkip.push(skip)
      for (let rule of scoped.rules) this.astRules.push({skip, rule})
    }

    for (let {rule} of this.astRules) {
      this.unique(rule.id)
      if (this.namespaces[rule.id.name])
        this.raise(`Rule name '${rule.id.name}' conflicts with a defined namespace`, rule.id.start)
    }

    this.currentSkip.push(this.noSkip)
    if (mainSkip != this.noSkip) {
      this.skipRules.push(mainSkip)
      this.defineRule(mainSkip, this.normalizeExpr(this.ast.mainSkip!))
    }
    for (let i = 0; i < this.ast.scopedSkip.length; i++) {
      let skip = scopedSkip[i]
      if (skip != this.noSkip && skip != mainSkip && (i == 0 || scopedSkip.lastIndexOf(skip, i - 1) == -1)) {
        this.skipRules.push(scopedSkip[i])
        this.defineRule(scopedSkip[i], this.normalizeExpr(this.ast.scopedSkip[i].expr))
      }
    }
    this.currentSkip.pop()

    this.currentSkip.push(mainSkip)
    let top = this.ast.topRule
    let {name, props} = this.nodeInfo(top.props, null, none, none, top.expr)
    this.defineRule(this.terms.makeTop(name, props), this.normalizeExpr(top.expr))
    this.currentSkip.pop()

    for (let rule of this.ast.rules) {
      if (rule.exported && this.ruleNames[rule.id.name] && rule.params.length == 0) {
        let {name, props} = this.nodeInfo(rule.props, rule.id.name)
        let term = this.namedTerms[rule.id.name] = this.newName(rule.id.name, name, props)
        term.preserve = true
        if (rule.expr instanceof SequenceExpression && rule.expr.exprs.length == 0)
          this.used(rule.id.name)
      }
    }

    for (let name in this.ruleNames) {
      let value = this.ruleNames[name]
      if (value) this.warn(`Unused rule '${value.name}'`, value.start)
    }

    this.tokens.takePrecedences()
  }

  unique(id: Identifier) {
    if (id.name in this.ruleNames)
      this.raise(`Duplicate definition of rule '${id.name}'`, id.start)
    this.ruleNames[id.name] = id
  }

  used(name: string) {
    this.ruleNames[name] = null
  }

  defineNamespace(name: string, value: Namespace, pos: number = 0) {
    if (this.namespaces[name]) this.raise(`Duplicate definition of namespace '${name}'`, pos)
    this.namespaces[name] = value
  }

  newName(base: string, nodeName: string | null | true = null, props: Props = noProps): Term {
    for (let i = nodeName ? 0 : 1;; i++) {
      let name = i ? `${base}-${i}` : base
      if (!this.terms.names[name])
        return this.terms.makeNonTerminal(name, nodeName === true ? null : nodeName, props)
    }
  }

  getParser() {
    let rules = simplifyRules(this.rules, [...this.skipRules, ...this.nestedGrammars.map(g => g.placeholder), this.terms.top])
    let {nodeTypes, names} = this.terms.finish(rules)
    for (let prop in this.namedTerms) this.termTable[prop] = this.namedTerms[prop].id

    if (/\bgrammar\b/.test(verbose)) console.log(rules.join("\n"))

    let first = computeFirstSets(this.terms)
    let fullSkipAutomata = this.skipRules.map(name => name.rules.some(r => r.parts.length > 0) ? buildFullAutomaton(this.terms, name, first) : null)
    let fullTable = buildFullAutomaton(this.terms, this.terms.top, first)
    let {tokenMasks, tokenGroups, tokenPrec} = this.tokens.buildTokenGroups(fullTable, fullSkipAutomata)
    let table = finishAutomaton(fullTable, first) as LRState[]
    let firstSkipState = table.length + 1
    // Merge states generated by skip expressions into the table
    let skipStartStates = fullSkipAutomata.map((states, i) => {
      if (!states) return null
      // If this skip expression involves actual states (as opposed to just some single-token actions), add them
      if (!states[0].actions.every(a => isSimpleSkip(a, this.skipRules[i]))) {
        let offset = table.length
        for (let state of states) {
          table.push(state)
          state.partOfSkip = this.skipRules[i]
          state.id += offset
        }
      }
      return states[0]
    })

    this.addNestedGrammars(table)

    if (/\blr\b/.test(verbose)) console.log(table.join("\n"))
    let specialized = [], specializations = []
    for (let name in this.specialized) {
      specialized.push(this.terms.names[name]!.id)
      let table: {[value: string]: number} = {}
      for (let {value, term, type} of this.specialized[name]) {
        let code = type == "specialize" ? Specialize.Specialize : Specialize.Extend
        table[value] = (term.id << 1) | code
      }
      specializations.push(table)
    }
    specialized.push(Seq.End)

    let tokenData = this.tokens.tokenizer(tokenMasks, tokenPrec)
    let tokStart = (tokenizer: TempExternalTokenizer | LezerTokenGroup) => {
      if (tokenizer instanceof TempExternalTokenizer) return tokenizer.set.ast.start
      return this.tokens.ast ? this.tokens.ast.start : -1
    }
    let tokenizers = 
      (tokenGroups.map(g => new LezerTokenGroup(tokenData, g.id)) as (LezerTokenGroup | TempExternalTokenizer)[])
      .concat(this.externalTokens.map(ext => new TempExternalTokenizer(ext, this.termTable)))
      .sort((a, b) => tokStart(a) - tokStart(b))

    let data = new DataBuilder
    let skipData = skipStartStates.map((state, i) => {
      let actions: number[] = []
      if (state) {
        for (let action of state.actions as Shift[]) {
          if (isSimpleSkip(action, this.skipRules[i]))
            actions.push(action.term.id, 0, Action.StayFlag >> 16)
          else
            actions.push(action.term.id, state.id, Action.GotoFlag >> 16)
        }
        // No need to store simple skip actions in the skip start
        // state—they'll never be accessed, since the STAY_FLAG action
        // avoids state changes entirely.
        state.actions = state.actions.filter(a => !isSimpleSkip(a, this.skipRules[i]))
      }
      actions.push(Seq.End)
      return data.storeArray(actions)
    })
    let noSkip = data.storeArray([Seq.End])
    let states = new Uint32Array(table.length * ParseState.Size)
    for (let s of table) {
      let skip = noSkip, skipState = null
      if (s.skip != this.noSkip) {
        let index = this.skipRules.indexOf(s.skip)
        skip = skipData[index]
        skipState = skipStartStates[index]
      }
      this.finishState(s, tokenizers, data, skip, skipState, s.id >= firstSkipState, states)
    }

    let nested = this.nestedGrammars.map(g => ({
      name: g.name,
      grammar: tempNestedGrammar(this, g),
      end: new LezerTokenGroup(g.end.compile().toArray({}, none), 0),
      placeholder: g.placeholder.id
    }))

    let skipped = this.gatherSkippedTerms()
    let group = new NodeGroup(nodeTypes.map((term, i) => this.toNodeType(term, skipped, i)))
    
    let precTable = data.storeArray(tokenPrec.concat(Seq.End))
    let specTable = data.storeArray(specialized)
    return new Parser(states, data.finish(), computeGotoTable(table), group,
                      tokenizers, nested,
                      specTable, specializations, precTable, names)
  }

  toNodeType(term: Term, skipped: readonly Term[], id: number) {
    let propData: any[] = []
    let props = {propData} as {[id: number]: any}
    if (skipped.includes(term)) NodeProp.skipped.set(props, true)
    for (let prop in term.props) {
      let propType = this.knownProps[prop]
      if (!propType) throw new Error("No known prop type for " + prop)
      propType.prop.set(props, propType.prop.deserialize(term.props[prop]))
      propData.push(propType.source, term.props[prop])
    }
    return new NodeType(term.nodeName || "", props, id)
  }

  addNestedGrammars(table: LRState[]) {
    for (let state of table) {
      let nest = state.set.filter(pos => this.nestedGrammars.some(g => g.placeholder == pos.next))
      if (nest.length) {
        let placeholder = nest[0].next
        if (!nest.every(pos => pos.next == placeholder))
          this.raise(`Multiple nested grammars possible after ${nest[0].trail()}`)
        if (!state.set.every(pos => pos.next == placeholder || (pos.pos == 0 && state.set.some(p => p.next == pos.rule.name))))
          this.raise(`Nested grammar in ambiguous position after ${nest[0].trail()} ` + state.set)
        state.nested = this.nestedGrammars.findIndex(g => g.placeholder == placeholder)
      }
    }
  }

  makeTerminal(name: string, tag: string | null, props: Props) {
    for (let i = 0;; i++) {
      let cur = i ? `${name}-${i}` : name
      if (this.terms.names[cur]) continue
      return this.terms.makeTerminal(cur, tag, props)
    }
  }

  gatherSkippedTerms() {
    let terms: Term[] = this.skipRules.slice()
    for (let i = 0; i < terms.length; i++) {
      for (let rule of terms[i].rules) {
        for (let part of rule.parts) if (part.nodeType && !terms.includes(part)) terms.push(part)
      }
    }
    return terms
  }

  finishState(state: LRState, tokenizers: (LezerTokenGroup | TempExternalTokenizer)[],
              data: DataBuilder, skipTable: number, skipState: LRState | null, isSkip: boolean,
              stateArray: Uint32Array) {
    let actions = [], recover = [], forcedReduce = 0
    let defaultReduce = state.defaultReduce ? reduceAction(state.defaultReduce, state.partOfSkip) : 0
    let flags = (isSkip ? StateFlag.Skipped : 0) |
      (state.nested > -1 ? StateFlag.StartNest | (state.nested << StateFlag.NestShift) : 0)

    let other = -1
    if (defaultReduce == 0) for (let action of state.actions) {
      if (action instanceof Shift) {
        actions.push(action.term.id, action.target.id, 0)
      } else {
        let code = reduceAction(action.rule, state.partOfSkip)
        if (state.partOfSkip && action.term.eof) other = code
        else actions.push(action.term.id, code & Action.ValueMask, code >> 16)
      }
    }
    if (other > -1) actions.push(T.Err, other & Action.ValueMask, other >> 16)
    actions.push(Seq.End)

    for (let action of state.recover)
      recover.push(action.term.id, action.target.id)
    recover.push(Seq.End)

    let forceCandidates = state.set.filter(pos => pos.pos > 0)
    if (forceCandidates.length) {
      let defaultPos = forceCandidates.reduce((a, b) => (b.depth - a.depth || a.pos - b.pos ||
                                                         b.rule.parts.length - a.rule.parts.length) < 0 ? b : a)
      if (!defaultPos.rule.name.top)
        forcedReduce = reduceAction(defaultPos.rule, state.partOfSkip, defaultPos.pos)
    }
    if (state.set.some(p => p.rule.name.top && p.pos == p.rule.parts.length)) flags |= StateFlag.Accepting

    let external: ExternalTokenSet[] = []
    for (let {term} of state.actions) {
      for (;;) {
        let orig = this.tokenOrigins[term.name]
        if (orig instanceof Term) { term = orig; continue }
        if (orig instanceof ExternalTokenSet) addToSet(external, orig)
        break
      }
    }
    external.sort((a, b) => a.ast.start - b.ast.start)
    let tokenizerMask = 0
    for (let i = 0; i < tokenizers.length; i++) {
      let tok = tokenizers[i]
      if (tok instanceof TempExternalTokenizer ? external.includes(tok.set) : tok.id == state.tokenGroup)
        tokenizerMask |= (1 << i)
    }

    let base = state.id * ParseState.Size
    stateArray[base + ParseState.Flags] = flags
    stateArray[base + ParseState.Actions] = data.storeArray(actions)
    stateArray[base + ParseState.Recover] = data.storeArray(recover)
    stateArray[base + ParseState.Skip] = skipTable
    stateArray[base + ParseState.TokenizerMask] = tokenizerMask
    stateArray[base + ParseState.DefaultReduce] = defaultReduce
    stateArray[base + ParseState.ForcedReduce] = forcedReduce
  }

  substituteArgs(expr: Expression, args: readonly Expression[], params: readonly Identifier[]) {
    if (args.length == 0) return expr
    return expr.walk(expr => {
      let found
      if (expr instanceof NameExpression && !expr.namespace &&
          (found = params.findIndex(p => p.name == expr.id.name)) > -1) {
        let arg = args[found]
        if (expr.args.length) {
          if (arg instanceof NameExpression && !arg.args.length)
            return new NameExpression(expr.start, arg.namespace, arg.id, expr.args)
          this.raise(`Passing arguments to a parameter that already has arguments`, expr.start)
        }
        return arg
      } else if (expr instanceof InlineRuleExpression) {
        let r = expr.rule, props = this.substituteArgsInProps(r.props, args, params)
        return props == r.props ? expr :
          new InlineRuleExpression(expr.start, new RuleDeclaration(r.start, r.id, r.exported, props, r.params, r.expr))
      } else if (expr instanceof SpecializeExpression) {
        let props = this.substituteArgsInProps(expr.props, args, params)
        return props == expr.props ? expr :
          new SpecializeExpression(expr.start, expr.type, props, expr.token, expr.content)
      }
      return expr
    })
  }

  substituteArgsInProps(props: readonly Prop[], args: readonly Expression[], params: readonly Identifier[]) {
    let substituteInValue = (value: readonly PropPart[]) => {
      let result = value as PropPart[]
      for (let i = 0; i < value.length; i++) {
        let part = value[i]
        if (!part.name) continue
        let found = params.findIndex(p => p.name == part.name)
        if (found < 0) continue
        if (result == value) result = value.slice()
        let expr = args[found]
        if (expr instanceof NameExpression && !expr.namespace && !expr.args.length)
          result[i] = new PropPart(part.start, expr.id.name, null)
        else if (expr instanceof LiteralExpression)
          result[i] = new PropPart(part.start, expr.value, null)
        else
          this.raise(`Trying to interpolate expression '${expr}' into a prop`, part.start)
      }
      return result
    }
    let result = props as Prop[]
    for (let i = 0; i < props.length; i++) {
      let prop = props[i], value = substituteInValue(prop.value)
      if (value != prop.value) {
        if (result == props) result = props.slice()
        result[i] = new Prop(prop.start, prop.name, value)
      }
    }
    return result
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
    let skip = this.currentSkip[this.currentSkip.length - 1]
    for (let choice of choices)
      this.rules.push(new Rule(name, choice.terms, choice.ensureConflicts(), skip))
    return name
  }

  resolve(expr: NameExpression): Parts[] {
    if (expr.namespace) {
      let ns = this.namespaces[expr.namespace.name]
      if (!ns)
        this.raise(`Reference to undefined namespace '${expr.namespace.name}'`, expr.start)
      return ns.resolve(expr, this)
    } else {
      for (let built of this.built) if (built.matches(expr)) return [p(built.term)]

      let found = this.tokens.getToken(expr)
      if (found) return [p(found)]
      for (let ext of this.externalTokens) {
        let found = ext.getToken(expr)
        if (found) return [p(found)]
      }

      let known = this.astRules.find(r => r.rule.id.name == expr.id.name)
      if (!known)
        return this.raise(`Reference to undefined rule '${expr.id.name}'`, expr.start)
      if (known.rule.params.length != expr.args.length)
        this.raise(`Wrong number or arguments for '${expr.id.name}'`, expr.start)
      this.used(known.rule.id.name)
      return [p(this.buildRule(known.rule, expr.args, known.skip))]
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
    let inner = this.newName(name, true, {repeated: ""})

    let outer = inner
    if (expr.kind == "*") {
      outer = this.newName(name + "-wrap", true)
      this.defineRule(outer, [Parts.none, p(inner)])
    }
    this.built.push(new BuiltRule(expr.kind, [expr.expr], outer))

    let top = this.normalizeExpr(expr.expr)
    top.push(new Parts([inner, inner], [Conflicts.none, new Conflicts(PREC_REPEAT - 1, none), new Conflicts(PREC_REPEAT, none)]))
    this.defineRule(inner, top)

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
      return [p(this.tokens.getLiteral(expr)!)]
    } else if (expr instanceof NameExpression) {
      return this.resolve(expr)
    } else if (expr instanceof SpecializeExpression) {
      return [p(this.resolveSpecialization(expr))]
    } else if (expr instanceof InlineRuleExpression) {
      return [p(this.buildRule(expr.rule, none, this.currentSkip[this.currentSkip.length - 1]))]
    } else {
      return this.raise(`This type of expression ('${expr}') may not occur in non-token rules`, expr.start)
    }
  }

  buildRule(rule: RuleDeclaration, args: readonly Expression[], skip: Term): Term {
    let expr = this.substituteArgs(rule.expr, args, rule.params)
    let {name: nodeName, props} = this.nodeInfo(rule.props || none, rule.id.name, args, rule.params, rule.expr)
    if (rule.exported && rule.params.length) this.warn(`Can't export parameterized rules`, rule.start)
    let name = this.newName(rule.id.name + (args.length ? "<" + args.join(",") + ">" : ""), nodeName || true, props)
    if ((name.nodeType || rule.exported) && rule.params.length == 0) {
      if (!nodeName) name.preserve = true
      this.namedTerms[rule.id.name] = name
    }

    this.built.push(new BuiltRule(rule.id.name, args, name))
    this.currentSkip.push(skip)
    let result = this.defineRule(name, this.normalizeExpr(expr))
    this.currentSkip.pop()
    return result
  }

  nodeInfo(props: readonly Prop[], defaultName: string | null = null,
           args: readonly Expression[] = none, params: readonly Identifier[] = none,
           expr?: Expression): {name: string | null, props: Props} {
    let result: Props = noProps, name = defaultName && !ignored(defaultName) && !/ /.test(defaultName) ? defaultName : null
    for (let prop of props) {
      if (prop.name == "name") {
        name = this.finishProp(prop, args, params)
        if (/ /.test(name)) this.raise(`Node names cannot have spaces ('${name}')`, prop.start)
        continue
      }
      if (RESERVED_PROPS.includes(prop.name)) this.raise(`Prop name '${prop.name}' is reserved`, prop.start)
      if (!this.knownProps[prop.name]) this.raise(`Unknown prop name '${prop.name}'`, prop.start)
      if (result == noProps) result = Object.create(null)
      result[prop.name] = this.finishProp(prop, args, params)
    }
    if (expr && this.ast.autoDelim && (name || result != noProps)) {
      let delim = this.findDelimiters(expr)
      if (delim) {
        if (result == noProps) result = Object.create(null)
        result.delim = delim
      }
    }
    if (result != noProps && !name)
      this.raise(`Node has properties but no name`, props.length ? props[0].start : expr!.start)
    return {name, props: result}
  }

  finishProp(prop: Prop, args: readonly Expression[], params: readonly Identifier[]): string {
    return prop.value.map(part => {
      if (part.value) return part.value
      let pos = params.findIndex(param => param.name == part.name)
      if (pos < 0) this.raise(`Property refers to '${part.name}', but no parameter by that name is in scope`, part.start)
      let expr = args[pos]
      if (expr instanceof NameExpression && !expr.args.length && !expr.namespace) return expr.id.name
      if (expr instanceof LiteralExpression) return expr.value
      return this.raise(`Expression '${expr}' can not be used as part of a property value`, part.start)
    }).join("")
  }

  resolveSpecialization(expr: SpecializeExpression) {
    let type = expr.type
    let {name, props} = this.nodeInfo(expr.props)
    let terminal = this.normalizeExpr(expr.token)
    if (terminal.length != 1 || terminal[0].terms.length != 1 || !terminal[0].terms[0].terminal)
      this.raise(`The first argument to '${type}' must resolve to a token`, expr.token.start)
    let values
    if (expr.content instanceof LiteralExpression)
      values = [expr.content.value]
    else if ((expr.content instanceof ChoiceExpression) && expr.content.exprs.every(e => e instanceof LiteralExpression))
      values = expr.content.exprs.map(expr => (expr as LiteralExpression).value)
    else
      return this.raise(`The second argument to '${expr.type}' must be a literal or choice of literals`, expr.content.start)

    let term = terminal[0].terms[0], token = null
    let table = this.specialized[term.name] || (this.specialized[term.name] = [])
    for (let value of values) {
      let known = table.find(sp => sp.value == value)
      if (known == null) {
        if (!token) token = this.makeTerminal(term.name + "/" + JSON.stringify(value), name, props)
        table.push({value, term: token, type})
        this.tokenOrigins[token.name] = term
      } else {
        if (known.type != type)
          this.raise(`Conflicting specialization types for ${JSON.stringify(value)} of ${term.name} (${type} vs ${known.type})`, expr.start)
        if (token && known.term != token)
          this.raise(`Conflicting specialization tokens for ${JSON.stringify(value)} of ${term.name}`, expr.start)
        token = known.term
      }
    }
    return token!
  }

  findDelimiters(expr: Expression) {
    if (!(expr instanceof SequenceExpression) || expr.exprs.length < 2) return null
    let findToken = (expr: Expression): string | null => {
      if (expr instanceof LiteralExpression) return expr.value
      if (expr instanceof NameExpression && expr.args.length == 0) {
        let rule = this.ast.rules.find(r => r.id.name == expr.id.name)
        if (rule) return findToken(rule.expr)
        let token = this.tokens.rules.find(r => r.id.name == expr.id.name)
        if (token && token.expr instanceof LiteralExpression) return token.expr.value
      }
      return null
    }
    let lastToken = findToken(expr.exprs[expr.exprs.length - 1])
    if (!lastToken) return null
    const brackets = ["()", "[]", "{}", "<>"]
    let bracket = brackets.find(b => lastToken!.indexOf(b[1]) > -1 && lastToken!.indexOf(b[0]) < 0)
    if (!bracket) return null
    let firstToken = findToken(expr.exprs[0])
    if (!firstToken || firstToken.indexOf(bracket[0]) < 0 || firstToken.indexOf(bracket[1]) > -1) return null
    return firstToken + " " + lastToken
  }
}

const RESERVED_PROPS = ["error", "repeated"]

function reduceAction(rule: Rule, partOfSkip: Term | null, depth = rule.parts.length) {
  return rule.name.id | Action.ReduceFlag |
    (rule.isRepeatLeaf && depth == rule.parts.length ? Action.RepeatFlag : 0) |
    (rule.name == partOfSkip ? Action.StayFlag : 0) |
    (depth << Action.ReduceDepthShift)
}

function isSimpleSkip(action: Shift | Reduce, skipRule: Term) {
  return action instanceof Shift && !!action.target.defaultReduce && action.target.defaultReduce.name == skipRule
}

function findArray(data: number[], value: number[]) {
  search: for (let i = 0;;) {
    let next = data.indexOf(value[0], i)
    if (next == -1 || next + value.length > data.length) break
    for (let j = 1; j < value.length; j++) {
      if (value[j] != data[next + j]) {
        i = next + 1
        continue search
      }
    }
    return next
  }
  return -1
}

class DataBuilder {
  data: number[] = []

  storeArray(data: number[]) {
    let found = findArray(this.data, data)
    if (found > -1) return found
    let pos = this.data.length
    for (let num of data) this.data.push(num)
    return pos
  }

  finish() {
    return Uint16Array.from(this.data)
  }
}

// The goto table maps a start state + a term to a new state, and is
// used to determine the new state when reducing. Because this allows
// more more efficient representation and access, unlike the action
// tables, the goto table is organized by term, with groups of start
// states that map to a given end state enumerated for each term.
// Since many terms only have a single valid goto target, this makes
// it cheaper to look those up.
//
// (Unfortunately, though the standard LR parsing mechanism never
// looks up invalid goto states, the incremental parsing mechanism
// needs accurate goto information for a state/term pair, so we do
// need to store state ids even for terms that have only one target.)
//
// - First comes the amount of terms in the table
//
// - Then, for each term, the offset of the term's data
//
// - At these offsets, there's a record for each target state
//
//   - Such a record starts with the amount of start states that go to
//     this target state, shifted one to the left, with the first bit
//     only set if this is the last record for this term.
//
//   - Then follows the target state id
//
//   - And then the start state ids
function computeGotoTable(states: readonly LRState[]) {
  let goto: {[term: number]: {[to: number]: number[]}} = {}
  let maxTerm = 0
  for (let state of states)
    for (let entry of state.goto) {
      maxTerm = Math.max(entry.term.id, maxTerm)
      let set = goto[entry.term.id] || (goto[entry.term.id] = {})
      ;(set[entry.target.id] || (set[entry.target.id] = [])).push(state.id)
    }
  let data = new DataBuilder
  let index: number[] = []
  let offset = maxTerm + 2 // Offset of the data, taking index size into account

  for (let term = 0; term <= maxTerm; term++) {
    let entries = goto[term]
    if (!entries) {
      index.push(1)
      continue
    }
    let termTable: number[] = []
    let keys = Object.keys(entries)
    for (let target of keys) {
      let list = entries[target as any]
      termTable.push((target == keys[keys.length - 1] ? 1 : 0) + (list.length << 1))
      termTable.push(+target)
      for (let source of list) termTable.push(source)
    }
    index.push(data.storeArray(termTable) + offset)
  }
  if (index.some(n => n > 0xffff)) throw new Error("Goto table too large")

  return Uint16Array.from([maxTerm + 1, ...index, ...data.data])
}

class TokenGroup {
  constructor(readonly tokens: Term[], readonly id: number) {}
}

function addToSet<T>(set: T[], value: T) {
  if (!set.includes(value)) set.push(value)
}

function buildTokenMasks(groups: TokenGroup[]) {
  let masks: {[id: number]: number} = Object.create(null)
  for (let group of groups) {
    let groupMask = 1 << group.id
    for (let term of group.tokens) {
      masks[term.id] = (masks[term.id] || 0) | groupMask
    }
  }
  return masks
}

interface Namespace {
  resolve(expr: NameExpression, builder: Builder): Parts[]
}

class NestedGrammarSpec {
  constructor(readonly placeholder: Term,
              readonly name: string,
              readonly extName: string,
              readonly source: string | null,
              readonly end: State) {}
}

class NestNamespace implements Namespace {
  resolve(expr: NameExpression, builder: Builder): Parts[] {
    if (expr.args.length > 2)
      builder.raise(`Too many arguments to 'nest.${expr.id.name}'`, expr.start)
    let [endExpr, defaultExpr] = expr.args as [Expression | undefined, Expression | undefined]
    let extGrammar = builder.ast.grammars.find(g => g.id.name == expr.id.name)
    if (!extGrammar) return builder.raise(`No external grammar '${expr.id.name}' defined`, expr.id.start)
    let placeholder = builder.newName(expr.id.name + "-placeholder", true)
    builder.defineRule(placeholder, defaultExpr ? builder.normalizeExpr(defaultExpr) : [])

    if (!endExpr && !(endExpr = findExprAfter(builder.ast, expr)))
      return builder.raise(`No end token specified, and no token found directly after the nest expression`, expr.start)
    let endStart = new State, endEnd = new State([builder.terms.eof])
    try {
      builder.tokens.build(endExpr, endStart, endEnd, none)
    } catch(e) {
      if (!(e instanceof SyntaxError)) throw e
      builder.raise(`End token '${endExpr}' for nested grammar is not a valid token expression`, endExpr.start)
    }
    builder.nestedGrammars.push(new NestedGrammarSpec(placeholder, extGrammar.id.name, extGrammar.externalID.name,
                                                      extGrammar.source, endStart))
    if (builder.nestedGrammars.length >= 2**(30 - StateFlag.NestShift))
      builder.raise("Too many nested grammars used")
    return [p(placeholder)]
  }
}

function findExprAfter(ast: GrammarDeclaration, expr: Expression) {
  let found: Expression | undefined
  function walk(cur: Expression) {
    if (cur instanceof SequenceExpression) {
      let index = cur.exprs.indexOf(expr)
      if (index > -1 && index < cur.exprs.length - 1) found = cur.exprs[index + 1]
    }
    return cur
  }
  for (let rule of ast.rules) rule.expr.walk(walk)
  ast.topRule.expr.walk(walk)
  return found
}

class TokenArg {
  constructor(readonly name: string, readonly expr: Expression, readonly scope: readonly TokenArg[]) {}
}

class BuildingRule {
  constructor(readonly name: string, readonly start: State, readonly to: State, readonly args: readonly Expression[]) {}
}

class TokenSet {
  startState: State = new State
  built: BuiltRule[] = []
  building: BuildingRule[] = [] // Used for recursion check
  rules: readonly RuleDeclaration[]
  precedences: Term[] = []
  precedenceRelations: readonly {term: Term, after: readonly Term[]}[] = []

  constructor(readonly b: Builder, readonly ast: TokenDeclaration | null) {
    this.rules = ast ? ast.rules : none
    for (let rule of this.rules) this.b.unique(rule.id)
  }

  getToken(expr: NameExpression) {
    for (let built of this.built) if (built.matches(expr)) return built.term
    let name = expr.id.name
    let rule = this.rules.find(r => r.id.name == name)
    if (!rule) return null
    let {name: nodeName, props} =
      this.b.nodeInfo(rule.props, name, expr.args, rule.params.length != expr.args.length ? none : rule.params)
    let term = this.b.makeTerminal(expr.toString(), nodeName, props)
                                                                     
    if ((term.nodeType || rule.exported) && rule.params.length == 0) {
      if (!term.nodeType) term.preserve = true
      this.b.namedTerms[name] = term
    }
    this.buildRule(rule, expr, this.startState, new State([term]))
    this.built.push(new BuiltRule(name, expr.args, term))
    return term
  }

  getLiteral(expr: LiteralExpression) {
    let id = JSON.stringify(expr.value)
    for (let built of this.built) if (built.id == id) return built.term
    let name = null, props = noProps
    let decl = this.ast ? this.ast.literals.find(l => l.literal == expr.value) : null
    if (decl) ({name, props} = this.b.nodeInfo(decl.props, expr.value))
    let term = this.b.makeTerminal(id, name, props)
    this.build(expr, this.startState, new State([term]), none)
    this.built.push(new BuiltRule(id, none, term))
    return term
  }

  buildRule(rule: RuleDeclaration, expr: NameExpression, from: State, to: State, args: readonly TokenArg[] = none) {
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
    this.b.used(rule.id.name)
    let start = new State
    from.nullEdge(start)
    this.building.push(new BuildingRule(name, start, to, expr.args))
    this.build(this.b.substituteArgs(rule.expr, expr.args, rule.params), start, to,
               expr.args.map((e, i) => new TokenArg(rule!.params[i].name, e, args)))
    this.building.pop()
  }

  build(expr: Expression, from: State, to: State, args: readonly TokenArg[]): void {
    if (expr instanceof NameExpression) {
      if (expr.namespace) {
        if (expr.namespace.name == "std") return this.buildStd(expr, from, to)
        this.b.raise(`Unknown namespace '${expr.namespace.name}'`, expr.start)
      }
      let name = expr.id.name, arg = args.find(a => a.name == name)
      if (arg) return this.build(arg.expr, from, to, arg.scope)
      let rule = this.rules.find(r => r.id.name == name)
      if (!rule) return this.b.raise(`Reference to rule '${expr.id.name}', which isn't found in this token group`, expr.start)
      this.buildRule(rule, expr, from, to, args)
    } else if (expr instanceof ChoiceExpression) {
      for (let choice of expr.exprs) this.build(choice, from, to, args)
    } else if (isEmpty(expr)) {
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

  buildStd(expr: NameExpression, from: State, to: State) {
    if (expr.args.length) this.b.raise(`'std.${expr.id.name}' does not take arguments`, expr.args[0].start)
    if (!STD_RANGES.hasOwnProperty(expr.id.name)) this.b.raise(`There is no builtin rule 'std.${expr.id.name}'`, expr.start)
    for (let [a, b] of STD_RANGES[expr.id.name]) from.edge(a, b, to)
  }

  tokenizer(tokenMasks: {[id: number]: number}, precedence: readonly number[]) {
    let startState = this.startState.compile()
    if (startState.accepting.length)
      this.b.raise(`Grammar contains zero-length tokens (in '${startState.accepting[0].name}')`,
                   this.rules.find(r => r.id.name == startState.accepting[0].name)!.start)
    if (/\btokens\b/.test(verbose)) console.log(startState.toString())
    return startState.toArray(tokenMasks, precedence)
  }

  takePrecedences() {
    let rel: {term: Term, after: Term[]}[] = []
    if (this.ast) for (let group of this.ast.precedences) {
      let terms: Term[] = []
      for (let item of group.items) {
        let known
        if (item instanceof NameExpression) {
          known = this.built.find(b => b.matches(item as NameExpression))
        } else {
          let id = JSON.stringify(item.value)
          known = this.built.find(b => b.id == id)
        }
        if (!known)
          this.b.warn(`Precedence specified for unknown token ${item}`, item.start)
        else
          terms.push(known.term)
      }
      for (let i = 0; i < terms.length; i++) {
        let found = rel.find(r => r.term == terms[i])
        if (!found) rel.push(found = {term: terms[i], after: terms.slice(0, i)})
        else for (let j = 0; j < i; j++) addToSet(found.after, terms[j])
      }
    }
    this.precedenceRelations = rel.slice()

    let ordered: Term[] = []
    add: for (;;) {
      for (let i = 0; i < rel.length; i++) {
        let record = rel[i]
        if (record.after.every(t => ordered.includes(t))) {
          ordered.push(record.term)
          let last = rel.pop()!
          if (i < rel.length) rel[i] = last
          continue add
        }
      }
      if (rel.length)
        this.b.raise(`Cyclic token precedence relation between ${rel.map(r => r.term).join(", ")}`)
      break
    }
    this.precedences = ordered
  }

  precededBy(a: Term, b: Term) {
    let found = this.precedenceRelations.find(r => r.term == a)
    return found && found.after.includes(b)
  }

  buildTokenGroups(states: readonly LRState[], skipStates: (LRState[] | null)[]) {
    let tokens = this.startState.compile()
    let usedPrec: Term[] = []
    let conflicts = tokens.findConflicts().filter(({a, b}) => {
      // If both tokens have a precedence, the conflict is resolved
      addToSet(usedPrec, a)
      addToSet(usedPrec, b)
      return !this.precededBy(a, b) && !this.precededBy(b, a)
    })

    let groups: TokenGroup[] = []
    let checkState = (state: LRState) => {
      // Find potentially-conflicting terms (in terms) and the things
      // they conflict with (in conflicts), and raise an error if
      // there's a token conflict directly in this state.
      let terms: Term[] = [], hasTerms = false, incompatible: Term[] = []
      let skip = null
      if (state.skip != this.b.noSkip) {
        let states = skipStates[this.b.skipRules.indexOf(state.skip)]
        if (states) skip = states[0]
      }
      if (skip) for (let action of skip.actions) {
        if (state.actions.some(a => a.term == action.term))
          this.b.raise(`Use of token ${action.term.name} conflicts with skip rule`)
      }

      for (let i = 0; i < state.actions.length + (skip ? skip.actions.length : 0); i++) {
        let term = (i < state.actions.length ? state.actions[i] : skip!.actions[i - state.actions.length]).term
        let orig = this.b.tokenOrigins[term.name]
        if (orig instanceof Term) {
          if (terms.includes(orig)) continue
          term = orig
        } else if (orig) {
          continue
        }
        hasTerms = true
        let hasConflict = false
        for (let conflict of conflicts) {
          let conflicting = conflict.a == term ? conflict.b : conflict.b == term ? conflict.a : null
          if (!conflicting) continue
          hasConflict = true
          if (!incompatible.includes(conflicting)) {
            if (state.actions.some(a => a.term == conflicting))
              this.b.raise(`Overlapping tokens ${term.name} and ${conflicting.name} used in same context`)
            incompatible.push(conflicting)
          }
        }
        if (hasConflict) terms.push(term)
      }
      if (!hasTerms) return

      let tokenGroup = null
      for (let group of groups) {
        if (incompatible.some(term => group.tokens.includes(term))) continue
        for (let term of terms) addToSet(group.tokens, term)
        tokenGroup = group
        break
      }
      if (!tokenGroup) {
        tokenGroup = new TokenGroup(terms, groups.length)
        groups.push(tokenGroup)
      }
      state.tokenGroup = tokenGroup.id
    }
    for (let state of states) checkState(state)
    for (let states of skipStates) if (states) for (let state of states) checkState(state)

    if (groups.length > 16)
      this.b.raise(`Too many different token groups (${groups.length}) to represent them as a 16-bit bitfield`)

    let tokenPrec = this.precedences.filter(term => usedPrec.includes(term)).map(t => t.id)
    return {tokenMasks: buildTokenMasks(groups), tokenGroups: groups, tokenPrec}
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

function isEmpty(expr: Expression) {
  return expr instanceof SequenceExpression && expr.exprs.length == 0
}

class ExternalTokenSet {
  tokens: {[name: string]: Term} = Object.create(null)

  constructor(readonly b: Builder, readonly ast: ExternalTokenDeclaration) {
    for (let token of ast.tokens) {
      b.unique(token.id)
      let {name, props} = b.nodeInfo(token.props, token.id.name)
      let term = b.makeTerminal(token.id.name, name, props)
      b.namedTerms[token.id.name] = this.tokens[token.id.name] = term
      this.b.tokenOrigins[term.name] = this
    }
  }

  getToken(expr: NameExpression) {
    let found = this.tokens[expr.id.name]
    if (!found) return null
    if (expr.args.length) this.b.raise("External tokens cannot take arguments", expr.args[0].start)
    this.b.used(expr.id.name)
    return found
  }
}

class TempExternalTokenizer {
  _inner: null | ExternalTokenizer = null

  constructor(readonly set: ExternalTokenSet, readonly terms: {[name: string]: number}) {}

  get inner(): ExternalTokenizer {
    if (!this._inner) {
      let getExt = this.set.b.options.externalTokenizer
      this._inner = getExt ? getExt(this.set.ast.id.name, this.terms) : null
      if (!this._inner) return this.set.b.raise(`Using external tokenizer without passing externalTokenizer option`)
    }
    return this._inner
  }
  
  token(stream: InputStream, token: Token, stack: any) {
    this.inner.token(stream, token, stack)
  }

  get contextual() { return this.inner.contextual }
}

function tempNestedGrammar(b: Builder, grammar: NestedGrammarSpec): NestedGrammar {
  let resolved: NestedGrammar | null = null
  let result = function(input: InputStream, stack: Stack) {
    if (!resolved && grammar.source)
      resolved = b.options.nestedGrammar ? b.options.nestedGrammar(grammar.name, b.termTable) : null
    return resolved instanceof Parser ? {parser: resolved} : resolved ? resolved(input, stack) : {}
  }
  ;(result as any).spec = grammar
  return result
}

function inlineRules(rules: readonly Rule[], preserve: readonly Term[]): readonly Rule[] {
  for (;;) {
    let inlinable: {[name: string]: Rule} = Object.create(null), found
    for (let i = 0; i < rules.length; i++) {
      let rule = rules[i]
      if (!rule.name.interesting && !rule.parts.includes(rule.name) && rule.parts.length < 3 &&
          !preserve.includes(rule.name) &&
          (rule.parts.length == 1 || rules.every(other => other.skip == rule.skip || !other.parts.includes(rule.name))) &&
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
      newRules.push(new Rule(rule.name, parts, conflicts, rule.skip))
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
                  new Rule(rule.name, rule.parts.map(p => merged[p.name] || p), rule.conflicts, rule.skip))
  }
  return newRules
}

function simplifyRules(rules: readonly Rule[], preserve: readonly Term[]): readonly Rule[] {
  return mergeRules(inlineRules(rules, preserve))
}

/// Build an in-memory parser instance for a given grammar. This is
/// mostly useful for testing. If your grammar uses external
/// tokenizers or nested grammars, you'll have to provide the
/// `externalTokenizer` and/or `nestedGrammar` options for the
/// returned parser to be able to parse anything.
export function buildParser(text: string, options: BuildOptions = {}): Parser {
  return new Builder(text, options).getParser()
}

const KEYWORDS = ["break", "case", "catch", "continue", "debugger", "default", "do", "else", "finally",
                  "for", "function", "if", "return", "switch", "throw", "try", "var", "while", "with",
                  "null", "true", "false", "instanceof", "typeof", "void", "delete", "new", "in", "this",
                  "const", "class", "extends", "export", "import", "super", "enum", "implements", "interface",
                  "let", "package", "private", "protected", "public", "static", "yield"]

/// Build the code that represents the parser tables for a given
/// grammar description. The `parser` property in the return value
/// holds the main file that exports the `Parser` instance. The
/// `terms` property holds a declaration file that defines constants
/// for all of the named terms in grammar, holding their ids as value.
/// This is useful when external code, such as a tokenizer, needs to
/// be able to use these ids. It is recommended to run a tree-shaking
/// bundler when importing this file, since you usually only need a
/// handful of the many terms in your code.
export function buildParserFile(text: string, options: BuildOptions = {}): {parser: string, terms: string} {
  let builder = new Builder(text, options), parser = builder.getParser()
  let mod = options.moduleStyle || "cjs"

  let gen = "// This file was generated by lezer-generator. You probably shouldn't edit it.\n", head = gen
  head += mod == "cjs" ? `const {Parser} = require("lezer")\n`
    : `import {Parser} from "lezer"\n`
  let tokenData = null, imports: {[source: string]: string[]} = {}, imported: {[spec: string]: string} = Object.create(null)
  let defined = Object.create(null)
  defined.Parser = true
  let getName = (prefix: string) => {
    for (let i = 0;; i++) {
      let id = prefix + (i ? "_" + i : "")
      if (!defined[id]) return id
    }
  }
  let importName = (name: string, source: string, prefix: string) => {
    let spec = name + " from " + source
    if (imported[spec]) return imported[spec]
    let src = JSON.stringify(source), varName = name
    if (name in defined) {
      varName = getName(prefix)
      name += `${mod == "cjs" ? ":" : " as"} ${varName}`
    }
    ;(imports[src] || (imports[src] = [])).push(name)
    return imported[spec] = varName
  }

  let tokenizers = parser.tokenizers.map(tok => {
    if (tok instanceof TempExternalTokenizer) {
      let {source, id: {name}} = tok.set.ast
      return importName(name, source, "tok")
    } else {
      tokenData = (tok as LezerTokenGroup).data
      return (tok as LezerTokenGroup).id
    }
  })

  let nested = parser.nested.map(({name, grammar, end, placeholder}) => {
    let spec: NestedGrammarSpec = (grammar as any).spec
    if (!spec) throw new Error("Spec-less nested grammar in parser")
    return `[${JSON.stringify(name)}, ${spec.source ? importName(spec.extName, spec.source, spec.name) : "null"},\
${encodeArray((end as LezerTokenGroup).data)}, ${placeholder}]`
  })

  let nodeNames = [], nodeProps: {prop: string, terms: (number | string)[]}[] = []
  let repeatCount = 0
  for (let type of parser.group.types) {
    if (type.prop(NodeProp.repeated)) repeatCount++
    else nodeNames.push(type.name)
    let propData = (type as any).props.propData
    for (let i = 0; i < propData.length; i += 2) {
      let source = propData[i], value = propData[i + 1]
      if (source.from == null && (source.name == "repeated" || source.name == "error")) continue
      let propID = source.from ? importName(source.name, source.from, "prop") :
        importName("NodeProp", "lezer", "NodeProp") + "." + source.name
      let known = nodeProps.find(p => p.prop == propID)
      if (!known) nodeProps.push(known = {prop: propID, terms: []})
      known.terms.push(type.id, value)
    }
  }

  for (let source in imports) {
    if (mod == "cjs")
      head += `const {${imports[source].join(", ")}} = require(${source})\n`
    else
      head += `import {${imports[source].join(", ")}} from ${source}\n`
  }

  function specializationString(table: {[name: string]: number}) {
    return "{" + Object.keys(table).map(key => `${/\W/.test(key) ? JSON.stringify(key) : key}:${table[key]}`).join(", ") + "}"
  }

  let parserStr = `Parser.deserialize({
  states: ${encodeArray(parser.states, 0xffffffff)},
  stateData: ${encodeArray(parser.data)},
  goto: ${encodeArray(parser.goto)},
  nodeNames: ${JSON.stringify(nodeNames.join(" "))},${nodeProps.length ? `
  nodeProps: [
    ${nodeProps.map(p => `[${p.prop}, ${p.terms.map(val => JSON.stringify(val)).join(",")}]`).join(",\n    ")}
  ],` : ""}
  repeatNodeCount: ${repeatCount},
  tokenData: ${encodeArray(tokenData || [])},
  tokenizers: [${tokenizers.join(", ")}],${nested.length ? `
  nested: [${nested.join(", ")}],` : ""}
  specializeTable: ${parser.specializeTable},${parser.specializations.length ? `
  specializations: [${parser.specializations.map(specializationString).join(",\n   ")}],` : ""}
  tokenPrec: ${parser.tokenPrecTable}${options.includeNames ? `,
  termNames: ${JSON.stringify(parser.termNames)}` : ''}
})` // FIXME more compact format for term names (omit named nodes, drop quotes)

  let terms: string[] = []
  for (let name in builder.termTable) {
    let id = name
    if (KEYWORDS.includes(id)) for (let i = 1;; i++) {
      id = "_".repeat(i) + name
      if (!(id in builder.termTable)) break
    }
    terms.push(`${id}${mod == "cjs" ? ":" : " ="} ${builder.termTable[name]}`)
  }

  let exportName = options.exportName || "parser"
  return {
    parser: head + (mod == "cjs" ? `exports.${exportName} = ${parserStr}\n` : `export const ${exportName} = ${parserStr}\n`),
    terms: mod == "cjs" ? `${gen}module.exports = {\n  ${terms.join(",\n  ")}\n}`
      : `${gen}export const\n  ${terms.join(",\n  ")}\n`
  }
}

function ignored(name: string) {
  let first = name[0]
  return first == "_" || first.toUpperCase() != first
}
