import {GrammarDeclaration, RuleDeclaration, TokenDeclaration, ExternalTokenDeclaration,
        Expression, Identifier, LiteralExpression, NamedExpression, SequenceExpression,
        ChoiceExpression, RepeatExpression, SetExpression, AnyExpression, ConflictMarker,
        exprsEq, exprEq} from "./node"
import {Term, TermSet, PREC_REPEAT, Rule, Conflicts} from "./grammar"
import {State, MAX_CHAR} from "./token"
import {Input} from "./parse"
import {computeFirstSets, buildFullAutomaton, finishAutomaton, State as LRState, Shift, Reduce} from "./automaton"
import {encodeArray} from "./encode"
import {Parser, TagMap, ParseState, TokenGroup as LezerTokenGroup, ExternalTokenizer,
        REDUCE_DEPTH_SHIFT, REDUCE_FLAG, ACTION_VALUE_MASK, REDUCE_REPEAT_FLAG, SPECIALIZE, EXTEND,
        TERM_ERR, TERM_OTHER, STAY_FLAG, GOTO_FLAG, SKIPPED_FLAG, ACCEPTING_FLAG} from "lezer"

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

const reserved = ["specialize", "extend", "skip"]

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
  externalTokenizer?: (name: string, terms: {[name: string]: number}) => ExternalTokenizer
}

class Builder {
  ast: GrammarDeclaration
  input: Input
  terms = new TermSet
  tokens: TokenSet
  externalTokens: ExternalTokenSet[]
  nestedGrammars: NestedGrammar[] = []
  specialized: {[name: string]: {value: string, term: Term, type: string}[]} = Object.create(null)
  tokenOrigins: {[name: string]: Term | ExternalTokenSet} = Object.create(null)
  rules: Rule[] = []
  built: BuiltRule[] = []
  ruleNames: {[name: string]: Identifier | null} = Object.create(null)
  namespaces: {[name: string]: Namespace} = Object.create(null)
  namedTerms: {[name: string]: Term} = Object.create(null)
  termTable: {[name: string]: number} = Object.create(null)

  astRules: {skip: Term, rule: RuleDeclaration}[] = []
  currentSkip: Term[] = []
  noSkip: Term
  skipRules: Term[] = []

  constructor(text: string, readonly options: BuildOptions) {
    this.input = new Input(text, options.fileName)
    this.ast = this.input.parse()

    this.tokens = new TokenSet(this, this.ast.tokens)
    this.externalTokens = this.ast.externalTokens.map(ext => new ExternalTokenSet(this, ext))

    this.defineNamespace("tag", new TagNamespace)
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
      let skip = scoped.expr instanceof SequenceExpression && !scoped.expr.exprs.length ? this.noSkip : this.newName("%skip", true)
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
    // FIXME deduplicate?
    for (let i = 0; i < this.ast.scopedSkip.length; i++) if (scopedSkip[i] != this.noSkip) {
      this.skipRules.push(scopedSkip[i])
      this.defineRule(scopedSkip[i], this.normalizeExpr(this.ast.scopedSkip[i].expr))
    }
    this.currentSkip.pop()

    let program = this.astRules.find(r => r.rule.id.name == "program")
    if (!program) return this.raise(`Missing 'program' rule declaration`)
    if (program.rule.params.length) this.raise(`'program' rules should not take parameters`, program.rule.id.start)
    this.buildRule(program.rule, [], program.skip)

    for (let name in this.ruleNames) {
      let value = this.ruleNames[name]
      if (value) this.warn(`Unused rule '${value.name}'`, value.start)
    }

    this.tokens.takePrecedences()
  }

  unique(id: Identifier) {
    if (id.name in this.ruleNames)
      this.raise(`Duplicate definition of rule '${id.name}'`, id.start)
    if (reserved.includes(id.name)) this.raise(`The name '${id.name}' is reserved for a built-in operator`, id.start)
    this.ruleNames[id.name] = id
  }

  used(name: string) {
    this.ruleNames[name] = null
  }

  defineNamespace(name: string, value: Namespace, pos: number = 0) {
    if (this.namespaces[name]) this.raise(`Duplicate definition of namespace '${name}'`, pos)
    this.namespaces[name] = value
  }

  newName(base: string, tag: string | null | true = null): Term {
    for (let i = tag ? 0 : 1;; i++) {
      let name = i ? `${base}-${i}` : base
      if (!this.terms.nonTerminals.some(t => t.name == name))
        return this.terms.makeNonTerminal(name, tag === true ? null : tag)
    }
  }

  getParser() {
    let rules = simplifyRules(this.rules, this.skipRules.concat(this.terms.nonTerminals.find(t => t.program)!))
    let {tags, names} = this.terms.finish(rules)
    for (let prop in this.namedTerms) this.termTable[prop] = this.namedTerms[prop].id

    if (/\bgrammar\b/.test(verbose)) console.log(rules.join("\n"))

    let first = computeFirstSets(this.terms)
    let fullSkipAutomata = this.skipRules.map(name => name.rules.some(r => r.parts.length > 0) ? buildFullAutomaton(this.terms, name, first) : null)
    let fullTable = buildFullAutomaton(this.terms, this.terms.nonTerminals.find(t => t.program)!, first)
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
      specialized.push(this.terms.terminals.find(t => t.name == name)!.id)
      let table: {[value: string]: number} = {}
      for (let {value, term, type} of this.specialized[name]) {
        let code = type == "specialize" ? SPECIALIZE : EXTEND
        table[value] = (term.id << 1) | code
      }
      specializations.push(table)
    }
    specialized.push(TERM_ERR)

    let tokenData = this.tokens.tokenizer(tokenMasks, tokenPrec)
    let tokStart = (tokenizer: TempExternalTokenizer | LezerTokenGroup) => {
      if (tokenizer instanceof TempExternalTokenizer) return tokenizer.set.ast.start
      return this.tokens.ast ? this.tokens.ast.start : -1
    }
    let tokenizers = 
      (tokenGroups.map(g => new LezerTokenGroup(tokenData, g.id)) as (LezerTokenGroup | TempExternalTokenizer)[])
      .concat(this.externalTokens.map(ext => new TempExternalTokenizer(ext, this.termTable)))
      .sort((a, b) => tokStart(a) - tokStart(b))

    let data = new StateDataBuilder
    let skipData = skipStartStates.map((state, i) => {
      let actions: number[] = []
      if (state) {
        for (let action of state.actions as Shift[]) {
          if (isSimpleSkip(action, this.skipRules[i]))
            actions.push(action.term.id, 0, STAY_FLAG >> 16)
          else
            actions.push(action.term.id, state.id, GOTO_FLAG >> 16)
        }
        // No need to store simple skip actions in the skip start
        // state—they'll never be accessed, since the STAY_FLAG action
        // avoids state changes entirely.
        state.actions = state.actions.filter(a => !isSimpleSkip(a, this.skipRules[i]))
      }
      actions.push(TERM_ERR)
      return data.storeArray(actions)
    })
    let noSkip = data.storeArray([TERM_ERR])
    let states = table.map(s => {
      let skip = noSkip, skipState = null
      if (s.skip != this.noSkip) {
        let index = this.skipRules.indexOf(s.skip)
        skip = skipData[index]
        skipState = skipStartStates[index]
      }
      return this.finishState(s, tokenizers, data, skip, skipState, s.id >= firstSkipState)
    })

    let skipTags = this.gatherSkippedTerms().filter(t => t.tag).map(t => t.id)
    skipTags.push(TERM_ERR)

    let precTable = data.storeArray(tokenPrec.concat(TERM_ERR))
    let specTable = data.storeArray(specialized)
    let skipTable = data.storeArray(skipTags)
    let id = Parser.allocateID()
    return new Parser(id, states, data.finish(), computeGotoTable(table), TagMap.single(id, tags), tokenizers,
                      specTable, specializations, precTable, skipTable, names)
  }

  addNestedGrammars(table: LRState[]) {
    for (let state of table) {
      let ext = -1
      for (let i = 0; i < state.set.length; i++) {
        let next = state.set[i].next, match = this.nestedGrammars.findIndex(n => n.placeholder == next)
        if (i == 0) ext = match
        else if (ext != match) this.raise(`Nested grammar in ambiguous position after ${state.set[i].trail()}`)
      }
      state.nested = ext
    }
  }

  makeTerminal(name: string, tag: string | null) {
    for (let i = 0;; i++) {
      let cur = i ? `${name}-${i}` : name
      if (this.terms.terminals.some(t => t.name == cur)) continue
      return this.terms.makeTerminal(cur, tag)
    }
  }

  gatherSkippedTerms() {
    // FIXME check that tagged things from skip rules aren't also used outside of them
    let terms: Term[] = this.skipRules.slice()
    for (let i = 0; i < terms.length; i++) {
      for (let rule of terms[i].rules) {
        for (let part of rule.parts) if (!terms.includes(part)) terms.push(part)
      }
    }
    return terms
  }

  finishState(state: LRState, tokenizers: (LezerTokenGroup | TempExternalTokenizer)[],
              data: StateDataBuilder, skipTable: number, skipState: LRState | null, isSkip: boolean) {
    let actions = [], recover = [], forcedReduce = 0
    let defaultReduce = state.defaultReduce ? reduceAction(state.defaultReduce, state.partOfSkip) : 0
    let flags = isSkip ? SKIPPED_FLAG : 0

    let other = -1
    if (defaultReduce == 0) for (let action of state.actions) {
      // FIXME move this to buildTokenGroups?
      if (skipState && skipState.actions.some(a => a.term == action.term))
        this.raise(`Use of token ${action.term.name} conflicts with skip rule`)
      if (action instanceof Shift) {
        actions.push(action.term.id, action.target.id, 0)
      } else {
        let code = reduceAction(action.rule, state.partOfSkip)
        if (state.partOfSkip && action.term.eof) other = code
        else actions.push(action.term.id, code & ACTION_VALUE_MASK, code >> 16)
      }
    }
    if (other > -1) actions.push(TERM_OTHER, other & ACTION_VALUE_MASK, other >> 16)
    actions.push(TERM_ERR)

    for (let action of state.recover)
      recover.push(action.term.id, action.target.id)
    recover.push(TERM_ERR)

    let positions = state.set.filter(p => p.pos > 0)
    if (positions.length) {
      let defaultPos = positions.reduce((a, b) => a.pos - b.pos || b.rule.parts.length - a.rule.parts.length < 0 ? b : a)
      if (!defaultPos.rule.name.program)
        forcedReduce = reduceAction(defaultPos.rule, state.partOfSkip, defaultPos.pos)
      else if (positions.some(p => p.rule.name.program && p.pos == p.rule.parts.length))
        flags |= ACCEPTING_FLAG
    }

    let external: ExternalTokenSet[] = []
    for (let {term} of state.actions) {
      let orig = this.tokenOrigins[term.name]
      if (orig instanceof ExternalTokenSet) addToSet(external, orig)
    }
    external.sort((a, b) => a.ast.start - b.ast.start)
    let tokenizerMask = 0
    for (let i = 0; i < tokenizers.length; i++) { // FIXME use a bitmask here
      let tok = tokenizers[i]
      if (tok instanceof TempExternalTokenizer ? external.includes(tok.set) : tok.id == state.tokenGroup)
        tokenizerMask |= (1 << i)
    }

    return new ParseState(state.id, flags,
                          data.storeArray(actions),
                          data.storeArray(recover),
                          skipTable,
                          tokenizerMask,
                          defaultReduce,
                          forcedReduce)
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
    let skip = this.currentSkip[this.currentSkip.length - 1]
    for (let choice of choices)
      this.rules.push(new Rule(name, choice.terms, choice.ensureConflicts(), skip))
    return name
  }

  resolve(expr: NamedExpression): Parts[] {
    if (expr.namespace) {
      let ns = this.namespaces[expr.namespace.name]
      if (!ns)
        this.raise(`Reference to undefined namespace '${expr.namespace.name}'`, expr.start)
      return ns.resolve(expr, this)
    } else if (expr.id.name == "specialize" || expr.id.name == "extend") {
      return [p(this.resolveSpecialization(expr, expr.id.name))]
    } else if (expr.id.name == "program") {
      return this.raise(`The 'program' rule can't be referenced in other expressions`, expr.start)
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
    let inner = this.newName(name, true)
    inner.repeated = true

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
    } else if (expr instanceof NamedExpression) {
      return this.resolve(expr)
    } else {
      return this.raise("This type of expression may not occur in non-token rules", expr.start)
    }
  }

  buildRule(rule: RuleDeclaration, args: readonly Expression[], skip: Term): Term {
    let expr = this.substituteArgs(rule.expr, args, rule.params)
    this.used(rule.id.name)
    let name = this.newName(rule.id.name + (args.length ? "<" + args.join(",") + ">" : ""),
                            rule.tag ? rule.tag.name : isTag(rule.id.name) || true)
    if (args.length == 0) this.namedTerms[rule.id.name] = name
    this.built.push(new BuiltRule(rule.id.name, args, name))
    this.currentSkip.push(skip)
    let result = this.defineRule(name, this.normalizeExpr(expr))
    this.currentSkip.pop()
    return result
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
      token = this.makeTerminal(term.name + "/" + JSON.stringify(value), tag)
      table.push({value, term: token, type})
      this.tokenOrigins[token.name] = term
    } else {
      if (known.type != type)
        this.raise(`Conflicting specialization types for ${JSON.stringify(value)} of ${term.name} (${type} vs ${known.type})`, expr.start)
      token = known.term
    }
    return token
  }
}

function reduceAction(rule: Rule, partOfSkip: Term | null, depth = rule.parts.length) {
  return rule.name.id | REDUCE_FLAG |
    (rule.isRepeatLeaf && depth == rule.parts.length ? REDUCE_REPEAT_FLAG : 0) |
    (rule.name == partOfSkip ? STAY_FLAG : 0) |
    (depth << REDUCE_DEPTH_SHIFT)
}

function isSimpleSkip(action: Shift | Reduce, skipRule: Term) {
  return action instanceof Shift && !!action.target.defaultReduce && action.target.defaultReduce.name == skipRule
}

class StateDataBuilder {
  data: number[] = []

  storeArray(data: number[]) {
    search: for (let i = 0;;) {
      let next = this.data.indexOf(data[0], i)
      if (next == -1 || next + data.length > this.data.length) break
      for (let j = 1; j < data.length; j++) {
        if (data[j] != this.data[next + j]) {
          i = next + 1
          continue search
        }
      }
      return next
    }
    let pos = this.data.length
    for (let num of data) this.data.push(num)
    return pos
  }

  finish() {
    return Uint16Array.from(this.data)
  }
}

function computeGotoTable(states: readonly LRState[]) {
  let goto: {[term: number]: {[to: number]: number[]}} = {}
  let maxTerm = 0
  for (let state of states)
    for (let entry of state.goto) {
      maxTerm = Math.max(entry.term.id, maxTerm)
      let set = goto[entry.term.id] || (goto[entry.term.id] = {})
      ;(set[entry.target.id] || (set[entry.target.id] = [])).push(state.id)
    }
  let data: number[] = [], index: number[] = []
  for (let term = 0; term <= maxTerm; term++) {
    let entries = goto[term]
    if (!entries) {
      index.push(1)
      continue
    }
    index.push(data.length + maxTerm + 2) // Offset of the data, taking index size into account
    let keys = Object.keys(entries)
    for (let target of keys) {
      let list = entries[target as any]
      data.push((target == keys[keys.length - 1] ? 1 : 0) + (list.length << 1))
      data.push(+target)
      for (let source of list) data.push(source)
    }
  }

  return Uint16Array.from([maxTerm + 1, ...index, ...data])
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

class NestedGrammar {
  constructor(readonly placeholder: Term,
              readonly extName: string,
              readonly source: string,
              readonly end: State) {}
}

class NestNamespace implements Namespace {
  resolve(expr: NamedExpression, builder: Builder): Parts[] {
    if (expr.args.length < 2 || expr.args.length > 3 || !(expr.args[0] instanceof Identifier))
      builder.raise(`Invalid number of arguments to 'nest.${expr.id.name}'`, expr.start)
    let [grammar, endExpr, defaultExpr] = expr.args as [Identifier, Expression, Expression | undefined]
    let extGrammar = builder.ast.grammars.find(g => g.id.name == grammar.name)
    if (!extGrammar) return builder.raise(`No external grammar '${grammar.name}' defined`, grammar.start)
    let term = builder.newName(expr.id.name, expr.id.name)
    builder.defineRule(term, defaultExpr ? builder.normalizeExpr(defaultExpr) : [])
    let endStart = new State, endEnd = new State([builder.terms.eof])
    builder.tokens.build(endExpr, endStart, endEnd, none)
    builder.nestedGrammars.push(new NestedGrammar(term, extGrammar.externalID.name, extGrammar.source, endStart))
    return [p(term)]
  }
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

  getToken(expr: NamedExpression) {
    for (let built of this.built) if (built.matches(expr)) return built.term
    let name = expr.id.name
    let rule = this.rules.find(r => r.id.name == name)
    if (!rule) return null
    let term = this.b.makeTerminal(expr.toString(), rule.tag ? rule.tag.name : isTag(name))
    if (expr.args.length == 0) this.b.namedTerms[expr.id.name] = term
    this.buildRule(rule, expr, this.startState, new State([term]))
    this.built.push(new BuiltRule(name, expr.args, term))
    return term
  }

  getLiteral(expr: LiteralExpression) {
    let id = JSON.stringify(expr.value)
    for (let built of this.built) if (built.id == id) return built.term
    let term = this.b.makeTerminal(id, id)
    this.build(expr, this.startState, new State([term]), none)
    this.built.push(new BuiltRule(id, none, term))
    return term
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
    this.b.used(rule.id.name)
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
      let rule = this.rules.find(r => r.id.name == name)
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
        if (item instanceof NamedExpression) {
          known = this.built.find(b => b.matches(item as NamedExpression))
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
      // FIXME this causes us to not tokenize whitespace for states that don't match any group
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

    // FIXME more helpful message?
    if (groups.length > 16) this.b.raise(`Too many different token groups to represent them as a 16-bit bitfield`)

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

class ExternalTokenSet {
  tokens: {[name: string]: Term} = Object.create(null)

  constructor(readonly b: Builder, readonly ast: ExternalTokenDeclaration) {
    for (let token of ast.tokens) {
      b.unique(token.id)
      let term = b.makeTerminal(token.id.name, token.tag ? token.tag.name : null)
      b.namedTerms[token.id.name] = this.tokens[token.id.name] = term
      this.b.tokenOrigins[term.name] = this
    }
  }

  getToken(expr: NamedExpression) {
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
  
  token(stream: any, stack: any) {
    this.inner.token(stream, stack)
  }

  get contextual() { return this.inner.contextual }
}


// FIXME maybe add a pass that, if there's a tagless token whole only
// use is in a tagged single-term rule, move the tag to the token and
// collapse the rule.

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

export function buildParser(text: string, options: BuildOptions = {}): Parser {
  return new Builder(text, options).getParser()
}

function flattenStates(states: readonly ParseState[]): number[] {
  let data = []
  for (let state of states) {
    data.push(state.flags, state.actions, state.recover, state.skip, state.tokenizerMask, state.defaultReduce, state.forcedReduce)
  }
  return data
}

const KEYWORDS = ["break", "case", "catch", "continue", "debugger", "default", "do", "else", "finally",
                  "for", "function", "if", "return", "switch", "throw", "try", "var", "while", "with",
                  "null", "true", "false", "instanceof", "typeof", "void", "delete", "new", "in", "this",
                  "const", "class", "extends", "export", "import", "super", "enum", "implements", "interface",
                  "let", "package", "private", "protected", "public", "static", "yield"]

export function buildParserFile(text: string, options: BuildOptions = {}): {parser: string, terms: string} {
  let builder = new Builder(text, options), parser = builder.getParser()
  let mod = options.moduleStyle || "cjs"

  let gen = "// This file was generated by lezer-generator. You probably shouldn't edit it.\n", head = gen
  head += mod == "cjs" ? `const {Parser} = require("lezer")\n`
    : `import {Parser} from "lezer"\n`
  let tokenData = null, imports: {[source: string]: string[]} = {}
  let defined = Object.create(null)
  defined.Parser = true
  let getName = (prefix: string) => {
    for (let i = 0;; i++) {
      let id = prefix + (i ? "_" + i : "")
      if (!defined[id]) return id
    }
  }

  let tokenizers = parser.tokenizers.map(tok => {
    if (tok instanceof TempExternalTokenizer) {
      let {source, id: {name}} = tok.set.ast
      let src = JSON.stringify(source), varName = name
      if (name in defined) {
        varName = getName("tok")
        name += ` as ${varName}`
      }
      ;(imports[src] || (imports[src] = [])).push(name)
      return varName
    } else {
      tokenData = (tok as LezerTokenGroup).data
      return (tok as LezerTokenGroup).id
    }
  })
  for (let source in imports) {
    if (mod == "cjs")
      head += `const {${imports[source].join(", ")}} = require(${source})\n`
    else
      head += `import {${imports[source].join(", ")}} from ${source}\n`
  }

  let tagNames: {[tag: string]: null | string} = Object.create(null)
  let tagDefs: string[] = [], tagArray = []
  for (let i = 1;; i += 2) {
    let tag = parser.tags.get(i | parser.id)
    if (tag == null) break
    if (!(tag! in tagNames)) {
      tagNames[tag!] = null
    } else if (tagNames[tag!] == null) {
      let name = getName(tag!)
      tagNames[tag!] = name
      tagDefs.push(`${name} = ${JSON.stringify(tag)}`)
    }
  }
  if (tagDefs.length) head += `const ${tagDefs.join(", ")}\n`
  for (let i = 1;; i += 2) {
    let tag = parser.tags.get(i | parser.id)
    if (tag == null) break
    tagArray.push(tagNames[tag] || JSON.stringify(tag))
  }

  let parserStr = `Parser.deserialize(
  ${encodeArray(flattenStates(parser.states), 0xffffffff)},
  ${encodeArray(parser.data)},
  ${encodeArray(parser.goto)},
  [${tagArray.join(",")}],
  ${encodeArray(tokenData || [])},
  [${tokenizers.join(", ")}],
  ${parser.specializeTable},
  ${JSON.stringify(parser.specializations)},
  ${parser.tokenPrecTable},
  ${parser.skippedNodes}${options.includeNames ? `,
  ${JSON.stringify(parser.termNames)}` : ''}
)`

  let terms: string[] = []
  for (let name in builder.termTable) {
    let id = name
    if (KEYWORDS.includes(id)) for (let i = 1;; i++) {
      id = "_".repeat(i) + name
      if (!(id in builder.termTable)) break
    }
    terms.push(`${id} = ${builder.termTable[name]}`)
  }

  return {parser: head + (mod == "cjs" ? `module.exports = ${parserStr}\n` : `export default ${parserStr}\n`),
          terms: `${gen}export const\n  ${terms.join(",\n  ")}\n`}
}
