import {GrammarDeclaration, RuleDeclaration, TokenDeclaration, LocalTokenDeclaration,
        ExternalTokenDeclaration, ExternalSpecializeDeclaration,
        Expression, Identifier, LiteralExpression, NameExpression, SequenceExpression,
        ChoiceExpression, RepeatExpression, SetExpression, AnyExpression, ConflictMarker,
        InlineRuleExpression, SpecializeExpression, Prop, PropPart, CharClass, CharClasses,
        exprsEq, exprEq} from "./node"
import {Term, TermSet, Rule, Conflicts, Props, hasProps} from "./grammar"
import {State, MAX_CHAR, Conflict} from "./token"
import {Input} from "./parse"
import {computeFirstSets, buildFullAutomaton, finishAutomaton, State as LRState, Shift, Reduce, Pos} from "./automaton"
import {encodeArray} from "./encode"
import {GenError} from "./error"
import {verbose, time} from "./log"
import {NodeProp, NodePropSource} from "@lezer/common"
import {LRParser, ExternalTokenizer, LocalTokenGroup, Stack, ContextTracker} from "@lezer/lr"
import {Action, Specialize, StateFlag, Seq, ParseState, File} from "@lezer/lr/dist/constants"

const none: readonly any[] = []

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
    return this.id == "+" && exprEq(expr.expr, this.args[0])
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
  /// Set this to true to output TypeScript code instead of plain
  /// JavaScript.
  typeScript?: boolean,
  /// The name of the export that holds the parser in the output file.
  /// Defaults to `"parser"`.
  exportName?: string,
  /// When calling `buildParser`, this can be used to provide
  /// placeholders for external tokenizers.
  externalTokenizer?: (name: string, terms: {[name: string]: number}) => ExternalTokenizer
  /// Used by `buildParser` to resolve external prop sources.
  externalPropSource?: (name: string) => NodePropSource
  /// Provide placeholders for external specializers when using
  /// `buildParser`.
  externalSpecializer?: (name: string, terms: {[name: string]: number}) => (value: string, stack: Stack) => number
  /// If given, will be used to initialize external props in the parser
  /// returned by `buildParser`.
  externalProp?: (name: string) => NodeProp<any>
  /// If given, will be used as context tracker in a parser built with
  /// `buildParser`.
  contextTracker?: ContextTracker<any> | ((terms: {[name: string]: number}) => ContextTracker<any>)
}

type SkipInfo = {skip: readonly Term[], rule: Term | null, startTokens: readonly Term[], id: number}

class Builder {
  ast!: GrammarDeclaration
  input!: Input
  terms = new TermSet
  tokens: MainTokenSet
  localTokens: readonly LocalTokenSet[]
  externalTokens: ExternalTokenSet[]
  externalSpecializers: ExternalSpecializer[]
  specialized: {[name: string]: {value: string, name: string | null, term: Term, type: string, dialect: number | null}[]}
    = Object.create(null)
  tokenOrigins: {[name: string]: {spec?: Term, external?: ExternalTokenSet | ExternalSpecializer,
                                  group?: LocalTokenSet}} = Object.create(null)
  rules: Rule[] = []
  built: BuiltRule[] = []
  ruleNames: {[name: string]: Identifier | null} = Object.create(null)
  namespaces: {[name: string]: Namespace} = Object.create(null)
  namedTerms: {[name: string]: Term} = Object.create(null)
  termTable: {[name: string]: number} = Object.create(null)
  knownProps: {[name: string]: {prop: NodeProp<any>, source: {name: string, from: string | null}}} = Object.create(null)
  dialects: readonly string[]
  dynamicRulePrecedences: {rule: Term, prec: number}[] = []
  definedGroups: {name: Term, group: string, rule: RuleDeclaration}[] = []

  astRules: {skip: Term, rule: RuleDeclaration}[] = []
  currentSkip: Term[] = []
  skipRules!: Term[]

  constructor(text: string, readonly options: BuildOptions) {
    time("Parse", () => {
      this.input = new Input(text, options.fileName)
      this.ast = this.input.parse()
    })

    let NP: {[key: string]: any} = NodeProp
    for (let prop in NP) {
      if (NP[prop] instanceof NodeProp && !NP[prop].perNode)
        this.knownProps[prop] = {prop: NP[prop], source: {name: prop, from: null}}
    }
    for (let prop of this.ast.externalProps) {
      this.knownProps[prop.id.name] = {
        prop: this.options.externalProp ? this.options.externalProp(prop.id.name) : new NodeProp<string>(),
        source: {name: prop.externalID.name, from: prop.source}
      }
    }

    this.dialects = this.ast.dialects.map(d => d.name)

    this.tokens = new MainTokenSet(this, this.ast.tokens)
    this.localTokens = this.ast.localTokens.map(g => new LocalTokenSet(this, g))
    this.externalTokens = this.ast.externalTokens.map(ext => new ExternalTokenSet(this, ext))
    this.externalSpecializers = this.ast.externalSpecializers.map(decl => new ExternalSpecializer(this, decl))

    time("Build rules", () => {
      let noSkip = this.newName("%noskip", true)
      this.defineRule(noSkip, [])

      let mainSkip = this.ast.mainSkip ? this.newName("%mainskip", true) : noSkip
      let scopedSkip: Term[] = [], topRules: {rule: RuleDeclaration, skip: Term}[] = []
      for (let rule of this.ast.rules) this.astRules.push({skip: mainSkip, rule})
      for (let rule of this.ast.topRules) topRules.push({skip: mainSkip, rule})
      for (let scoped of this.ast.scopedSkip) {
        let skip = noSkip, found = this.ast.scopedSkip.findIndex((sc, i) => i < scopedSkip.length && exprEq(sc.expr, scoped.expr))
        if (found > -1) skip = scopedSkip[found]
        else if (this.ast.mainSkip && exprEq(scoped.expr, this.ast.mainSkip)) skip = mainSkip
        else if (!isEmpty(scoped.expr)) skip = this.newName("%skip", true)
        scopedSkip.push(skip)
        for (let rule of scoped.rules) this.astRules.push({skip, rule})
        for (let rule of scoped.topRules) topRules.push({skip, rule})
      }

      for (let {rule} of this.astRules) {
        this.unique(rule.id)
      }

      this.currentSkip.push(noSkip)
      this.skipRules = mainSkip == noSkip ? [mainSkip] : [noSkip, mainSkip]
      if (mainSkip != noSkip)
        this.defineRule(mainSkip, this.normalizeExpr(this.ast.mainSkip!))
      for (let i = 0; i < this.ast.scopedSkip.length; i++) {
        let skip = scopedSkip[i]
        if (!this.skipRules.includes(skip)) {
          this.skipRules.push(skip)
          if (skip != noSkip)
            this.defineRule(skip, this.normalizeExpr(this.ast.scopedSkip[i].expr))
        }
      }
      this.currentSkip.pop()

      for (let {rule, skip} of topRules.sort((a, b) => a.rule.start - b.rule.start)) {
        this.unique(rule.id)
        this.used(rule.id.name)
        this.currentSkip.push(skip)
        let {name, props} = this.nodeInfo(rule.props, "a", rule.id.name, none, none, rule.expr)
        let term = this.terms.makeTop(name, props)
        this.namedTerms[name!] = term
        this.defineRule(term, this.normalizeExpr(rule.expr))
        this.currentSkip.pop()
      }

      for (let ext of this.externalSpecializers) ext.finish()

      for (let {skip, rule} of this.astRules) {
        if (this.ruleNames[rule.id.name] && isExported(rule) && !rule.params.length) {
          this.buildRule(rule, [], skip, false)
          if (rule.expr instanceof SequenceExpression && rule.expr.exprs.length == 0)
            this.used(rule.id.name)
        }
      }
    })

    for (let name in this.ruleNames) {
      let value = this.ruleNames[name]
      if (value) this.warn(`Unused rule '${value.name}'`, value.start)
    }

    this.tokens.takePrecedences()
    this.tokens.takeConflicts()
    for (let lt of this.localTokens) lt.takePrecedences()

    for (let {name, group, rule} of this.definedGroups) this.defineGroup(name, group, rule)
    this.checkGroups()
  }

  unique(id: Identifier) {
    if (id.name in this.ruleNames)
      this.raise(`Duplicate definition of rule '${id.name}'`, id.start)
    this.ruleNames[id.name] = id
  }

  used(name: string) {
    this.ruleNames[name] = null
  }

  newName(base: string, nodeName: string | null | true = null, props: Props = {}): Term {
    for (let i = nodeName ? 0 : 1;; i++) {
      let name = i ? `${base}-${i}` : base
      if (!this.terms.names[name])
        return this.terms.makeNonTerminal(name, nodeName === true ? null : nodeName, props)
    }
  }

  prepareParser() {
    let rules = time("Simplify rules", () => simplifyRules(this.rules, [
      ...this.skipRules,
      ...this.terms.tops]))
    let {nodeTypes, names: termNames, minRepeatTerm, maxTerm} = this.terms.finish(rules)
    for (let prop in this.namedTerms) this.termTable[prop] = this.namedTerms[prop].id

    if (/\bgrammar\b/.test(verbose)) console.log(rules.join("\n"))

    let startTerms = this.terms.tops.slice()
    let first = computeFirstSets(this.terms)
    let skipInfo: readonly SkipInfo[] = this.skipRules.map((name, id) => {
      let skip = [], startTokens: Term[] = [], rules: Rule[] = []
      for (let rule of name.rules) {
        if (!rule.parts.length) continue
        let start = rule.parts[0]
        for (let t of start.terminal ? [start] : first[start.name] || [])
          if (t && !startTokens.includes(t)) startTokens.push(t)
        if (start.terminal && rule.parts.length == 1 && !rules.some(r => r != rule && r.parts[0] == start))
          skip.push(start)
        else
          rules.push(rule)
      }
      name.rules = rules
      if (rules.length) startTerms.push(name)
      return {skip, rule: rules.length ? name : null, startTokens, id}
    })
    let fullTable = time("Build full automaton", () => buildFullAutomaton(this.terms, startTerms, first))
    let localTokens = this.localTokens
      .map((grp, i) => grp.buildLocalGroup(fullTable, skipInfo, i))
    let {tokenGroups, tokenPrec, tokenData} =
      time("Build token groups", () => this.tokens.buildTokenGroups(fullTable, skipInfo, localTokens.length))
    let table = time("Finish automaton", () => finishAutomaton(fullTable))
    let skipState = findSkipStates(table, this.terms.tops)

    if (/\blr\b/.test(verbose)) console.log(table.join("\n"))

    let specialized: (ExternalSpecializer | {token: Term, table: {[value: string]: number}})[] = []
    for (let ext of this.externalSpecializers)
      specialized.push(ext)
    for (let name in this.specialized)
      specialized.push({token: this.terms.names[name], table: buildSpecializeTable(this.specialized[name])})

    let tokStart = (tokenizer: TokenGroup | ExternalTokenSet) => {
      if (tokenizer instanceof ExternalTokenSet) return tokenizer.ast.start
      return this.tokens.ast ? this.tokens.ast.start : -1
    }
    let tokenizers = ((tokenGroups as (TokenGroup | ExternalTokenSet)[])
      .concat(this.externalTokens)
      .sort((a, b) => tokStart(a) - tokStart(b)) as TokenizerSpec[])
      .concat(localTokens)

    let data = new DataBuilder
    let skipData = skipInfo.map(info => {
      let actions: number[] = []
      for (let term of info.skip)
        actions.push(term.id, 0, Action.StayFlag >> 16)
      if (info.rule) {
        let state = table.find(s => s.startRule == info.rule)!
        for (let action of state.actions as Shift[])
          actions.push(action.term.id, state.id, Action.GotoFlag >> 16)
      }
      actions.push(Seq.End, Seq.Done)
      return data.storeArray(actions)
    })
    let states = time("Finish states", () => {
      let states = new Uint32Array(table.length * ParseState.Size)
      let forceReductions = this.computeForceReductions(table, skipInfo)
      let finishCx = new FinishStateContext(tokenizers, data, states, skipData, skipInfo, table, this)
      for (let s of table) finishCx.finish(s, skipState(s.id), forceReductions[s.id])
      return states
    })
    let dialects: {[name: string]: number} = Object.create(null)
    for (let i = 0; i < this.dialects.length; i++)
      dialects[this.dialects[i]] = data.storeArray((this.tokens.byDialect[i] || none).map(t => t.id).concat(Seq.End))

    let dynamicPrecedences = null
    if (this.dynamicRulePrecedences.length) {
      dynamicPrecedences = Object.create(null)
      for (let {rule, prec} of this.dynamicRulePrecedences) dynamicPrecedences[rule.id] = prec
    }

    let topRules: {[rule: string]: [number, number]} = Object.create(null)
    for (let term of this.terms.tops)
      topRules[term.nodeName!] = [table.find(state => state.startRule == term)!.id, term.id]

    let precTable = data.storeArray(tokenPrec.concat(Seq.End))
    let {nodeProps, skippedTypes} = this.gatherNodeProps(nodeTypes)

    return {
      states,
      stateData: data.finish(),
      goto: computeGotoTable(table),
      nodeNames: nodeTypes.filter(t => t.id < minRepeatTerm).map(t => t.nodeName).join(" "),
      nodeProps,
      skippedTypes,
      maxTerm,
      repeatNodeCount: nodeTypes.length - minRepeatTerm,
      tokenizers,
      tokenData,
      topRules,
      dialects,
      dynamicPrecedences,
      specialized,
      tokenPrec: precTable,
      termNames
    }
  }

  getParser() {
    let {
      states,
      stateData,
      goto,
      nodeNames,
      nodeProps: rawNodeProps,
      skippedTypes,
      maxTerm,
      repeatNodeCount,
      tokenizers,
      tokenData,
      topRules,
      dialects,
      dynamicPrecedences,
      specialized: rawSpecialized,
      tokenPrec,
      termNames
    } = this.prepareParser()

    let specialized = rawSpecialized.map(v => {
      if (v instanceof ExternalSpecializer) {
        let ext = this.options.externalSpecializer!(v.ast.id.name, this.termTable)
        return {
          term: v.term!.id,
          get: (value: string, stack: Stack) => (ext(value, stack) << 1) |
            (v.ast.type == "extend" ? Specialize.Extend : Specialize.Specialize),
          external: ext,
          extend: v.ast.type == "extend"
        }
      } else {
        return {term: v.token.id, get: (value: string) => v.table[value] || -1}
      }
    })

    return LRParser.deserialize({
      version: File.Version,
      states,
      stateData,
      goto,
      nodeNames,
      maxTerm,
      repeatNodeCount,
      nodeProps: rawNodeProps.map(({prop, terms}) => [this.knownProps[prop].prop, ...terms]),
      propSources: !this.options.externalPropSource ? undefined
        : this.ast.externalPropSources.map(s => this.options.externalPropSource!(s.id.name)),
      skippedNodes: skippedTypes,
      tokenData,
      tokenizers: tokenizers.map(tok => tok.create()),
      context: !this.ast.context ? undefined
        : typeof this.options.contextTracker == "function" ? this.options.contextTracker(this.termTable)
        : this.options.contextTracker,
      topRules,
      dialects,
      dynamicPrecedences,
      specialized,
      tokenPrec,
      termNames
    }) as LRParser
  }

  getParserFile() {
    let {
      states,
      stateData,
      goto,
      nodeNames,
      nodeProps: rawNodeProps,
      skippedTypes,
      maxTerm,
      repeatNodeCount,
      tokenizers: rawTokenizers,
      tokenData,
      topRules,
      dialects: rawDialects,
      dynamicPrecedences,
      specialized: rawSpecialized,
      tokenPrec,
      termNames
    } = this.prepareParser()

    let mod = this.options.moduleStyle || "es"

    let gen = "// This file was generated by lezer-generator. You probably shouldn't edit it.\n", head = gen
    let imports: {[source: string]: string[]} = {}, imported: {[spec: string]: string} = Object.create(null)
    let defined: {[name: string]: boolean} = Object.create(null)
    for (let word of KEYWORDS) defined[word] = true
    let exportName = this.options.exportName || "parser"
    defined[exportName] = true
    let getName = (prefix: string) => {
      for (let i = 0;; i++) {
        let id = prefix + (i ? "_" + i : "")
        if (!defined[id]) return id
      }
    }

    let importName = (name: string, source: string, prefix: string = name) => {
      let spec = name + " from " + source
      if (imported[spec]) return imported[spec]
      let src = JSON.stringify(source), varName = name
      if (name in defined) {
        varName = getName(prefix)
        name += `${mod == "cjs" ? ":" : " as"} ${varName}`
      }
      defined[varName] = true
      ;(imports[src] || (imports[src] = [])).push(name)
      return imported[spec] = varName
    }
    let lrParser = importName("LRParser", "@lezer/lr")

    let tokenizers = rawTokenizers.map(tok => tok.createSource(importName))

    let context = this.ast.context ? importName(this.ast.context.id.name, this.ast.context.source) : null

    let nodeProps = rawNodeProps.map(({prop, terms}) => {
      let {source} = this.knownProps[prop]
      let propID = source.from ? importName(source.name, source.from) : JSON.stringify(source.name)
      return `[${propID}, ${terms.map(serializePropValue).join(",")}]`
    })

    function specializationTableString(table: {[name: string]: number}) {
      return "{__proto__:null," + Object.keys(table).map(key => `${/^(\d+|[a-zA-Z_]\w*)$/.test(key) ? key : JSON.stringify(key)}:${table[key]}`)
        .join(", ") + "}"
    }

    let specHead = ""
    let specialized = rawSpecialized.map(v => {
      if (v instanceof ExternalSpecializer) {
        let name = importName(v.ast.id.name, v.ast.source)
        let ts = this.options.typeScript ? ": any" : ""
        return `{term: ${v.term!.id}, get: (value${ts}, stack${ts}) => (${name}(value, stack) << 1)${
          v.ast.type == "extend" ? ` | ${Specialize.Extend}` : ''}, external: ${name}${
          v.ast.type == "extend" ? ', extend: true' : ''}}`
      } else {
        let tableName = getName("spec_" + v.token.name.replace(/\W/g, ""))
        defined[tableName] = true
        specHead += `const ${tableName} = ${specializationTableString(v.table)}\n`
        let ts = this.options.typeScript ? `: keyof typeof ${tableName}` : ""
        return `{term: ${v.token.id}, get: (value${ts}) => ${tableName}[value] || -1}`
      }
    })

    let propSources = this.ast.externalPropSources.map(s => importName(s.id.name, s.source))

    for (let source in imports) {
      if (mod == "cjs")
        head += `const {${imports[source].join(", ")}} = require(${source})\n`
      else
        head += `import {${imports[source].join(", ")}} from ${source}\n`
    }

    head += specHead

    function serializePropValue(value: any) {
      return typeof value != "string" || /^(true|false|\d+(\.\d+)?|\.\d+)$/.test(value) ? value : JSON.stringify(value)
    }

    let dialects = Object.keys(rawDialects).map(d => `${d}: ${rawDialects[d]}`)

    let parserStr = `${lrParser}.deserialize({
  version: ${File.Version},
  states: ${encodeArray(states, 0xffffffff)},
  stateData: ${encodeArray(stateData)},
  goto: ${encodeArray(goto)},
  nodeNames: ${JSON.stringify(nodeNames)},
  maxTerm: ${maxTerm}${context ? `,
  context: ${context}` : ""}${nodeProps.length ? `,
  nodeProps: [
    ${nodeProps.join(",\n    ")}
  ]` : ""}${propSources.length ? `,
  propSources: [${propSources.join()}]` : ""}${skippedTypes.length ? `,
  skippedNodes: ${JSON.stringify(skippedTypes)}` : ""},
  repeatNodeCount: ${repeatNodeCount},
  tokenData: ${encodeArray(tokenData)},
  tokenizers: [${tokenizers.join(", ")}],
  topRules: ${JSON.stringify(topRules)}${dialects.length ? `,
  dialects: {${dialects.join(", ")}}` : ""}${dynamicPrecedences ? `,
  dynamicPrecedences: ${JSON.stringify(dynamicPrecedences)}` : ""}${specialized.length ? `,
  specialized: [${specialized.join(",")}]` : ""},
  tokenPrec: ${tokenPrec}${this.options.includeNames ? `,
  termNames: ${JSON.stringify(termNames)}` : ''}
})`

    let terms: string[] = []
    for (let name in this.termTable) {
      let id = name
      if (KEYWORDS.includes(id)) for (let i = 1;; i++) {
        id = "_".repeat(i) + name
        if (!(id in this.termTable)) break
      } else if (!/^[\w$]+$/.test(name)) {
        continue
      }
      terms.push(`${id}${mod == "cjs" ? ":" : " ="} ${this.termTable[name]}`)
    }
    for (let id = 0; id < this.dialects.length; id++)
      terms.push(`Dialect_${this.dialects[id]}${mod == "cjs" ? ":" : " ="} ${id}`)

    return {
      parser: head + (mod == "cjs" ? `exports.${exportName} = ${parserStr}\n` : `export const ${exportName} = ${parserStr}\n`),
      terms: mod == "cjs" ? `${gen}module.exports = {\n  ${terms.join(",\n  ")}\n}`
        : `${gen}export const\n  ${terms.join(",\n  ")}\n`
    }
  }

  gatherNonSkippedNodes() {
    let seen: {[term: number]: boolean} = Object.create(null)
    let work: Term[] = []
    let add = (term: Term) => {
      if (!seen[term.id]) {
        seen[term.id] = true
        work.push(term)
      }
    }
    this.terms.tops.forEach(add)
    for (let i = 0; i < work.length; i++) {
      for (let rule of work[i].rules) for (let part of rule.parts) add(part)
    }
    return seen
  }

  gatherNodeProps(nodeTypes: readonly Term[]) {
    let notSkipped = this.gatherNonSkippedNodes(), skippedTypes = []
    let nodeProps: {prop: string, values: {[val: string]: number[]}}[] = []
    for (let type of nodeTypes) {
      if (!notSkipped[type.id] && !type.error) skippedTypes.push(type.id)
      for (let prop in type.props) {
        let known = this.knownProps[prop]
        if (!known) throw new GenError("No known prop type for " + prop)
        if (known.source.from == null && (known.source.name == "repeated" || known.source.name == "error")) continue
        let rec = nodeProps.find(r => r.prop == prop)
        if (!rec) nodeProps.push(rec = {prop, values: {}})
        ;(rec.values[type.props[prop]] || (rec.values[type.props[prop]] = [])).push(type.id)
      }
    }
    return {
      nodeProps: nodeProps.map(({prop, values}) => {
        let terms: (string | number)[] = []
        for (let val in values) {
          let ids = values[val]
          if (ids.length == 1) {
            terms.push(ids[0], val)
          } else {
            terms.push(-ids.length)
            for (let id of ids) terms.push(id)
            terms.push(val)
          }
        }
        return {prop, terms}
      }),
      skippedTypes
    }
  }

  makeTerminal(name: string, tag: string | null, props: Props) {
    return this.terms.makeTerminal(this.terms.uniqueName(name), tag, props)
  }

  computeForceReductions(states: readonly LRState[], skipInfo: readonly SkipInfo[]) {
    // This finds a forced reduction for every state, trying to guard
    // against cyclic forced reductions, where a given parse stack can
    // endlessly continue running forced reductions without making any
    // progress.
    //
    // This occurs with length-1 reductions. We never generate
    // length-0 reductions, and length-2+ reductions always shrink the
    // stack, so they are guaranteed to make progress.
    //
    // If there are states S1 and S2 whose forced reductions reduce
    // terms T1 and T2 respectively, both with a length of 1, _and_
    // there is a state S3, which has goto entries T1 -> S2, T2 -> S1,
    // you can get cyclic reductions. Of course, the cycle may also
    // contain more than two steps.
    let reductions: number[] = []
    let candidates: Pos[][] = []
    // A map from terms to states that they are mapped to in goto
    // entries.
    let gotoEdges: {[term: number]: {parents: number[], target: number}[]} = Object.create(null)
    for (let state of states) {
      reductions.push(0)
      for (let edge of state.goto) {
        let array = gotoEdges[edge.term.id] || (gotoEdges[edge.term.id] = [])
        let found = array.find(o => o.target == edge.target.id)
        if (found) found.parents.push(state.id)
        else array.push({parents: [state.id], target: edge.target.id})
      }
      candidates[state.id] = state.set.filter(pos => pos.pos > 0 && !pos.rule.name.top)
        .sort((a, b) => b.pos - a.pos || a.rule.parts.length - b.rule.parts.length)
    }
    // Mapping from state ids to terms that that state has a length-1
    // forced reduction for.
    let length1Reductions: {[state: number]: number} = Object.create(null)
    function createsCycle(term: number, startState: number, parents: number[] | null = null): boolean {
      let edges = gotoEdges[term]
      if (!edges) return false
      return edges.some(val => {
        let parentIntersection = parents ? parents.filter(id => val.parents.includes(id)) : val.parents
        if (parentIntersection.length == 0) return false
        if (val.target == startState) return true
        let found = length1Reductions[val.target]
        return found != null && createsCycle(found, startState, parentIntersection)
      })
    }

    for (let state of states) {
      if (state.defaultReduce && state.defaultReduce.parts.length > 0) {
        reductions[state.id] = reduceAction(state.defaultReduce, skipInfo)
        if (state.defaultReduce.parts.length == 1) length1Reductions[state.id] = state.defaultReduce.name.id
      }
    }
    // To avoid painting states that only have one potential forced
    // reduction into a corner, reduction assignment is done by
    // candidate size, starting with the states with fewer candidates.
    for (let setSize = 1;; setSize++) {
      let done = true
      for (let state of states) {
        if (state.defaultReduce) continue
        let set = candidates[state.id]
        if (set.length != setSize) {
          if (set.length > setSize) done = false
          continue
        }
        for (let pos of set) {
          if (pos.pos != 1 || !createsCycle(pos.rule.name.id, state.id)) {
            reductions[state.id] = reduceAction(pos.rule, skipInfo, pos.pos)
            if (pos.pos == 1) length1Reductions[state.id] = pos.rule.name.id
            break
          }
        }
      }
      if (done) break
    }
    return reductions
  }

  substituteArgs(expr: Expression, args: readonly Expression[], params: readonly Identifier[]) {
    if (args.length == 0) return expr
    return expr.walk(expr => {
      let found
      if (expr instanceof NameExpression &&
          (found = params.findIndex(p => p.name == expr.id.name)) > -1) {
        let arg = args[found]
        if (expr.args.length) {
          if (arg instanceof NameExpression && !arg.args.length)
            return new NameExpression(expr.start, arg.id, expr.args)
          this.raise(`Passing arguments to a parameter that already has arguments`, expr.start)
        }
        return arg
      } else if (expr instanceof InlineRuleExpression) {
        let r = expr.rule, props = this.substituteArgsInProps(r.props, args, params)
        return props == r.props ? expr :
          new InlineRuleExpression(expr.start, new RuleDeclaration(r.start, r.id, props, r.params, r.expr))
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
        if (expr instanceof NameExpression && !expr.args.length)
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
        result[i] = new Prop(prop.start, prop.at, prop.name, value)
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
  }

  resolve(expr: NameExpression): Parts[] {
    for (let built of this.built) if (built.matches(expr)) return [p(built.term)]

    let found = this.tokens.getToken(expr)
    if (found) return [p(found)]
    for (let grp of this.localTokens) {
      let found = grp.getToken(expr)
      if (found) return [p(found)]
    }
    for (let ext of this.externalTokens) {
      let found = ext.getToken(expr)
      if (found) return [p(found)]
    }
    for (let ext of this.externalSpecializers) {
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

  // For tree-balancing reasons, repeat expressions X+ have to be
  // normalized to something like
  //
  //     R -> X | R R
  //
  // Returns the `R` term.
  normalizeRepeat(expr: RepeatExpression) {
    let known = this.built.find(b => b.matchesRepeat(expr))
    if (known) return p(known.term)

    let name = expr.expr.prec < expr.prec ? `(${expr.expr})+` : `${expr.expr}+`
    let term = this.terms.makeRepeat(this.terms.uniqueName(name))
    this.built.push(new BuiltRule("+", [expr.expr], term))

    this.defineRule(term, this.normalizeExpr(expr.expr).concat(p(term, term)))
    return p(term)
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
      let repeated = this.normalizeRepeat(expr)
      return expr.kind == "+" ? [repeated] : [Parts.none, repeated]
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
      return [p(this.buildRule(expr.rule, none, this.currentSkip[this.currentSkip.length - 1], true))]
    } else {
      return this.raise(`This type of expression ('${expr}') may not occur in non-token rules`, expr.start)
    }
  }

  buildRule(rule: RuleDeclaration, args: readonly Expression[], skip: Term, inline = false): Term {
    let expr = this.substituteArgs(rule.expr, args, rule.params)
    let {name: nodeName, props, dynamicPrec, inline: explicitInline, group, exported} =
      this.nodeInfo(rule.props || none, inline ? "pg" : "pgi", rule.id.name, args, rule.params, rule.expr)
    if (exported && rule.params.length) this.warn(`Can't export parameterized rules`, rule.start)
    if (exported && inline) this.warn(`Can't export inline rule`, rule.start)
    let name = this.newName(rule.id.name + (args.length ? "<" + args.join(",") + ">" : ""), nodeName || true, props)
    if (explicitInline) name.inline = true
    if (dynamicPrec) this.registerDynamicPrec(name, dynamicPrec)
    if ((name.nodeType || exported) && rule.params.length == 0) {
      if (!nodeName) name.preserve = true
      if (!inline) this.namedTerms[exported || rule.id.name] = name
    }

    if (!inline) this.built.push(new BuiltRule(rule.id.name, args, name))
    this.currentSkip.push(skip)
    let parts = this.normalizeExpr(expr)
    if (parts.length > 100 * (expr instanceof ChoiceExpression ? expr.exprs.length : 1))
      this.warn(`Rule ${rule.id.name} is generating a lot (${parts.length}) of choices.\n  Consider splitting it up or reducing the amount of ? or | operator uses.`, rule.start)
    if (/\brulesize\b/.test(verbose) && parts.length > 10) console.log(`Rule ${rule.id.name}: ${parts.length} variants`)
    this.defineRule(name, parts)
    this.currentSkip.pop()
    if (group) this.definedGroups.push({name, group, rule})
    return name
  }

  nodeInfo(props: readonly Prop[],
           // p for dynamic precedence, d for dialect, i for inline, g for group, a for disabling the ignore test for default name
           allow: string,
           defaultName: string | null = null,
           args: readonly Expression[] = none, params: readonly Identifier[] = none,
           expr?: Expression, defaultProps?: Props): {
    name: string | null,
    props: Props,
    dialect: number | null,
    dynamicPrec: number,
    inline: boolean,
    group: string | null,
    exported: string | null
  } {
    let result: Props = {}
    let name = defaultName && (allow.indexOf("a") > -1 || !ignored(defaultName)) && !/ /.test(defaultName) ? defaultName : null
    let dialect = null, dynamicPrec = 0, inline = false, group: string | null = null, exported = null
    for (let prop of props) {
      if (!prop.at) {
        if (!this.knownProps[prop.name]) {
          let builtin = ["name", "dialect", "dynamicPrecedence", "export", "isGroup"].includes(prop.name)
            ? ` (did you mean '@${prop.name}'?)` : ""
          this.raise(`Unknown prop name '${prop.name}'${builtin}`, prop.start)
        }
        result[prop.name] = this.finishProp(prop, args, params)
      } else if (prop.name == "name") {
        name = this.finishProp(prop, args, params)
        if (/ /.test(name)) this.raise(`Node names cannot have spaces ('${name}')`, prop.start)
      } else if (prop.name == "dialect") {
        if (allow.indexOf("d") < 0)
          this.raise("Can't specify a dialect on non-token rules", props[0].start)
        if (prop.value.length != 1 && !prop.value[0].value)
          this.raise("The '@dialect' rule prop must hold a plain string value")
        let dialectID = this.dialects.indexOf(prop.value[0].value!)
        if (dialectID < 0) this.raise(`Unknown dialect '${prop.value[0].value}'`, prop.value[0].start)
        dialect = dialectID
      } else if (prop.name == "dynamicPrecedence") {
        if (allow.indexOf("p") < 0)
          this.raise("Dynamic precedence can only be specified on nonterminals")
        if (prop.value.length != 1 || !/^-?(?:10|\d)$/.test(prop.value[0].value!))
          this.raise("The '@dynamicPrecedence' rule prop must hold an integer between -10 and 10")
        dynamicPrec = +prop.value[0].value!
      } else if (prop.name == "inline") {
        if (prop.value.length) this.raise("'@inline' doesn't take a value", prop.value[0].start)
        if (allow.indexOf("i") < 0) this.raise("Inline can only be specified on nonterminals")
        inline = true
      } else if (prop.name == "isGroup") {
        if (allow.indexOf("g") < 0) this.raise("'@isGroup' can only be specified on nonterminals")
        group = prop.value.length ? this.finishProp(prop, args, params) : defaultName
      } else if (prop.name == "export") {
        if (prop.value.length) exported = this.finishProp(prop, args, params)
        else exported = defaultName
      } else {
        this.raise(`Unknown built-in prop name '@${prop.name}'`, prop.start)
      }
    }
    if (expr && this.ast.autoDelim && (name || hasProps(result))) {
      let delim = this.findDelimiters(expr)
      if (delim) {
        addToProp(delim[0], "closedBy", delim[1].nodeName!)
        addToProp(delim[1], "openedBy", delim[0].nodeName!)
      }
    }
    if (defaultProps && hasProps(defaultProps)) {
      for (let prop in defaultProps) if (!(prop in result)) result[prop] = defaultProps[prop]
    }
    if (hasProps(result) && !name)
      this.raise(`Node has properties but no name`, props.length ? props[0].start : expr!.start)
    if (inline && (hasProps(result) || dialect || dynamicPrec))
      this.raise(`Inline nodes can't have props, dynamic precedence, or a dialect`, props[0].start)
    if (inline && name) name = null
    return {name, props: result, dialect, dynamicPrec, inline, group, exported}
  }

  finishProp(prop: Prop, args: readonly Expression[], params: readonly Identifier[]): string {
    return prop.value.map(part => {
      if (part.value) return part.value
      let pos = params.findIndex(param => param.name == part.name)
      if (pos < 0) this.raise(`Property refers to '${part.name}', but no parameter by that name is in scope`, part.start)
      let expr = args[pos]
      if (expr instanceof NameExpression && !expr.args.length) return expr.id.name
      if (expr instanceof LiteralExpression) return expr.value
      return this.raise(`Expression '${expr}' can not be used as part of a property value`, part.start)
    }).join("")
  }

  resolveSpecialization(expr: SpecializeExpression) {
    let type = expr.type
    let {name, props, dialect, exported} = this.nodeInfo(expr.props, "d")
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
        if (!token) {
          token = this.makeTerminal(term.name + "/" + JSON.stringify(value), name, props)
          if (dialect != null) (this.tokens.byDialect[dialect] || (this.tokens.byDialect[dialect] = [])).push(token)
        }
        table.push({value, term: token, type, dialect, name})
        this.tokenOrigins[token.name] = {spec: term}
        if (name || exported) {
          if (!name) token.preserve = true
          this.namedTerms[exported || name!] = token
        }
      } else {
        if (known.type != type)
          this.raise(`Conflicting specialization types for ${JSON.stringify(value)} of ${term.name} (${type} vs ${known.type})`,
                     expr.start)
        if (known.dialect != dialect)
          this.raise(`Conflicting dialects for specialization ${JSON.stringify(value)} of ${term.name}`, expr.start)
        if (known.name != name)
          this.raise(`Conflicting names for specialization ${JSON.stringify(value)} of ${term.name}`, expr.start)
        if (token && known.term != token)
          this.raise(`Conflicting specialization tokens for ${JSON.stringify(value)} of ${term.name}`, expr.start)
        token = known.term
      }
    }
    return token!
  }

  findDelimiters(expr: Expression) {
    if (!(expr instanceof SequenceExpression) || expr.exprs.length < 2) return null
    let findToken = (expr: Expression): {term: Term, str: string} | null => {
      if (expr instanceof LiteralExpression) return {term: this.tokens.getLiteral(expr), str: expr.value}
      if (expr instanceof NameExpression && expr.args.length == 0) {
        let rule = this.ast.rules.find(r => r.id.name == expr.id.name)
        if (rule) return findToken(rule.expr)
        let token = this.tokens.rules.find(r => r.id.name == expr.id.name)
        if (token && token.expr instanceof LiteralExpression) return {term: this.tokens.getToken(expr)!, str: token.expr.value}
      }
      return null
    }
    let lastToken = findToken(expr.exprs[expr.exprs.length - 1])
    if (!lastToken || !lastToken.term.nodeName) return null
    const brackets = ["()", "[]", "{}", "<>"]
    let bracket = brackets.find(b => lastToken!.str.indexOf(b[1]) > -1 && lastToken!.str.indexOf(b[0]) < 0)
    if (!bracket) return null
    let firstToken = findToken(expr.exprs[0])
    if (!firstToken || !firstToken.term.nodeName ||
        firstToken.str.indexOf(bracket[0]) < 0 || firstToken.str.indexOf(bracket[1]) > -1) return null
    return [firstToken.term, lastToken.term]
  }

  registerDynamicPrec(term: Term, prec: number) {
    this.dynamicRulePrecedences.push({rule: term, prec})
    term.preserve = true
  }

  defineGroup(rule: Term, group: string, ast: RuleDeclaration) {
    let recur: Term[] = []
    let getNamed = (rule: Term): Term[] => {
      if (rule.nodeName) return [rule]
      if (recur.includes(rule))
        this.raise(`Rule '${ast.id.name}' cannot define a group because it contains a non-named recursive rule ('${rule.name}')`,
                   ast.start)
      let result: Term[] = []
      recur.push(rule)
      for (let r of this.rules) if (r.name == rule) {
        let names = r.parts.map(getNamed).filter(x => x.length)
        if (names.length > 1)
          this.raise(`Rule '${ast.id.name}' cannot define a group because some choices produce multiple named nodes`, ast.start)
        if (names.length == 1) for (let n of names[0]) result.push(n)
      }
      recur.pop()
      return result
    }

    for (let name of getNamed(rule))
      name.props["group"] = (name.props["group"]?.split(" ") || []).concat(group).sort().join(" ")
  }

  checkGroups() {
    let groups: {[name: string]: Term[]} = Object.create(null), nodeNames: {[name: string]: boolean} = Object.create(null)
    for (let term of this.terms.terms) if (term.nodeName) {
      nodeNames[term.nodeName] = true
      if (term.props["group"]) for (let group of term.props["group"].split(" ")) {
        ;(groups[group] || (groups[group] = [])).push(term)
      }
    }
    let names = Object.keys(groups)
    for (let i = 0; i < names.length; i++) {
      let name = names[i], terms = groups[name]
      if (nodeNames[name]) this.warn(`Group name '${name}' conflicts with a node of the same name`)
      for (let j = i + 1; j < names.length; j++) {
        let other = groups[names[j]]
        if (terms.some(t => other.includes(t)) &&
            (terms.length > other.length ? other.some(t => !terms.includes(t)) : terms.some(t => !other.includes(t))))
          this.warn(`Groups '${name}' and '${names[j]}' overlap without one being a superset of the other`)
      }
    }
  }
}

const MinSharedActions = 5

type SharedActions = {actions: readonly (Shift | Reduce)[], addr: number}

interface TokenizerSpec {
  groupID?: number,
  create: () => any,
  createSource: (importName: (name: string, source: string, prefix?: string) => string) => string
}

class FinishStateContext {
  sharedActions: SharedActions[] = []

  constructor(
    readonly tokenizers: TokenizerSpec[],
    readonly data: DataBuilder,
    readonly stateArray: Uint32Array,
    readonly skipData: readonly number[],
    readonly skipInfo: readonly SkipInfo[],
    readonly states: readonly LRState[],
    readonly builder: Builder
  ) {}

  findSharedActions(state: LRState): SharedActions | null {
    if (state.actions.length < MinSharedActions) return null
    let found = null
    for (let shared of this.sharedActions) {
      if ((!found || shared.actions.length > found.actions.length) &&
          shared.actions.every(a => state.actions.some(b => b.eq(a))))
        found = shared
    }
    if (found) return found
    let max: (Shift | Reduce)[] | null = null, scratch = []
    for (let i = state.id + 1; i < this.states.length; i++) {
      let other = this.states[i], fill = 0
      if (other.defaultReduce || other.actions.length < MinSharedActions) continue
      for (let a of state.actions) for (let b of other.actions) if (a.eq(b)) scratch[fill++] = a
      if (fill >= MinSharedActions && (!max || max.length < fill)) {
        max = scratch
        scratch = []
      }
    }
    if (!max) return null
    let result = {actions: max, addr: this.storeActions(max, -1, null)}
    this.sharedActions.push(result)
    return result
  }

  storeActions(actions: readonly (Shift | Reduce)[], skipReduce: number, shared: SharedActions | null) {
    if (skipReduce < 0 && shared && shared.actions.length == actions.length) return shared.addr

    let data = []
    for (let action of actions) {
      if (shared && shared.actions.some(a => a.eq(action))) continue
      if (action instanceof Shift) {
        data.push(action.term.id, action.target.id, 0)
      } else {
        let code = reduceAction(action.rule, this.skipInfo)
        if (code != skipReduce) data.push(action.term.id, code & Action.ValueMask, code >> 16)
      }
    }
    data.push(Seq.End)
    if (skipReduce > -1) data.push(Seq.Other, skipReduce & Action.ValueMask, skipReduce >> 16)
    else if (shared) data.push(Seq.Next, shared.addr & 0xffff, shared.addr >> 16)
    else data.push(Seq.Done)
    return this.data.storeArray(data)
  }

  finish(state: LRState, isSkip: boolean, forcedReduce: number) {
    let b = this.builder
    let skipID = b.skipRules.indexOf(state.skip)
    let skipTable = this.skipData[skipID], skipTerms = this.skipInfo[skipID].startTokens

    let defaultReduce = state.defaultReduce ? reduceAction(state.defaultReduce, this.skipInfo) : 0
    let flags = isSkip ? StateFlag.Skipped : 0

    let skipReduce = -1, shared = null
    if (defaultReduce == 0) {
      if (isSkip) for (const action of state.actions)
        if (action instanceof Reduce && action.term.eof)
          skipReduce = reduceAction(action.rule, this.skipInfo)
      if (skipReduce < 0) shared = this.findSharedActions(state)
    }

    if (state.set.some(p => p.rule.name.top && p.pos == p.rule.parts.length)) flags |= StateFlag.Accepting

    let external: TokenizerSpec[] = []
    for (let i = 0; i < state.actions.length + skipTerms.length; i++) {
      let term = i < state.actions.length ? state.actions[i].term : skipTerms[i - state.actions.length]
      for (;;) {
        let orig = b.tokenOrigins[term.name]
        if (orig && orig.spec) { term = orig.spec; continue }
        if (orig && (orig.external instanceof ExternalTokenSet)) addToSet(external, orig.external)
        break
      }
    }
    let tokenizerMask = 0
    for (let i = 0; i < this.tokenizers.length; i++) {
      let tok = this.tokenizers[i]
      if (external.includes(tok) || tok.groupID == state.tokenGroup)
        tokenizerMask |= (1 << i)
    }

    let base = state.id * ParseState.Size
    this.stateArray[base + ParseState.Flags] = flags
    this.stateArray[base + ParseState.Actions] = this.storeActions(defaultReduce ? none : state.actions, skipReduce, shared)
    this.stateArray[base + ParseState.Skip] = skipTable
    this.stateArray[base + ParseState.TokenizerMask] = tokenizerMask
    this.stateArray[base + ParseState.DefaultReduce] = defaultReduce
    this.stateArray[base + ParseState.ForcedReduce] = forcedReduce
  }
}

function addToProp(term: Term, prop: string, value: string) {
  let cur = term.props[prop]
  if (!cur || cur.split(" ").indexOf(value) < 0) term.props[prop] = cur ? cur + " " + value : value
}

function buildSpecializeTable(spec: {value: string, term: Term, type: string}[]): {[value: string]: number} {
  let table: {[value: string]: number} = Object.create(null)
  for (let {value, term, type} of spec) {
    let code = type == "specialize" ? Specialize.Specialize : Specialize.Extend
    table[value] = (term.id << 1) | code
  }
  return table
}

function reduceAction(rule: Rule, skipInfo: readonly SkipInfo[], depth = rule.parts.length) {
  return rule.name.id | Action.ReduceFlag |
    (rule.isRepeatWrap && depth == rule.parts.length ? Action.RepeatFlag : 0) |
    (skipInfo.some(i => i.rule == rule.name) ? Action.StayFlag : 0) |
    (depth << Action.ReduceDepthShift)
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

function findSkipStates(table: readonly LRState[], startRules: readonly Term[]) {
  let nonSkip: {[id: number]: boolean} = Object.create(null)
  let work: LRState[] = []
  let add = (state: LRState) => {
    if (!nonSkip[state.id]) {
      nonSkip[state.id] = true
      work.push(state)
    }
  }
  for (let state of table) if (state.startRule && startRules.includes(state.startRule)) add(state)
  for (let i = 0; i < work.length; i++) {
    for (let a of work[i].actions) if (a instanceof Shift) add(a.target)
    for (let a of work[i].goto) add(a.target)
  }
  return (id: number) => !nonSkip[id]
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
  for (let state of states) {
    for (let entry of state.goto) {
      maxTerm = Math.max(entry.term.id, maxTerm)
      let set = goto[entry.term.id] || (goto[entry.term.id] = {})
      ;(set[entry.target.id] || (set[entry.target.id] = [])).push(state.id)
    }
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
  if (index.some(n => n > 0xffff)) throw new GenError("Goto table too large")

  return Uint16Array.from([maxTerm + 1, ...index, ...data.data])
}

class TokenGroup implements TokenizerSpec {
  constructor(readonly tokens: Term[], readonly groupID: number) {}
  create() { return this.groupID }
  createSource() { return String(this.groupID) }
}

function addToSet<T>(set: T[], value: T) {
  if (!set.includes(value)) set.push(value)
}

function buildTokenMasks(groups: TokenGroup[]) {
  let masks: {[id: number]: number} = Object.create(null)
  for (let group of groups) {
    let groupMask = 1 << group.groupID
    for (let term of group.tokens) {
      masks[term.id] = (masks[term.id] || 0) | groupMask
    }
  }
  return masks
}

interface Namespace {
  resolve(expr: NameExpression, builder: Builder): Parts[]
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
  byDialect: {[dialect: number]: Term[]} = Object.create(null)
  precedenceRelations: readonly {term: Term, after: readonly Term[]}[] = []

  constructor(readonly b: Builder, readonly ast: TokenDeclaration | LocalTokenDeclaration | null) {
    this.rules = ast ? ast.rules : none
    for (let rule of this.rules) b.unique(rule.id)
  }

  getToken(expr: NameExpression) {
    for (let built of this.built) if (built.matches(expr)) return built.term
    let name = expr.id.name
    let rule = this.rules.find(r => r.id.name == name)
    if (!rule) return null
    let {name: nodeName, props, dialect, exported} =
      this.b.nodeInfo(rule.props, "d", name, expr.args, rule.params.length != expr.args.length ? none : rule.params)
    let term = this.b.makeTerminal(expr.toString(), nodeName, props)
    if (dialect != null) (this.byDialect[dialect] || (this.byDialect[dialect] = [])).push(term)

    if ((term.nodeType || exported) && rule.params.length == 0) {
      if (!term.nodeType) term.preserve = true
      this.b.namedTerms[exported || name] = term
    }
    this.buildRule(rule, expr, this.startState, new State([term]))
    this.built.push(new BuiltRule(name, expr.args, term))
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
      let name = expr.id.name, arg = args.find(a => a.name == name)
      if (arg) return this.build(arg.expr, from, to, arg.scope)
      let rule
      for (let i = 0, lt = this.b.localTokens; i <= lt.length; i++) {
        let set = i == lt.length ? this.b.tokens : lt[i]
        rule = set.rules.find(r => r.id.name == name)
        if (rule) break
      }
      if (!rule) return this.b.raise(`Reference to token rule '${name}', which isn't found`, expr.start)
      this.buildRule(rule, expr, from, to, args)
    } else if (expr instanceof CharClass) {
      for (let [a, b] of CharClasses[expr.type]) from.edge(a, b, to)
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
      let mid = new State
      from.edge(0, 0xDC00, to)
      from.edge(0xDC00, MAX_CHAR + 1, to)
      from.edge(0xD800, 0xDC00, mid)
      mid.edge(0xDC00, 0xE000, to)
    } else {
      return this.b.raise(`Unrecognized expression type in token`, (expr as any).start)
    }
  }

  takePrecedences() {
    let rel: {term: Term, after: Term[]}[] = this.precedenceRelations = []
    if (this.ast) for (let group of this.ast.precedences) {
      let prev: Term[] = []
      for (let item of group.items) {
        let level = []
        if (item instanceof NameExpression) {
          for (let built of this.built)
            if (item.args.length ? built.matches(item) : built.id == item.id.name)
              level.push(built.term)
        } else {
          let id = JSON.stringify(item.value), found = this.built.find(b => b.id == id)
          if (found) level.push(found.term)
        }
        if (!level.length) this.b.warn(`Precedence specified for unknown token ${item}`, item.start)
        for (let term of level) addRel(rel, term, prev)
        prev = prev.concat(level)
      }
    }
  }

  precededBy(a: Term, b: Term) {
    let found = this.precedenceRelations.find(r => r.term == a)
    return found && found.after.includes(b)
  }

  buildPrecTable(softConflicts: readonly Conflict[]) {
    let precTable: number[] = [], rel = this.precedenceRelations.slice()
    // Add entries for soft-conflicting tokens that are in the
    // precedence table, to make sure they'll appear in the right
    // order and don't mess up the longer-wins default rule.
    for (let {a, b, soft} of softConflicts) if (soft) {
      if (!rel.some(r => r.term == a) || !rel.some(r => r.term == b)) continue
      if (soft < 0) [a, b] = [b, a] // Now a is longer than b (and should thus take precedence)
      addRel(rel, b, [a])
      addRel(rel, a, [])
    }
    add: while (rel.length) {
      for (let i = 0; i < rel.length; i++) {
        let record = rel[i]
        if (record.after.every(t => precTable.includes(t.id))) {
          precTable.push(record.term.id)
          if (rel.length == 1) break add
          rel[i] = rel.pop()!
          continue add
        }
      }
      this.b.raise(`Cyclic token precedence relation between ${rel.map(r => r.term).join(", ")}`)
    }

    return precTable
  }
}

class MainTokenSet extends TokenSet {
  explicitConflicts: {a: Term, b: Term}[] = []
  ast!: TokenDeclaration | null

  getLiteral(expr: LiteralExpression) {
    let id = JSON.stringify(expr.value)
    for (let built of this.built) if (built.id == id) return built.term
    let name = null, props = {}, dialect = null, exported = null
    let decl = this.ast ? this.ast.literals.find(l => l.literal == expr.value) : null
    if (decl) ({name, props, dialect, exported} = this.b.nodeInfo(decl.props, "da", expr.value))

    let term = this.b.makeTerminal(id, name, props)
    if (dialect != null) (this.byDialect[dialect] || (this.byDialect[dialect] = [])).push(term)
    if (exported) this.b.namedTerms[exported] = term
    this.build(expr, this.startState, new State([term]), none)
    this.built.push(new BuiltRule(id, none, term))
    return term
  }

  takeConflicts() {
    let resolve = (expr: NameExpression | LiteralExpression) => {
      if (expr instanceof NameExpression) {
        for (let built of this.built) if (built.matches(expr)) return built.term
      } else {
        let id = JSON.stringify(expr.value), found = this.built.find(b => b.id == id)
        if (found) return found.term
      }
      this.b.warn(`Precedence specified for unknown token ${expr}`, expr.start)
      return null
    }
    for (let c of this.ast?.conflicts || []) {
      let a = resolve(c.a), b = resolve(c.b)
      if (a && b) {
        if (a.id < b.id) [a, b] = [b, a]
        this.explicitConflicts.push({a, b})
      }
    }
  }

  // Token groups are a mechanism for allowing conflicting (matching
  // overlapping input, without an explicit precedence being given)
  // tokens to exist in a grammar _if_ they don't occur in the same
  // place (aren't used in the same states).
  //
  // States that use tokens that conflict will raise an error when any
  // of the conflicting pairs of tokens both occur in that state.
  // Otherwise, they are assigned a token group, which includes all
  // the potentially-conflicting tokens they use. If there's already a
  // group that doesn't have any conflicts with those tokens, that is
  // reused, otherwise a new group is created.
  //
  // So each state has zero or one token groups, and each conflicting
  // token may belong to one or more groups. Tokens get assigned a
  // 16-bit bitmask with the groups they belong to set to 1 (all-1s
  // for non-conflicting tokens). When tokenizing, that mask is
  // compared to the current state's group (again using all-1s for
  // group-less states) to determine whether a token is applicable for
  // this state.
  //
  // Extended/specialized tokens are treated as their parent token for
  // this purpose.
  buildTokenGroups(states: readonly LRState[], skipInfo: readonly SkipInfo[], startID: number) {
    let tokens = this.startState.compile()
    if (tokens.accepting.length)
      this.b.raise(`Grammar contains zero-length tokens (in '${tokens.accepting[0].name}')`,
                   this.rules.find(r => r.id.name == tokens.accepting[0].name)!.start)
    if (/\btokens\b/.test(verbose)) console.log(tokens.toString())

    // If there is a precedence specified for the pair, the conflict is resolved
    let allConflicts = tokens.findConflicts(checkTogether(states, this.b, skipInfo))
      .filter(({a, b}) => !this.precededBy(a, b) && !this.precededBy(b, a))
    for (let {a, b} of this.explicitConflicts) {
      if (!allConflicts.some(c => c.a == a && c.b == b))
        allConflicts.push(new Conflict(a, b, 0, "", ""))
    }
    let softConflicts = allConflicts.filter(c => c.soft), conflicts = allConflicts.filter(c => !c.soft)
    let errors: {conflict: Conflict, error: string}[] = []

    let groups: TokenGroup[] = []
    for (let state of states) {
      if (state.defaultReduce || state.tokenGroup > -1) continue
      // Find potentially-conflicting terms (in terms) and the things
      // they conflict with (in conflicts), and raise an error if
      // there's a token conflict directly in this state.
      let terms: Term[] = [], incompatible: Term[] = []
      let skip = skipInfo[this.b.skipRules.indexOf(state.skip)].startTokens
      for (let term of skip)
        if (state.actions.some(a => a.term == term))
          this.b.raise(`Use of token ${term.name} conflicts with skip rule`)

      let stateTerms: Term[] = []
      for (let i = 0; i < state.actions.length + (skip ? skip.length : 0); i++) {
        let term = i < state.actions.length ? state.actions[i].term : skip[i - state.actions.length]
        let orig = this.b.tokenOrigins[term.name]
        if (orig && orig.spec) term = orig.spec
        else if (orig && orig.external) continue
        addToSet(stateTerms, term)
      }
      if (stateTerms.length == 0) continue

      for (let term of stateTerms) {
        for (let conflict of conflicts) {
          let conflicting = conflict.a == term ? conflict.b : conflict.b == term ? conflict.a : null
          if (!conflicting) continue
          if (stateTerms.includes(conflicting) && !errors.some(e => e.conflict == conflict)) {
            let example = conflict.exampleA ? ` (example: ${JSON.stringify(conflict.exampleA)}${
              conflict.exampleB ? ` vs ${JSON.stringify(conflict.exampleB)}` : ""})` : ""
            errors.push({
              error: `Overlapping tokens ${term.name} and ${conflicting.name} used in same context${example}\n` +
                `After: ${state.set[0].trail()}`,
              conflict
            })
          }
          addToSet(terms, term)
          addToSet(incompatible, conflicting)
        }
      }

      let tokenGroup = null
      for (let group of groups) {
        if (incompatible.some(term => group.tokens.includes(term))) continue
        for (let term of terms) addToSet(group.tokens, term)
        tokenGroup = group
        break
      }
      if (!tokenGroup) {
        tokenGroup = new TokenGroup(terms, groups.length + startID)
        groups.push(tokenGroup)
      }
      state.tokenGroup = tokenGroup.groupID
    }

    if (errors.length)
      this.b.raise(errors.map(e => e.error).join("\n\n"))
    if (groups.length + startID > 16)
      this.b.raise(`Too many different token groups (${groups.length}) to represent them as a 16-bit bitfield`)

    let precTable = this.buildPrecTable(softConflicts)

    return {
      tokenGroups: groups,
      tokenPrec: precTable,
      tokenData: tokens.toArray(buildTokenMasks(groups), precTable)
    }
  }
}

class LocalTokenSet extends TokenSet {
  fallback: Term | null = null
  ast!: LocalTokenDeclaration

  constructor(b: Builder, ast: LocalTokenDeclaration) {
    super(b, ast)
    if (ast.fallback) b.unique(ast.fallback.id)
  }

  getToken(expr: NameExpression) {
    let term = null
    if (this.ast.fallback && this.ast.fallback.id.name == expr.id.name) {
      if (expr.args.length) this.b.raise(`Incorrect number of arguments for ${expr.id.name}`, expr.start)
      if (!this.fallback) {
        let {name: nodeName, props, exported} =
          this.b.nodeInfo(this.ast.fallback.props, "", expr.id.name, none, none)
        let term = this.fallback = this.b.makeTerminal(expr.id.name, nodeName, props)
        if (term.nodeType || exported) {
          if (!term.nodeType) term.preserve = true
          this.b.namedTerms[exported || expr.id.name] = term
        }
        this.b.used(expr.id.name)
      }
      term = this.fallback
    } else {
      term = super.getToken(expr)
    }
    if (term && !this.b.tokenOrigins[term.name])
      this.b.tokenOrigins[term.name] = {group: this}
    return term
  }

  buildLocalGroup(states: readonly LRState[], skipInfo: readonly SkipInfo[], id: number): TokenizerSpec {
    let tokens = this.startState.compile()
    if (tokens.accepting.length)
      this.b.raise(`Grammar contains zero-length tokens (in '${tokens.accepting[0].name}')`,
                   this.rules.find(r => r.id.name == tokens.accepting[0].name)!.start)

    for (let {a, b, exampleA} of tokens.findConflicts(() => true)) {
      if (!this.precededBy(a, b) && !this.precededBy(b, a))
        this.b.raise(`Overlapping tokens ${a.name} and ${b.name} in local token group${
          exampleA ? ` (example: ${JSON.stringify(exampleA)})` : ''}`)
    }

    for (let state of states) {
      if (state.defaultReduce) continue
      // See if this state uses any of the tokens in this group, and
      // if so, make sure it *only* uses tokens from this group.
      let usesThis: Term | null = null
      let usesOther: Term | undefined = skipInfo[this.b.skipRules.indexOf(state.skip)].startTokens[0]
      for (let {term} of state.actions) {
        let orig = this.b.tokenOrigins[term.name]
        if (orig?.group == this) usesThis = term
        else usesOther = term
      }
      if (usesThis) {
        if (usesOther)
          this.b.raise(`Tokens from a local token group used together with other tokens (${
            usesThis.name} with ${usesOther.name})`)
        state.tokenGroup = id
      }
    }

    let precTable = this.buildPrecTable(none)
    let tokenData = tokens.toArray({[id]: Seq.End}, precTable)
    let precOffset = tokenData.length
    let fullData = new Uint16Array(tokenData.length + precTable.length + 1)
    fullData.set(tokenData, 0)
    fullData.set(precTable, precOffset)
    fullData[fullData.length - 1] = Seq.End

    return {
      groupID: id,
      create: () => new LocalTokenGroup(fullData, precOffset, this.fallback ? this.fallback.id : undefined),
      createSource: importName =>
        `new ${importName("LocalTokenGroup", "@lezer/lr")}(${encodeArray(fullData)}, ${precOffset}${
          this.fallback ? `, ${this.fallback.id}` : ''})`
    }
  }
}

function checkTogether(states: readonly LRState[], b: Builder, skipInfo: readonly SkipInfo[]) {
  let cache: {[id: number]: boolean} = Object.create(null)
  function hasTerm(state: LRState, term: Term) {
    return state.actions.some(a => a.term == term) ||
      skipInfo[b.skipRules.indexOf(state.skip)].startTokens.includes(term)
  }
  return (a: Term, b: Term) => {
    if (a.id < b.id) [a, b] = [b, a]
    let key = a.id | (b.id << 16), cached = cache[key]
    if (cached != null) return cached
    return cache[key] = states.some(state => hasTerm(state, a) && hasTerm(state, b))
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
  if (low < ASTRAL) {
    if (low < GAP_START) from.edge(low, Math.min(hi, GAP_START), to)
    if (hi > GAP_END) from.edge(Math.max(low, GAP_END), Math.min(hi, MAX_CHAR + 1), to)
    low = ASTRAL
  }
  if (hi <= ASTRAL) return

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

function isEmpty(expr: Expression) {
  return expr instanceof SequenceExpression && expr.exprs.length == 0
}

function gatherExtTokens(b: Builder, tokens: readonly {id: Identifier, props: readonly Prop[]}[]) {
  let result: {[name: string]: Term} = Object.create(null)
  for (let token of tokens) {
    b.unique(token.id)
    let {name, props, dialect} = b.nodeInfo(token.props, "d", token.id.name)
    let term = b.makeTerminal(token.id.name, name, props)
    if (dialect != null) (b.tokens.byDialect[dialect] || (b.tokens.byDialect[dialect] = [])).push(term)
    b.namedTerms[token.id.name] = result[token.id.name] = term
  }
  return result
}

function findExtToken(b: Builder, tokens: {[name: string]: Term}, expr: NameExpression) {
  let found = tokens[expr.id.name]
  if (!found) return null
  if (expr.args.length) b.raise("External tokens cannot take arguments", expr.args[0].start)
  b.used(expr.id.name)
  return found
}

function addRel(rel: {term: Term, after: readonly Term[]}[], term: Term, after: readonly Term[]) {
  let found = rel.findIndex(r => r.term == term)
  if (found < 0) rel.push({term, after})
  else rel[found] = {term, after: rel[found].after.concat(after)}
}

class ExternalTokenSet implements TokenizerSpec {
  tokens: {[name: string]: Term}

  constructor(readonly b: Builder, readonly ast: ExternalTokenDeclaration) {
    this.tokens = gatherExtTokens(b, ast.tokens)
    for (let name in this.tokens)
      this.b.tokenOrigins[this.tokens[name].name] = {external: this}
  }

  getToken(expr: NameExpression) { return findExtToken(this.b, this.tokens, expr) }

  create() {
    return this.b.options.externalTokenizer!(this.ast.id.name, this.b.termTable)
  }

  createSource(importName: (name: string, source: string, prefix?: string) => string) {
    let {source, id: {name}} = this.ast
    return importName(name, source)
  }
}

class ExternalSpecializer {
  term: Term | null = null
  tokens: {[name: string]: Term}

  constructor(readonly b: Builder, readonly ast: ExternalSpecializeDeclaration) {
    this.tokens = gatherExtTokens(b, ast.tokens)
  }

  finish() {
    let terms = this.b.normalizeExpr(this.ast.token)
    if (terms.length != 1 || terms[0].terms.length != 1 || !terms[0].terms[0].terminal)
      this.b.raise(`The token expression to '@external ${this.ast.type}' must resolve to a token`, this.ast.token.start)
    this.term = terms[0].terms[0]
    for (let name in this.tokens)
      this.b.tokenOrigins[this.tokens[name].name] = {spec: this.term, external: this}
  }

  getToken(expr: NameExpression) { return findExtToken(this.b, this.tokens, expr) }
}

function inlineRules(rules: readonly Rule[], preserve: readonly Term[]): readonly Rule[] {
  for (let pass = 0;; pass++) {
    let inlinable: {[name: string]: readonly Rule[]} = Object.create(null), found
    if (pass == 0) for (let rule of rules) {
      if (rule.name.inline && !inlinable[rule.name.name]) {
        let group = rules.filter(r => r.name == rule.name)
        if (group.some(r => r.parts.includes(rule.name))) continue
        found = inlinable[rule.name.name] = group
      }
    }
    for (let i = 0; i < rules.length; i++) {
      let rule = rules[i]
      if (!rule.name.interesting && !rule.parts.includes(rule.name) && rule.parts.length < 3 &&
          !preserve.includes(rule.name) &&
          (rule.parts.length == 1 || rules.every(other => other.skip == rule.skip || !other.parts.includes(rule.name))) &&
          !rule.parts.some(p => !!inlinable[p.name]) &&
          !rules.some((r, j) => j != i && r.name == rule.name))
        found = inlinable[rule.name.name] = [rule]
    }
    if (!found) return rules
    let newRules = []
    for (let rule of rules) {
      if (inlinable[rule.name.name]) continue
      if (!rule.parts.some(p => !!inlinable[p.name])) {
        newRules.push(rule)
        continue
      }
      function expand(at: number, conflicts: readonly Conflicts[], parts: readonly Term[]) {
        if (at == rule.parts.length) {
          newRules.push(new Rule(rule.name, parts, conflicts, rule.skip))
          return
        }
        let next = rule.parts[at], replace = inlinable[next.name]
        if (!replace) {
          expand(at + 1, conflicts.concat(rule.conflicts[at + 1]), parts.concat(next))
          return
        }
        for (let r of replace)
          expand(at + 1,
                 conflicts.slice(0, conflicts.length - 1)
                 .concat(conflicts[at].join(r.conflicts[0]))
                 .concat(r.conflicts.slice(1, r.conflicts.length - 1))
                 .concat(rule.conflicts[at + 1].join(r.conflicts[r.conflicts.length - 1])),
                 parts.concat(r.parts))
      }
      expand(0, [rule.conflicts[0]], [])
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
/// tokenizers, you'll have to provide the `externalTokenizer` option
/// for the returned parser to be able to parse anything.
export function buildParser(text: string, options: BuildOptions = {}): LRParser {
  let builder = new Builder(text, options), parser = builder.getParser()
  ;(parser as any).termTable = builder.termTable
  return parser
}

const KEYWORDS = ["arguments", "await", "break", "case", "catch", "continue", "debugger", "default", "do", "else",
                  "eval", "finally", "for", "function", "if", "return", "switch", "throw", "try", "var", "while",
                  "with", "null", "true", "false", "instanceof", "typeof", "void", "delete", "new", "in", "this",
                  "const", "class", "extends", "export", "import", "super", "enum", "implements", "interface",
                  "let", "package", "private", "protected", "public", "static", "yield", "require"]

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
  return new Builder(text, options).getParserFile()
}

function ignored(name: string) {
  let first = name[0]
  return first == "_" || first.toUpperCase() != first
}

function isExported(rule: RuleDeclaration) {
  return rule.props.some(p => p.at && p.name == "export")
}
