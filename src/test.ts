import {Tree, NodeType, NodeProp, Parser} from "@lezer/common"

const none: readonly any[] = []

class TestSpec {
  constructor(readonly name: string,
              readonly props: {prop: NodeProp<any>, value: any}[],
              readonly children: readonly TestSpec[] = none,
              readonly wildcard = false) {}

  static parse(spec: string): readonly TestSpec[] {
    let pos = 0, tok = "sof", value = ""
    function err(): never {
      throw new SyntaxError("Invalid test spec: " + spec)
    }
    function next() {
      while (pos < spec.length && /\s/.test(spec.charAt(pos))) pos++
      if (pos == spec.length) return tok = "eof"
      let next = spec.charAt(pos++)
      if (next == "(" && spec.slice(pos, pos + 4) == "...)") {
        pos += 4
        return tok = "..."
      }
      if (/[\[\](),=]/.test(next)) return tok = next
      if (/[^()\[\],="\s]/.test(next)) {
        let name = /[^()\[\],="\s]*/.exec(spec.slice(pos - 1))
        value = name![0]
        pos += name![0].length - 1
        return tok = "name"
      }
      if (next == '"') {
        let content = /^"((?:[^\\"]|\\.)*)"/.exec(spec.slice(pos - 1)) || err()
        value = JSON.parse(content[0])
        pos += content[0].length - 1
        return tok = "name"
      }
      return err()
    }
    next()
    function parseSeq() {
      let seq = []
      while (tok != "eof" && tok != ")") {
        seq.push(parse())
        if (tok == ",") next()
      }
      return seq
    }
    function parse() {
      let name = value, children = none, props = [], wildcard = false
      if (tok != "name") err()
      next()
      if (tok as any == "[") {
        next()
        while (tok as any != "]") {
          if (tok as any != "name") err()
          let prop = (NodeProp as any)[value] as NodeProp<any>, val = ""
          if (!(prop instanceof NodeProp)) err()
          next()
          if (tok as any == "=") {
            next()
            if (tok as any != "name") err()
            val = value
            next()
          }
          props.push({prop, value: prop.deserialize(val)})
        }
        next()
      }
      if (tok as any == "(") {
        next()
        children = parseSeq()
        // @ts-ignore TypeScript doesn't understand that `next` may have mutated `tok` (#9998)
        if (tok != ")") err()
        next()
      } else if (tok as any == "...") {
        wildcard = true
        next()
      }
      return new TestSpec(name, props, children, wildcard)
    }
    let result = parseSeq()
    if (tok != "eof") err()
    return result
  }

  matches(type: NodeType) {
    if (type.name != this.name) return false
    for (let {prop, value} of this.props)
      if ((value || type.prop(prop)) && JSON.stringify(type.prop(prop)) != JSON.stringify(value)) return false
    return true
  }
}

function defaultIgnore(type: NodeType) { return /\W/.test(type.name) }

export function testTree(tree: Tree, expect: string, mayIgnore = defaultIgnore) {
  let specs = TestSpec.parse(expect)
  let stack = [specs], pos = [0]
  tree.iterate({
    enter(n) {
      if (!n.name) return
      let last = stack.length - 1, index = pos[last], seq = stack[last]
      let next = index < seq.length ? seq[index] : null
      if (next && next.matches(n.type)) {
        if (next.wildcard) {
          pos[last]++
          return false
        }
        pos.push(0)
        stack.push(next.children)
        return undefined
      } else if (mayIgnore(n.type)) {
        return false
      } else {
        let parent = last > 0 ? stack[last - 1][pos[last - 1]].name : "tree"
        let after = next ? next.name + (parent == "tree" ? "" : " in " + parent) : `end of ${parent}`
        throw new Error(`Expected ${after}, got ${n.name} at ${n.to} \n${tree}`)
      }
    },
    leave(n) {
      if (!n.name) return
      let last = stack.length - 1, index = pos[last], seq = stack[last]
      if (index < seq.length) throw new Error(`Unexpected end of ${n.name}. Expected ${seq.slice(index).map(s => s.name).join(", ")} at ${n.from}\n${tree}`)
      pos.pop()
      stack.pop()
      pos[last - 1]++
    }
  })
  if (pos[0] != specs.length)
    throw new Error(`Unexpected end of tree. Expected ${stack[0].slice(pos[0]).map(s => s.name).join(", ")} at ${tree.length}\n${tree}`)
}

function toLineContext(file: string, index: number) {
  const endEol = file.indexOf('\n', index + 80);

  const endIndex = endEol === -1 ? file.length : endEol;

  return file.substring(index, endIndex).split(/\n/).map(str => '  | ' + str).join('\n');
}

export function fileTests(file: string, fileName: string, mayIgnore = defaultIgnore) {
  let caseExpr = /\s*#[ \t]*(.*)(?:\r\n|\r|\n)([^]*?)==+>([^]*?)(?:$|(?:\r\n|\r|\n)+(?=#))/gy
  let tests: {
    name: string,
    text: string,
    expected: string,
    configStr: string,
    config: object,
    strict: boolean,
    run(parser: Parser): void
  }[] = []
  let lastIndex = 0;
  for (;;) {
    let m = caseExpr.exec(file)
    if (!m) throw new Error(`Unexpected file format in ${fileName} around\n\n${toLineContext(file, lastIndex)}`)

    let text = m[2].trim(), expected = m[3].trim()
    let [, name, configStr] = /(.*?)(\{.*?\})?$/.exec(m[1])!
    let config = configStr ? JSON.parse(configStr) : null
    let strict = !/⚠|\.\.\./.test(expected)

    tests.push({
      name,
      text,
      expected,
      configStr,
      config,
      strict,
      run(parser: Parser) {
        if ((parser as any).configure && (strict || config))
          parser = (parser as any).configure({strict, ...config})
        testTree(parser.parse(text), expected, mayIgnore)
      }
    })
    lastIndex = m.index + m[0].length
    if (lastIndex == file.length) break
  }
  return tests
}
