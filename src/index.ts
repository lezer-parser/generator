import {Grammar} from "./grammar/grammar"
import {buildAutomaton} from "./grammar/automaton"
import {parse, Node} from "./parse"

import {normalizeGrammar} from "./grammar/normalize"
import {parseGrammar} from "./grammar/parse"

const g = new Grammar([
  "S -> A #",
  "A -> A + M",
  "A -> A - M",
  "A -> M",
  "M -> M * V",
  "M -> M / V",
  "M -> V",
  "V -> x",
  "V -> y",
  "V -> ( A )"
])

let table = buildAutomaton(g)
console.log(table.join("\n"))

let input = ["x", "*", "y", "+", "(", "y", "/", "x", ")"]
let ast = parse(input, g, table)

console.log("--------------")

let cache = new Node(null, 0, [], [])
ast.partial(0, 2, 0, cache)
ast.partial(4, input.length, 0, cache)
let newInput = input.slice()
newInput[3] = "*"
parse(newInput, g, table, cache)

console.log(normalizeGrammar(parseGrammar(`
S { P (("+" | "-") P)* }
P { T (("*" | "/") T)* }
T { "x" | "y" | "(" S ")" }
`)) + "")
