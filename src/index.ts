import {Grammar} from "./grammar/grammar"
import {buildAutomaton} from "./grammar/automaton"
import {parse, Node} from "./parse"

const g = new Grammar(`
S { P (("+" | "-") P)* }
P { T (("*" | "/") T)* }
T { "x" | "y" | "(" S ")" }
`)

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
