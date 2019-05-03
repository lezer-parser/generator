import {Tokenizer} from "lezer"
import {insertSemi, noInsertSemi, postfixOp} from "./javascript.terms"

const brace = "}".charCodeAt(0), newline = /[\n\u2028\u2029]/

export const insertSemicolon = new Tokenizer((input, stack) => {
  let start = input.pos, next = input.next()
  if (next == brace || next == -1 || newline.test(input.read(stack.pos, input.pos - 1)))
    input.accept(insertSemi, start)
})

export const noInsertSemicolon = new Tokenizer((input, stack) => {
  let start = input.pos, next = input.next()
  if (next != brace && next != -1 && !newline.test(input.read(stack.pos, input.pos - 1)))
    input.accept(terms.noInsertSemi, start)
})

const plus = "+".charCodeAt(0), minus = "-".charCodeAt(0)

// Make sure postfix ops don't happen after a line break
export const postfix = new Tokenizer((input, stack) => {
  let next = input.next()
  if ((next == plus || next == minus) && next == input.next() &&
      !newline.test(input.read(stack.pos, input.pos - 2)))
    input.accept(terms.postfixOp)
})
