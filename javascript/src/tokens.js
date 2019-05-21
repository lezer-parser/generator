/* Hand-written tokenizers for JavaScript tokens that can't be
   expressed by lezer's built-in tokenizer. */

import {ExternalTokenizer} from "lezer"
import {insertSemi, noSemi, postfixOp, templateContent, templateDollarBrace, templateEnd} from "./parser.terms.js"

const newline = [10, 13, 8232, 8233]
const space = [9, 11, 12, 32, 133, 160, 5760, 8192, 8193, 8194, 8195, 8196, 8197, 8198, 8199, 8200, 8201, 8202, 8239, 8287, 12288]

const braceR = 125, braceL = 123, semicolon = 59, slash = 47, star = 42,
      plus = 43, minus = 45, dollar = 36, backtick = 96, backslash = 92

// FIXME this should technically enter block comments
function newlineBefore(input, pos) {
  for (let i = pos - 1; i >= 0; i--) {
    let prev = input.peek(i)
    if (newline.includes(prev)) return true
    if (!space.includes(prev)) break
  }
  return false
}

export const insertSemicolon = new ExternalTokenizer((input, stack) => {
  let start = input.pos, next = input.next()
  if ((next == braceR || next == -1 || newlineBefore(input, start)) && stack.canShift(insertSemi))
    input.accept(insertSemi, start)
})

export const noSemicolon = new ExternalTokenizer((input, stack) => {
  let start = input.pos, next = input.next()
  if (space.includes(next) || newline.includes(next)) return
  if (next == slash) {
    let after = input.next()
    if (after == slash || after == star) return
  }
  if (next != braceR && next != semicolon && next != -1 && !newlineBefore(input, start) &&
      stack.canShift(noSemi))
    input.accept(noSemi, start)
})

export const postfix = new ExternalTokenizer((input, stack) => {
  let next = input.next()
  if ((next == plus || next == minus) && next == input.next() &&
      !newlineBefore(input, input.pos - 2) && stack.canShift(postfixOp))
    input.accept(postfixOp)
})

export const template = new ExternalTokenizer(input => {
  let start = input.pos, afterDollar = false
  for (;;) {
    let next = input.next()
    if (next < 0) {
      if (input.pos > start) input.accept(templateContent, input.pos)
      break
    } else if (next == backtick) {
      if (input.pos == start + 1) input.accept(templateEnd)
      else input.accept(templateContent, input.pos - 1)
      break
    } else if (next == braceL && afterDollar) {
      if (input.pos == start + 2) input.accept(templateDollarBrace)
      else input.accept(templateContent, input.pos - 2)
      break
    } else if (next == backslash) {
      input.next()
    }
    afterDollar = next == dollar
  }
})
