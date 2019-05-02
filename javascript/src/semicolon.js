const brace = "}".charCodeAt(0), newline = /[\n\u2028\u2029]/

import {insertSemi} from "./javascript.terms"

export function insertSemicolon(input, stack) {
  let next = input.next()
  if (next == brace || next == -1 || newline.test(input.read(stack.pos, input.pos - 1)))
    input.accept(insertSemi, input.pos - 1)
}
