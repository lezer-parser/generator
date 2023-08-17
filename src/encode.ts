// Encode numbers as groups of printable ascii characters
//
// - 0xffff, which is often used as placeholder, is encoded as "~"
//
// - The characters from " " (32) to "}" (125), excluding '"' and
//   "\\", indicate values from 0 to 92
//
// - The first bit in a 'digit' is used to indicate whether this is
//   the end of a number.
//
// - That leaves 46 other values, which are actually significant.
//
// - The digits in a number are ordered from high to low significance.

import {Encode} from "@lezer/lr/dist/constants"

function digitToChar(digit: number) {
  let ch = digit + Encode.Start
  if (ch >= Encode.Gap1) ch++
  if (ch >= Encode.Gap2) ch++
  return String.fromCharCode(ch)
}

export function encode(value: number, max = 0xffff) {
  if (value > max) throw new Error("Trying to encode a number that's too big: " + value)
  if (value == Encode.BigVal) return String.fromCharCode(Encode.BigValCode)
  let result = ""
  for (let first = Encode.Base;; first = 0 as Encode) {
    let low = value % Encode.Base, rest = value - low
    result = digitToChar(low + first) + result
    if (rest == 0) break
    value = rest / Encode.Base
  }
  return result
}

export function encodeArray(values: {length: number, readonly [i: number]: number}, max = 0xffff) {
  let result = '"' + encode(values.length, 0xffffffff)
  for (let i = 0; i < values.length; i++) result += encode(values[i], max)
  result += '"'
  return result
}
