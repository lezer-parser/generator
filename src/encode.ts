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

const BIG_VAL = 0xffff, BIG_VAL_CODE = 126

const START = 32, GAP1 = 34 /* '"' */, GAP2 = 92 /* "\\" */

const BASE = 46 // (126 - 32 - 2) / 2

function digitToChar(digit: number) {
  let ch = digit + START
  if (ch >= GAP1) ch++
  if (ch >= GAP2) ch++
  return String.fromCharCode(ch)
}

export function encode(value: number, max = 0xffff) {
  if (value > max) throw new Error("Trying to encode a number that's too big: " + value)
  if (value == BIG_VAL) return String.fromCharCode(BIG_VAL_CODE)
  let result = ""
  for (let first = BASE;; first = 0) {
    let low = value % BASE, rest = value - low
    result = digitToChar(low + first) + result
    if (rest == 0) break
    value = rest / BASE
  }
  return result
}

export function encodeArray(values: {length: number, readonly [i: number]: number}, max = 0xffff) {
  let result = '"' + encode(values.length)
  for (let i = 0; i < values.length; i++) result += encode(values[i], max)
  result += '"'
  return result
}

export function decode<T extends {[i: number]: number} = Uint16Array>(input: string, Type: {new (n: number): T} = Uint16Array as any): T {
  let array: T | null = null
  for (let pos = 0, out = 0; pos < input.length;) {
    let value = 0
    for (;;) {
      let next = input.charCodeAt(pos++), stop = false
      if (pos > input.length) throw new Error("AH")
      if (next == BIG_VAL_CODE) { value = BIG_VAL; break }
      if (next >= GAP2) next--
      if (next >= GAP1) next--
      let digit = next - START
      if (digit >= BASE) { digit -= BASE; stop = true }
      value += digit
      if (stop) break
      value *= BASE
    }
    if (array) array[out++] = value
    else array = new Type(value)
  }
  return array!
}
