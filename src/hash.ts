export function hash(a: number, b: number): number { return (a << 5) + a + b }

export function hashString(h: number, s: string) {
  for (let i = 0; i < s.length; i++) h = hash(h, s.charCodeAt(i))
  return h
}
