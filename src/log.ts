export const verbose = (typeof process != "undefined" && process.env.LOG) || ""
export const timing = /\btime\b/.test(verbose)
export const time: <T>(label: string, f: () => T) => T = timing ? (label, f) => {
  let t0 = Date.now()
  let result = f()
  console.log(`${label} (${((Date.now() - t0) / 1000).toFixed(2)}s)`)
  return result
} : (_label, f) => f()
