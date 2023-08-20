import {build, watch} from "@marijn/buildtool"
import {fileURLToPath} from "url"
import {dirname, join} from "path"
import {readFileSync, writeFileSync, mkdirSync} from "fs"
import {rollup} from "rollup"

let tsOptions = {
  lib: ["dom", "es2016"],
  types: ["mocha", "node"],
  target: "es6"
}

let base = dirname(fileURLToPath(import.meta.url)), src = join(base, "src"), dist = join(base, "dist")
let main = join(src, "index.ts"), mainConf = {tsOptions}
let test = join(src, "test.ts"), testConf = {tsOptions, bundleName: "test"}

let rollupFile = join(src, "rollup-plugin-lezer.js")
try { mkdirSync(dist) } catch {}

writeFileSync(join(dist, "rollup-plugin-lezer.js"), readFileSync(rollupFile, "utf8"))
rollup({
  input: rollupFile,
  external: () => true
}).then(bundle => bundle.generate({
  format: "cjs",
  file: join(dist, "rollup-plugin-lezer.cjs"),
  paths: id => id.endsWith("/index.js") ? "./index.cjs" : id
})).then(result => {
  writeFileSync(join(dist, "rollup-plugin-lezer.cjs"), result.output[0].code)
})

if (process.argv.includes("--watch")) {
  watch([main], [], mainConf)
  watch([test], [], testConf)
} else {
  build(main, mainConf)
  build(test, testConf)
}
