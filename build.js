import {build, watch} from "@marijn/buildtool"
import {fileURLToPath} from "url"
import {dirname, join} from "path"
import {mkdirSync} from "fs"
import * as tsup from "tsup"

let tsOptions = {
  lib: ["dom", "es2016"],
  types: ["mocha", "node"],
  target: "es6"
}

let base = dirname(fileURLToPath(import.meta.url)), src = join(base, "src"), dist = join(base, "dist")
let main = join(src, "index.ts"), mainConf = {tsOptions}
let test = join(src, "test.ts"), testConf = {tsOptions, bundleName: "test"}

let unpluginFile = join(src, "unplugin-lezer.ts")
try { mkdirSync(dist) } catch {}

tsup.build({
  entry: { unplugin: unpluginFile },
  dts: true,
  format: ["cjs", "esm"],
  outdir: dist,
  skipNodeModulesBundle: true,
})

if (process.argv.includes("--watch")) {
  watch([main], [], mainConf)
  watch([test], [], testConf)
} else {
  build(main, mainConf)
  build(test, testConf)
}
