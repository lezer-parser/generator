import {build, watch} from "@marijn/buildtool"
import {fileURLToPath} from "url"
import {dirname, join} from "path"
import {mkdirSync} from "fs"
import * as esbuild from "esbuild"

let tsOptions = {
  lib: ["dom", "es2016"],
  types: ["mocha", "node"],
  target: "es6"
}

let base = dirname(fileURLToPath(import.meta.url)), src = join(base, "src"), dist = join(base, "dist")
let main = join(src, "index.ts"), mainConf = {tsOptions}
let test = join(src, "test.ts"), testConf = {tsOptions, bundleName: "test"}

let unpluginFile = join(src, "unplugin-lezer.js")
try { mkdirSync(dist) } catch {}

["cjs", "esm"].map(format =>
  esbuild.buildSync({
    entryPoints: [unpluginFile],
    write: true,
    bundle: true,
    platform: "node",
    format,
    outExtension: { '.js': format === "esm" ? ".js" : ".cjs" },
    outdir: dist,
  })
)

if (process.argv.includes("--watch")) {
  watch([main], [], mainConf)
  watch([test], [], testConf)
} else {
  build(main, mainConf)
  build(test, testConf)
}
