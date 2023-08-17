import {build, watch} from "@marijn/buildtool"
import {fileURLToPath} from "url"
import {dirname, join} from "path"

let tsOptions = {
  lib: ["dom", "es2016"],
  types: ["mocha", "node"],
  target: "es6"
}

let src = join(dirname(fileURLToPath(import.meta.url)), "src")
let main = join(src, "index.ts"), mainConf = {tsOptions}
let test = join(src, "test.ts"), testConf = {tsOptions, bundleName: "test"}

if (process.argv.includes("--watch")) {
  watch([main], [], mainConf)
  watch([test], [], testConf)
} else {
  build(main, mainConf)
  build(test, testConf)
}
