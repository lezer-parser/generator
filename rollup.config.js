import typescript from "rollup-plugin-typescript2"
import {nodeResolve} from "@rollup/plugin-node-resolve"
import commonjs from "@rollup/plugin-commonjs"

export default [{
  input: "./src/index.ts",
  output: [{
    format: "cjs",
    file: "./dist/index.cjs"
  }, {
    format: "es",
    file: "./dist/index.js",
    externalLiveBindings: false
  }],
  external(id) { return !/^[\.\/]/.test(id) },
  plugins: [
    nodeResolve(),
    typescript({
      check: false,
      tsconfigOverride: {
        compilerOptions: {
          lib: ["es2018"],
          target: "es2018",
          strict: false,
          declaration: true
        },
        include: ["src/*.ts"]
      }
    }),
    commonjs()
  ]
}, {
  input: "./src/rollup-plugin-lezer.js",
  output: [{
    format: "cjs",
    file: "./dist/rollup-plugin-lezer.cjs"
  }, {
    format: "es",
    file: "./dist/rollup-plugin-lezer.js"
  }],
  external(id) { return true }
}]
