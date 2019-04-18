import typescript from "rollup-plugin-typescript2"
import nodeResolve from "rollup-plugin-node-resolve"
import commonjs from "rollup-plugin-commonjs"

export default {
  input: "bin/lezer.ts",
  output: {
    file: "dist/lezer.js",
    format: "cjs",
    paths: {"..": "./index.js"}
  },
  external: ["fs", ".."],
  plugins: [
    nodeResolve(),
    typescript({
      check: false,
      tsconfigOverride: {
        compilerOptions: {lib: ["es2018"], sourceMap: true, target: "es2018", strict: false},
        include: null
      }
    }),
    commonjs()
  ]
}
