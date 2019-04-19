import typescript from "rollup-plugin-typescript2"
import nodeResolve from "rollup-plugin-node-resolve"
import commonjs from "rollup-plugin-commonjs"

export default {
  input: "./src/index.ts",
  output: {
    format: "cjs",
    file: "./dist/index.js",
    sourcemap: true
  },
  external(id) { return !/^[\.\/]/.test(id) },
  plugins: [
    nodeResolve(),
    typescript({
      check: false,
      tsconfigOverride: {
        compilerOptions: {
          lib: ["es2018"],
          sourceMap: true,
          target: "es2018",
          strict: false,
          declaration: true
        },
        include: ["src/*.ts"]
      },
      include: ["src/*.ts"]
    }),
    commonjs()
  ]
}
