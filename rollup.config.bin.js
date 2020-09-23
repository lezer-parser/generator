import typescript from "rollup-plugin-typescript2"
import {nodeResolve} from "@rollup/plugin-node-resolve"
import commonjs from "@rollup/plugin-commonjs"

export default {
  input: "src/lezer-generator.ts",
  output: {
    banner: "#!/usr/bin/env node\n",
    file: "dist/lezer-generator.cjs",
    format: "cjs",
    paths: {"..": "./index.cjs"}
  },
  external: ["fs", ".."],
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
          declaration: false
        },
        include: ["bin/*.ts", "src/*.ts"]
      }
    }),
    commonjs()
  ]
}
