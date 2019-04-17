import {buildParserFile} from "./build"

let file = null

for (let i = 2; i < process.argv.length; i++) {
  let arg = process.argv[i]
  if (!/^-/.test(arg)) {
    if (file) error("Multiple input files given")
    file = arg
  } else if (arg == "--help") {
    console.log("Usage: build-parser file")
    process.exit(0)
  } else {
    error("Unrecognized option " + arg)
  }
}

function error(msg: string) {
  console.error(msg)
  process.exit(1)
}

try {
  console.log(buildParserFile(require("fs").readFileSync(file, "utf8"), file))
} catch (e) {
  console.error(e instanceof SyntaxError ? e.message : e.stack)
  process.exit(1)
}
