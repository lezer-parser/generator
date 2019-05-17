import {buildParserFile} from ".."

let file = undefined, moduleStyle = "CommonJS", includeNames = false

for (let i = 2; i < process.argv.length; i++) {
  let arg = process.argv[i]
  if (!/^-/.test(arg)) {
    if (file) error("Multiple input files given")
    file = arg
  } else if (arg == "--help") {
    console.log("Usage: build-parser [--es6] [--names] file")
    process.exit(0)
  } else if (arg == "--es6") {
    moduleStyle = "es6"
  } else if (arg == "--names") {
    includeNames = true
  } else {
    error("Unrecognized option " + arg)
  }
}

if (!file) error("No input file given")

function error(msg: string) {
  console.error(msg)
  process.exit(1)
}

try {
  console.log(buildParserFile(require("fs").readFileSync(file, "utf8"), {fileName: file, moduleStyle, includeNames}))
} catch (e) {
  console.error(e instanceof SyntaxError ? e.message : e.stack)
  process.exit(1)
}
