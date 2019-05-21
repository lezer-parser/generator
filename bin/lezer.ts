import {buildParserFile} from ".."

let file = undefined, out = undefined, moduleStyle = "CommonJS", includeNames = false

let {writeFileSync, readFileSync} = require("fs")

for (let i = 2; i < process.argv.length;) {
  let arg = process.argv[i++]
  if (!/^-/.test(arg)) {
    if (file) error("Multiple input files given")
    file = arg
  } else if (arg == "--help") {
    console.log("Usage: build-parser [--es6] [--names] file")
    process.exit(0)
  } else if (arg == "--es6") {
    moduleStyle = "es6"
  } else if (arg == "-o" || arg == "--output") {
    if (out) error("Multiple output files given")
    out = process.argv[i++]
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

let parser, terms
try {
  ;({parser, terms} = buildParserFile(readFileSync(file, "utf8"), {fileName: file, moduleStyle, includeNames}))
} catch (e) {
  console.error(e instanceof SyntaxError ? e.message : e.stack)
  process.exit(1)
}

if (out) {
  let ext = /^(.*)*?\.js$/.exec(out)
  let [parserFile, termFile] = ext ? [out, ext[1] + ".terms.js"] : [out + ".js", out + ".terms.js"]
  writeFileSync(parserFile, parser)
  writeFileSync(termFile, terms)
  console.log(`Wrote ${parserFile} and ${termFile}`)
} else {
  console.log(parser)
}
