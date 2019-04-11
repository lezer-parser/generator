const logString = typeof process != "undefined" && process.env.LOG || ""

function log(name: string) { return new RegExp("\\b" + name + "\\b").test(logString) }

export default {
  grammar: log("grammar"),
  lr: log("lr"),
  parse: log("parse")
}
