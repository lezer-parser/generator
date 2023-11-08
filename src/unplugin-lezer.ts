import { createUnplugin } from "unplugin"

import {resolve, dirname} from "path"
import {promises as fs} from "fs"
import {buildParserFile} from "./index.js"

export const unpluginLezer = createUnplugin(() => {
  let built = Object.create(null)

  return {
    name: 'unplugin-lezer',

    resolveId(source, importer) {
      let m = /^(.*\.grammar)(\.terms)?$/.exec(source)
      if (!m) return null
      let id = resolve(importer ? dirname(importer) : process.cwd(), m[1])
      return m[2] ? `\0${id}.terms` : id
    },

    load(id) {
      let m = /^\0?(.*\.grammar)(\.terms)?$/.exec(id)
      if (!m) return null
      if (!m[2]) this.addWatchFile(id)
      let base = m[1]
      let build = built[base] || (built[base] = fs.readFile(base, "utf8").then(code => buildParserFile(code, {
        fileName: base,
        moduleStyle: "es",
        warn: message => this.warn(message)
      })))
      return build.then((result: ReturnType<typeof buildParserFile>) => m?.[2] ? result.terms : result.parser)
    },

    watchChange(id) {
      if (built[id]) built[id] = null
    }
  }
})

// For backwards compatibility
export const lezer = unpluginLezer.rollup
export const rollup = unpluginLezer.rollup
