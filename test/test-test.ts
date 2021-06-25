// @ts-ignore
import {fileTests} from "../dist/test.js"

describe("test", () => {

  it("handle parser error", () => {

    const content = `
# Working Spec

b

==> B

# Broken Spec

bbbb bbbb bbbb bbbb
bbbb bbbb bbbb bbbb
bbbb bbbb bbbb bbbb
bbbb bbbb bbbb bbbb aaaa
bbbb
`
    const expectedError = `Unexpected file format in test-error.txt around

  | # Broken Spec
  |${ ' ' }
  | bbbb bbbb bbbb bbbb
  | bbbb bbbb bbbb bbbb
  | bbbb bbbb bbbb bbbb
  | bbbb bbbb bbbb bbbb aaaa`;

    const file = "test-error.txt"

    try {
      fileTests(content, file)
    } catch (err) {
      if (err.message !== expectedError) {
        throw err;
      }
    }
  })

})
