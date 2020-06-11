import {fileTests} from "../dist/test.cjs"

describe("test", () => {

  it("handle parser error", () => {

    const content = `
# Working Spec

b

==> B

# Broken Spec

b
`
    const expectedError = `Unexpected file format in test-error.txt around

  | # Broken Spec
  |${ ' ' }
  | b
  | `;

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
