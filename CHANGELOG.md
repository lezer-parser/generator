## 0.3.0 (2019-08-22)

### Bug fixes

Fix several issues in the way forced reductions were picked, to avoid infinite reduction loops.

### New features

Add support for props and custom node names in the grammar notation.

Allow importing of props via `@external prop`.

Rule capitalization is now relevant (again), only capitalized rules appear in the tree by default.

### Breaking changes

Remove support for node tags, tag expressions, the `@tags` block, and everything else related to tags.

Replace tagged expression syntax with inline rule syntax.

Literal declarations must now go into the `@tokens` block.

The `@detectDelim` declaration must now appear at the top level.

The dash in `@external-...` syntax was dropped—`@external` is now a separate token.

External grammars that default to null must now have the word `empty` instead of `from "..."` (to resolve an ambiguity that syntax introduced).

## 0.2.0 (2019-08-02)

### Bug fixes

Fix bug where the grammar parser unintentionally required semicolon between rules in skip blocks.

Actually throw an error when detecing a skip inconsistency.

Track skip context more accurately through parse states.

Fix specializing of external tokens.

### New features

Add support for tags.

Add `@tags` blocks, allow tags for literals.

Add `@punctuation` to succinctly declare punctuation tags.

Add `@infer-delim` to enable automatic delimiter detection.

Add `@all` as a way to append tags to all tagged rules in the grammar.

Allow a choice of literals to be passed to `@specialize`/`@extend`.

Add `dist/test.js` with test helper functions.

### Breaking changes

Require `@` in front of grammar keywords.

Remove support for `=`-style tag declarations.

Replace `tag.foo` syntax with colon suffix syntax.

## 0.1.1 (2019-07-09)

### Bug Fixes

Actually include the .d.ts file in the published package.

## 0.1.0 (2019-07-09)

### New Features

First documented release.
