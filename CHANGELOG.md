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
