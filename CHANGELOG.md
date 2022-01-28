## 0.15.4 (2022-01-28)

### Bug fixes

Fix a bug where explicitly specified token precedences were sometimes not properly enforced.

## 0.15.3 (2022-01-21)

### Bug fixes

Fix a bug that caused some kind of skip rules (those ending in something optional) to not work correctly.

## 0.15.2 (2021-09-24)

### Bug fixes

Fix an infinite recursion caused by some kinds of (obscure) token state machines.

## 0.15.1 (2021-09-03)

### Bug fixes

Fix a bug that could lead to spurious 'inconsistent skip sets' errors.

Fix a bug that caused an unescaped '-' at the start or end of a character set to silently be converted to a nonsensical character.

Fix a confusing behavior where literal tokens declared in the `@tokens` block didn't get names when they started with a lower-case character.

### New features

Top rules may now be defined inside `@skip` scopes.

The parser no longer treats an empty position in a choice operator as the empty expression, but requires an explicit () marker (to avoid a common mistake).

## 0.15.0 (2021-08-11)

### Breaking changes

The module's name changed from `lezer-generator` to `@lezer/generator`.

Nested parsers can no longer be specified in the grammar (they must now be set up programmatically).

### Bug fixes

Fix an issue where newlines in string tokens could silently corrupt the token.

Handle alternative output file extensions more gracefully.

### New features

`@export` props may now have a value to set a specific export name.

## 0.13.4 (2021-05-14)

### Bug fixes

Don't add inline rules to the terms file (since they may not be uniquely identified by name).

Generate more minimal state machines for the tokenizer.

## 0.13.3 (2021-02-17)

### New features

Support `@context` syntax to register a context tracker for a grammar.

## 0.13.2 (2021-01-20)

### Bug fixes

Fix an issue where imported identifiers could clash with the export name in generated code.

## 0.13.1 (2020-12-04)

### Bug fixes

Fix versions of lezer packages depended on.

## 0.13.0 (2020-12-04)

### Breaking changes

Adjust to the new way nested parsers work in Lezer.

### Bug fixes

Top rule node types will now show up in the terms file.

It is no longer allowed for a top rule to share a name with another rule.

## 0.12.0 (2020-10-23)

### Breaking changes

The serialized parser format changed.

Pseudo-props like `name`, `dialect`, `inline` and `dynamicPrec` now require an `@` in front of them when specified in a rule's prop list.

`@export` is now specified as a pseudo-prop instead of in front of the rule.

Top rule names are now required.

### New features

Rules can now specify an `@isGroup` pseudo-prop to automatically attach a group name to all the (single) named nodes they produce.

## 0.11.2 (2020-09-29)

### Bug fixes

Fix a crash that could happen when reporting a conflict error.

### New features

A `@conflict` block inside `@tokens` can now be used to explicitly indicate a conflict between two tokens.

Allow rules to be explicitly inlineable with an `[inline]` pseudo-prop.

## 0.11.1 (2020-09-26)

### Bug fixes

Fix lezer depencency versions

## 0.11.0 (2020-09-26)

### Breaking changes

Simplify the representation of repeat expressions in the grammar in a way that avoids some spurious conflicts.

The output format has been modified to allow states to share part of their action table for better compression.

### Bug fixes

Fix a bug where the state collapsing could introduce GLR parsing in grammars that otherwise didn't require it.

## 0.10.5 (2020-09-15)

### Bug fixes

Fix a bug where `moduleStyle` defaulted to `"cjs"` when using the node API (rather than to `"es"` as documented).

### New features

You can now import `"lezer-generator/rollup"` to get a rollup plugin that will transform grammars during the build.

## 0.10.4 (2020-09-14)

### Bug fixes

Fix a bug that broke `@external prop` declarations in grammars.

## 0.10.3 (2020-09-11)

### Bug fixes

Make sure unrelated precedence declarations for non-cyclic overlapping tokens don't end up also defining a relation between those tokens.

## 0.10.2 (2020-09-02)

### Bug fixes

Actually reuse parser states when skip rules refer to rules also used in other contexts.

Fix a bug where the automaton generated for skip rules wasn't being compressed.

Properly raise an error when different specializations for the same token are given different names.

Fix a bug that prevented `NodeProp.skipped` from being properly attached to node types.

Fix a corner-case infinite loop in the state-collapsing algorithm (and speed it up).

Compile `+` and `*` operators in a way that is less likely to lead to conflicts.

Emit all shift/reduce and reduce/reduce conflicts in a single run, rather than stopping on the first one.

Emit all overlapping token errors, rather than only the first one.

### New features

Inline rules can now be anonymous (with syntax `[props..] { body }`).

Dynamic precedences can now be associated with productions, which can help pick the preferred GLR parse when a grammar is ambiguous.

Token `@precedence` declarations can now refer to a parameterized rule by name (without arguments) to indicate that all instances of that rule have a given precedence.

## 0.10.1 (2020-08-07)

### Bug fixes

Fix an issue where the output file would in some cases have a superfluous comma.

## 0.10.0 (2020-08-07)

### Bug fixes

Fix a bug in the reuse of compiled rules (which sometimes got confused by inline rules).

The error message for overlapping tokens is a bit more concrete now, including an example of a string that matches both tokens.

### New features

Add support for grammar dialects.

Add support for external specializers.

Commas in precedence tag, dialect, or external token lists are now optional.

### Breaking changes

Changes the serialized parser format.

## 0.9.1 (2020-07-08)

### New features

The test runner helper now allows tests to pass additional configuration options.

## 0.9.0 (2020-06-08)

### Breaking changes

The `@detectDelim` directive now assigns `NodeProp.openedBy`/`closedBy` props to the bracket nodes, instead of `NodeProp.delim` to the parent node.

## 0.8.5 (2020-05-01)

### Bug fixes

Publish less useless cruft to npm, reducing package size.

## 0.8.4 (2020-04-14)

### Bug fixes

Fix an issue where token groups were inappropriately merged when conflicting tokens didn't appear in overlapping state sets.

Fix an issue where external tokenizers needed for skipped tokens were not properly enabled for some states.

Fix a bug where the tool would sometimes overeagerly merge states, resulting in incorrect output.

## 0.8.3 (2020-04-09)

### Bug fixes

Make dist/test loadable from CommonJS modules again.

Fix a bug that prevented `NodeProp.top` from being assigned to top rules in most cases.

## 0.8.2 (2020-04-01)

### Bug fixes

Fix an issue that broke the bin command.

## 0.8.1 (2020-04-01)

### Bug fixes

Make the package load as an ES module on node

## 0.8.0 (2020-02-03)

### Breaking changes

Changes the serialized parser format.

### New features

Add support for multiple `@top` rules.

## 0.7.1 (2020-01-23)

### New features

Support `Foo(...)` syntax in test specs to indicate "ignore the children of this node".

## 0.7.0 (2020-01-20)

### New features

You can now write a node name directly after `@top` to give your top node type a name.

### Breaking changes

Changes the way repeated nodes are represented in the generated parser.

## 0.5.2 (2020-01-15)

### Bug fixes

Fix crash when the top rule had no node name.

Adjust the way states' forced reductions are computed to avoid cycles (where force-reducing multiple times gets you back in your original state).

## 0.5.1 (2019-10-22)

### Bug fixes

Fix issue where serialized parsers didn't have the top node prop set.

## 0.5.0 (2019-10-22)

### New features

The generator now automatically assigns the `top` node prop to the grammar's top node.

## 0.4.0 (2019-09-10)

### Bug fixes

Fix bug that made matching single-token skipped expressions unnecessarily expensive.

### Breaking changes

Do not emit recovery actions in the parse table anymore.

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
