# lezer-generator

This is an [LR(1)](https://en.wikipedia.org/wiki/LR_parser) (more
precisely pseudo-[LALR](https://en.wikipedia.org/wiki/LALR_parser),
with opt-in [GLR](https://en.wikipedia.org/wiki/GLR_parser)) parser
generator which outputs grammars that can be used by the
[lezer](https://github.com/lezer-parser/lezer/) parser.

See `test/cases/` for some example grammars. Run the `lezer`
command to generate parser tables for a given grammar.

This project was hugely inspired by
[tree-sitter](http://tree-sitter.github.io/tree-sitter/).

The code is licensed under an MIT license.

Proper docs will follow when the system stabilizes.
