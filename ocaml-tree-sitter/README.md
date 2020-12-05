# OCaml bindings for using tree-sitter generated parsers

### Why write this bindings instead of just using Menhir?

Menhir is a fabulous tool, but it is unfortunately more tied to OCaml than
anything else. This means that the grammars I've been writing here for Erlang
won't be usable outisde of the OCaml ecosystem without considerable effort.

Instead, I decided to write an Erlang grammar with
[tree-sitter](https://tree-sitter.github.io), so that the grammar itself can be
reused in essentially any language that has tree-sitter bindings.

Now with this bindings, OCaml can also use tree-sitter generated parsers, which
means we can resuse the [Erlang parser] in this project, but also parsers for:

* HTML
* TOML
* TypeScript
* Javascript
* Lua
* Bash
* Markdown
* [and many, many more](https://tree-sitter.github.io/tree-sitter/#available-parsers)
