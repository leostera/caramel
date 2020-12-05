# OCaml bindings for using tree-sitter generated parsers

*NOTE*: there are already 2 other bindings for `tree-sitter`: in
[Oni](https://github.com/onivim/oni2/tree/master/src/reason-tree-sitter) and
[returntocorp/ocaml-tree-sitter](https://github.com/returntocorp/ocaml-tree-sitter).

### Why tree-sitter?

Menhir is a fabulous tool, but the grammars I've been writing here for Erlang
won't be usable outside of the OCaml ecosystem without some effort.

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

### Why _another_ set of bindings?

The bindings available in Oni do not expose the entire API surface of the
Parser to load languages, and it currently seems hardcoded to use the C and
JSON parsers.

The `returntocorp/ocaml-tree-sitter` project is a lot more involved and its
split into:

1. a code-generation tool that given a `grammar.json`, generated with
   `tree-sitter`, will generate a Concrete Syntax Tree and a translation module
   from the tree-sitter tree to it.

2. the same bindings from Oni

3. a small runtime library that runs the generated translator on the outputs of
   the parser and gives you a CST back

The bindings in the package in this repository are just a lightweight interface
to the tree-sitter runtime library, so you don't need more than a small OCaml
module, and a foreign library stanza, to use them:

```ocaml
(* tree_sitter_erlang.ml *)
external raw : unit -> Tree_sitter.Language.raw = "tree_sitter_erlang"
let language () = Tree_sitter.Language.from_raw_ptr (raw ())
```

```clojure
(library
 (name tree_sitter_erlang)
 (libraries tree_sitter)
 (foreign_stubs (language c) (names parser)))
```

## Testing

There's a single example/test right now that both inspects a parser's grammar
and iterates over the nodes and prints them all out:

```clojure

tree-sitter v12
Language version: 12
Node kind count: 8
Nodes:
  #0: end
  #1: atom
  #2: (
  #3: )
  #4: sexp
  #5: _sexp
  #6: list
  #7: list_repeat1
DONE
(hello (world (this is working (i think))))
(
hello
(world (this is working (i think)))
(
world
(this is working (i think))
(
this
is
working
(i think)
(
i
think
)
DONE

```

