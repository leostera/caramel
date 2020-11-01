## v0.0.14

The `erlang` library is now completely free of references to Caramel and should
be usable on its own without problems.

The `caramel/stdlib` has now been refactored to include the necessary Erlang
runtime to not depend anymore on special cases in the Erlang AST printer.

Some tests have been updated accordingly.

The `gen_tcp` example module has been split into an ffi and type definitions to
leverage the fact that empty modules (full of ffi's) do not generate any Erlang
code. This way getting the example up and running is more straightforward.

## v0.0.13

* erlang: prepare erlang library for initial release to opam.

* erlang: add a new `erldump` binary that can be used to dump the parsed ast.
  This is currently being used for the tests.

* caramelc: binary now exits with status code 0 only if everything went well.

* ci: include necessary erlang artifacts in releases so we can publish this
  library to opam.

* ci: prefix cache names with the secret version so breaking them is more
  effective.

* examples: add new `gen_tcp` example.

* docs: publish small website at `caramel.abstractmachines.dev`

## v0.0.12

* compiler: match expressions with cascading cases are not that
  straighforward to translate to Erlang and my gut tells me that doing the
  juggling here will get in the way of type-checking Erlang in the
  upcoming milestone, so this is forbidden as it is right now.

## v0.0.11

* compiler: preliminary support for guards is added in this release.
  They are "safe" as long as you don't redefine any of the expected
  functions. There is no good way at the moment to prevent this from
  happening, but we could achieve it by essentially forbidding the use
  of the module name `Erlang` and requiring all guards to be fully
  qualified names.

  This still leaves us with the issue of compounded guard expressions.

  At the end of the day, we want a function `is_valid_guard : expression -> bool`
  that can traverse an arbitrary expression and tell us if it
  is or is not a valid guard based on the specification found at [the
  Erlang docs](https://erlang.org/doc/reference_manual/expressions.html#guard-expressions).

* caramelc: the `parse` subcommand now can take a `--lang` and `--tree`
  parameters to print out the parse and typed trees for OCaml, as well
  as the parse tree of Erlang, and the parse tree of the result of
  compiling OCaml to Erlang.

  This is particularly useful for testing and understanding the AST
  translation, and will likely be used later on to see if the compile ->
  typecheck -> compile cycle yields the same inputs, and thus is an
  isomorphism.

## v0.0.10

* Add support for record updates (see #23):

```ocaml
{ my_record with field = value }
```
```erlang
My_record#{ field := value }
```

* Add tests showing that lambda function calls are now working as expected and
will be called with the right number of parameters:

```ocaml
let f () =
  let g () = 1 in
  g ()
```
```erlang
f() ->
  G = fun () -> 1 end,
  G().
```

## v0.0.9

* compiler(#12): the compiler will now let you know when you're
  redefining a function on the OCaml side, which is not allowed on the
  Erlang side and stop compilation.

* compiler(#16): shadowing bindings with let are (for) now unsupported
  on the OCaml side, which makes translation runtime safe. We won't see
  any more `X = X + 1` on the Erlang side.

* compiler(#15): to help with #16, priming of variables is now supported
  and translated to valid Erlang. We can write `x' = x + 1` and it will
  translate to `X_prime = X + 1`.

* compiler(#13): recursive let bindings within a function definition are
  now not supported since they don't have a direct Erlang equivalent and
  require runtime overhead.

* error messages have been created for all of the above

## v0.0.8

* stdlib: fix name of Io module so Merlin can pick it up properly.

## v0.0.7

Better releases
* compiler: automatic function reference arities! As see in #10

* compiler: We're ignoring all fresh type variables when translating to Dialyzer
  specs for now. More work will be done in #20

* caramelc: `caramelc compile` now supports multiple `--target` flags,
  so you can compile both archives and Erlang sources at once.

* caramelc: standard library will now by default be in the respective
  installation directory (respecting dune install conventions)

* stdlib: `Process.spawn/1` has been renamed to `Process.make/1` until
  we have support for module attributes (see #21)

* stdlib: Dropped top-level namespacing until we figure out how it can
  work best with .merlin

* ci: several changes to release flow, including a nicer folder
  structure in the tarball

* ci: entire codebase is instrumentabled by bisect_ppx now to start
  gathering coverage reports

* erlang: removed an unused helper

## v0.0.6

* Lots of work on the Stdlib, as visible in
  [issue #8](https://github.com/AbstractMachinesLab/caramel/issues/8).

* External definitions now can override the name they will use in the
  generated source code with their string value

* Parser supports arbitrarily parenthesized expressions

---

The idea for this pre-release is to start testing out how the Stdlib
feels to write some small programs and scan 1) whether some obvious FFIs
are missing, and 2) how straightforward it is to write new ones.


## v0.0.5

The internal modules for the compiler and the typing experiments are
split now, and the Erlang support is unified in a single library that
only depends on the standard library and the Sexplib library for
deriving Sexp representations.

The goals here are:

* make the codebase easier to hack on in general -- there's still work
  to do here, as I think the ideal structure would be flatter than it is
  right now.

* prepare the Erlang library for continued work -- extending the lexer
  with positioning information on every read token will help parametrize
  the AST with contextual information. This would aid in error
  reporting.

* establish clearer compilation and type checking paths:
  * from OCaml to Erlang,
  * from OCaml Lambda to Core Erlang,
  * from Erlang to Native binaries.

## v0.0.4-triples

dist: use valid gcc host triples in release names

## v0.0.4

First release shipping with a tiny Stdlib!

Now you don't really need OCaml installed for Caramel to be able to compile
some ML and typecheck some Erlang

## v0.0.3-musl

New release naming and a new linux+musl binary

## v0.0.3

* compilation should have much nicer errors when dealing with unsupported
  features and expression at the value and type level
* new command for type-checking erlang: `caramelc check`
* `--target` flag is now available, and supports `core`, `erl` and `native`
  targets
* `--dump-ast` flag is now documented and hooked up to core erlang backend
* string concatenation should support arbitrarily complex expressions now
* license was updated in binary file to point to BSD 3-clause too

## v0.0.2-bin

First release with `caramelc` binaries.

## v0.0.2

First release.
