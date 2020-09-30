# Roadmap

These are some things I intend to work on in the near future,
somewhat ordered:

## OCaml to Erlang compilation

* multiple branches in a match should not be supported (or their code should be
  duplicated for each branch)

* refactor fun refs to resolve arity at compile time

* investigate how to flatten out functors

* figure out labeled function arguments

* safe variable names:
  * deal with prime's
  * rebinding = renaming (X = 1, X = X + 1, becomes X2 = X + 1)
  * if a type variable is unbound on the return side, we should `any()` it

* support for guards:
  * any expression in most places
  * only allowlisted ones in function cases

*  consider [adding support for reason syntax](https://github.com/anmonteiro/bucklescript/blob/fork/jscomp/syntax/ast_reason_pp.ml)

## Standard Erlang libraries

* work through feature parity list

erl(parser): Extend Erlang parser to parse some OTP modules

erl(ast): parametrize the ast to include a context, we can then use that on
  parse time to include location information in case of ast invariants being
  broken, or at typed-tree->ast translation time to include information about
  the exact Erlang terms that failed type checking
  
## Core Erlang libraries

* work through feature parity list

core(parser): figure out overlap with erlang parser, steal it!

core(printer): Finish pretty printer of Core AST

## OCaml Lambda to Core Erlang compilation

comp(ml,core): Scout out what parts of Lambda are not compileable to Core (e.g, `Passign`)

comp(ml,core): Finish mapping from Lambda

comp(ml,core): Brainstorm ways to verify the semantics have not changed from
  `erlc +to_core` to `caramelc compile --to-core`

comp(ml,core): Figure out what type information is available at Lambda stage
  and if it makes sense to use that, or pull in the Signature to decore the
  Core AST

## Erlang type-checker

typ(invariants): do not allow tuples of 1 element

