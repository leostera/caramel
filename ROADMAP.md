# Roadmap

These are some things I intend to work on in the near future,
somewhat ordered:

comp(ml, erl):
  * multiple branches in a match should not be supported (or their code should
    be duplicated for each branch)
  * wrap A as (A) when concatenating binary strings: A/binary -> (A)/binary

comp(ml, erl): before every raise, make sure to print out as much info as possible
  on the term/type that is unsupported.

comp(ml,erl): refactor fun refs to resolve arity at compile time

comp(ml,erl): investigate how to flatten out functors

comp(ml,erl): figure out labeled function arguments

comp(ml,erl): safe variable names:
  deal with prime's
  rebinding = renaming (X = 1, X = X + 1, becomes X2 = X + 1)
  if a type variable is unbound on the return side, we should `any()` it

comp(ml,erl): support for guards:
  any expression in most places
  only allowlisted ones in function cases

erl(parser): Extend Erlang parser to parse some OTP modules

erl(ast): parametrize the ast to include a context, we can then use that on
  parse time to include location information in case of ast invariants being
  broken, or at typed-tree->ast translation time to include information about
  the exact Erlang terms that failed type checking

core(parser): figure out overlap with erlang parser, steal it!

core(printer): Finish pretty printer of Core AST

comp(ml,core): Add flag `--to-core` to pick the core backend

comp(ml,core): Compile and compare against the `./erltest` suite

comp(ml,core): Scout out what parts of Lambda are not compileable to Core (e.g, `Passign`)

comp(ml,core): Finish mapping from Lambda

comp(ml,core): Brainstorm ways to verify the semantics have not changed from
  `erlc +to_core` to `caramelc compile --to-core`

comp(ml,core): Figure out what type information is available at Lambda stage
  and if it makes sense to use that, or pull in the Signature to decore the
  Core AST

erl(parser): do not allow tuples of 1 element

comp(re, erl):
  add support for reason syntax
  https://github.com/anmonteiro/bucklescript/blob/fork/jscomp/syntax/ast_reason_pp.ml
