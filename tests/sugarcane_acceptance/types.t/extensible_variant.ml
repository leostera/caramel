(* NOTE: I think the best we can do here is to use any() since we just do not
 * know how will this type be extended when we compile this module.
 *
 * Alternatively we could split compilation and code-generation and add a step
 * in the middle that would consolidate the Erlang AST, and would allow us to
 * collect all the variants being used for this type.
 *)
type t = ..

type t += What
