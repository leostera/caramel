type opaque = string

(* FIXME: Idiomatic Erlang would actually generate a new module
   that has an opaque

   -type hidden() :: type_defs__opaque:hidden().
   -opaque hidden() :: any().

 *)
type hidden
