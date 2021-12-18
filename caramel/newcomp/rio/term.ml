module Erl = Erlang.Parsetree_helper

let from_ocaml_constant ~constant =
  let open Asttypes in
  let ctx = Erl.Loc.empty in
  match constant with
  | Const_char char -> Erl.Term.atom (Erl.Atom.mk ~ctx (String.make 1 char))
  | Const_float float -> Erl.Term.float ~ctx float
  | Const_int int -> Erl.Term.int ~ctx (string_of_int int)
  | Const_int32 int -> Erl.Term.int ~ctx (Int32.to_string int)
  | Const_int64 int -> Erl.Term.int ~ctx (Int64.to_string int)
  | Const_nativeint int -> Erl.Term.int ~ctx (Nativeint.to_string int)
  | Const_string (_str, _, _) ->
      Error.todo "Term.from_ocaml_constant(Const_string)"
