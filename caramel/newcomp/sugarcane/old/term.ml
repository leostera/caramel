let from_ocaml_constant ~constant =
  let open Asttypes in
  match constant with
  | Const_char _char -> Error.todo "const_char"
  | Const_float _float -> Error.todo "const_float"
  | Const_int _int -> Error.todo "const_int"
  | Const_int32 _int -> Error.todo "const_int32"
  | Const_int64 _int -> Error.todo "const_int64"
  | Const_nativeint _int -> Error.todo "nativeint"
  | Const_string (_str, _, _) ->
      Error.todo "Term.from_ocaml_constant(Const_string)"
