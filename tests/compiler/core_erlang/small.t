  $ cat >small.ml <<EOF
  > let add x y = x + y
  > EOF
  $ caramelc compile --dump-ast --target=core small.ml
  ERROR: Ocaml_to_core_erlang__Ast_transl.Unsupported_primitive_operation
  [1]
  $ cat small.core
  cat: small.core: No such file or directory
  [1]
