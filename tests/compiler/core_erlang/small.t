  $ cat >small.ml <<EOF
  > let add x y = x + y
  > EOF
  $ caramelc compile --dump-ast --target=core small.ml
  ERROR: Caramel_compiler.Lambda_to_core_erlang.Unsupported_primitive_operation
  $ cat small.core
  cat: small.core: No such file or directory
  [1]
