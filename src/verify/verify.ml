let extract_sources sources =
  let erl = List.hd sources in
  let ml = List.hd (List.tl sources) in
  (erl, ml)

let compare_typedtree a b =
  if a == b then Ok () else Error `not_equal

let verify sources =
  let erl, ml = extract_sources sources in
  let ml_typedtree =
    Caramel_typing.Ocaml.implementation ~source_file:ml
      ~output_prefix:(Filename.chop_extension ml ^ "_verif")
  in
  let erl_typedtree =
    Caramel_typing.Erlang_as_ocaml.check ~source_file:erl
      ~output_prefix:(Filename.chop_extension erl ^ "_verif")
      ~dump_ast:false
  in
  compare_typedtree erl_typedtree ml_typedtree
