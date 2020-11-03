let extract_sources sources =
  let erl = List.hd sources in
  let ml = List.hd (List.tl sources) in
  (erl, ml)

let compare_typedtree a b = if a = b then Ok () else Error `not_equal

let default_stdlib_path =
  let ( / ) = Filename.concat in
  let dirname = Filename.dirname in
  let root = dirname (dirname Sys.executable_name) in
  root / "lib" / "caramel" / "stdlib"

let verify sources ~stdlib_path =
  let open Caramel_typing in
  let erl, ml = extract_sources sources in
  let (`Structure ml_typedtree) =
    Typing.run_check (fun () ->
        Typing.initialize_compiler ~stdlib_path ~no_stdlib:false;
        Ocaml.implementation ~source_file:ml
          ~output_prefix:(Filename.chop_extension ml ^ "_verif"))
  in
  let (`Structure erl_typedtree) =
    Typing.run_check (fun () ->
        Typing.initialize_compiler ~stdlib_path ~no_stdlib:false;
        Caramel_typing.Erlang_as_ocaml.check ~source_file:erl
          ~output_prefix:(Filename.chop_extension erl ^ "_verif")
          ~dump_ast:false)
  in
  compare_typedtree erl_typedtree ml_typedtree
