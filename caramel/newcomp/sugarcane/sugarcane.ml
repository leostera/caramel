(** Turn an OCaml Typedtree into a list of Erlang ASTs that can be compiled to
    sources.
*)
let translate :
    module_name:string ->
    signature:Types.signature option ->
    Typedtree.structure ->
    Erlang.Ast.t list =
 fun ~module_name ~signature typedtree ->
  let top_module = Erl.Atom.(lowercase (mk module_name)) in
  let modules =
    List.fold_left
      (fun mods (nested_module_name, impl, sign) ->
        mk_module ~module_name:nested_module_name ~modules:mods impl sign
        :: mods)
      []
      (find_modules ~prefix:top_module typedtree)
  in
  [
    modules; [ mk_module ~module_name:top_module ~modules typedtree signature ];
  ]
  |> List.concat
