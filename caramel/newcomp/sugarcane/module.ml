(** Build a single Erlang module from a Typedtree.structure, and an optionally
    constraining Types.signature.
 *)
let make :
    module_name:Erlang.Ast.atom ->
    modules:Erlang.Ast.t list ->
    Typedtree.structure ->
    Types.signature option ->
    Erlang.Ast.t =
 fun ~module_name ~modules typedtree signature ->
  let exports = Export.make typedtree signature in
  let types = Typespecs.make typedtree signature in
  let functions = Fun.make ~module_name ~modules typedtree in
  let attributes = [] in
  let behaviours = [] in
  Erl.Mod.mk ~behaviours ~exports ~types ~functions ~attributes
    (Erl.Atom.lowercase module_name)
