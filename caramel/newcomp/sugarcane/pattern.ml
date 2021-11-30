module Erl = Erlang.Parsetree_helper
open Typedtree

let rec from_ocaml_pattern :
    type k. pattern:k general_pattern -> 'ctx Erlang.Parsetree.pattern list =
 fun ~pattern ->
  match pattern.pat_desc with
  | Tpat_alias (_, _, _) -> Error.todo ()
  | Tpat_any -> Error.todo ()
  | Tpat_array _ -> Error.todo ()
  | Tpat_constant _ -> Error.todo ()
  | Tpat_construct ({ txt = id; _ }, _stuff, patterns) ->
      from_ocaml_constructor ~id ~patterns
  | Tpat_exception _ -> Error.todo ()
  | Tpat_lazy _ -> Error.todo ()
  | Tpat_or (_, _, _) -> Error.todo ()
  | Tpat_record (_, _) -> Error.todo ()
  | Tpat_tuple _ -> Error.todo ()
  | Tpat_value _ -> Error.todo ()
  | Tpat_var (id, _stuff) -> from_ocaml_binding ~id
  | Tpat_variant _ -> Error.todo ()

(** Handle constructors, including lists and the unit *)
and from_ocaml_constructor ~id ~patterns:_ =
  let name = Identifier.constructor id in
  match Identifier.Well_known.is_well_known name with
  | `unit -> [ Erl.Pat.empty_tuple ]
  | `not_well_known -> [ Erl.Pat.ignore ]

(** Handle variable bindings *)
and from_ocaml_binding ~id =
  let name = Identifier.binding id in
  [ Erl.Pat.bind ~name ]

let from_ocaml_case :
    case:Typedtree.value Typedtree.case -> 'ctx Erlang.Parsetree.pattern list =
 fun ~case -> from_ocaml_pattern ~pattern:case.c_lhs
