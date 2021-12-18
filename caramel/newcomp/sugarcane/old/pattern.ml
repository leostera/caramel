open Typedtree

let rec from_ocaml_pattern :
    type k. pattern:k general_pattern -> Ir.pat_desc list =
 fun ~pattern ->
  match pattern.pat_desc with
  | Tpat_alias (_, _, _) -> Error.todo "Tpat_alias"
  | Tpat_any -> Error.todo "Tpat_any"
  | Tpat_array _ -> Error.todo "Tpat_array"
  | Tpat_constant _ -> Error.todo "Tpat_constant"
  | Tpat_construct (loc, _stuff, _, patterns) ->
      from_ocaml_constructor ~ident:loc.txt ~patterns
  | Tpat_exception _ -> Error.todo "Tpat_exception"
  | Tpat_lazy _ -> Error.todo "Tpat_lazy"
  | Tpat_or (_, _, _) -> Error.todo "Tpat_or"
  | Tpat_record (_, _) -> Error.todo "Tpat_record"
  | Tpat_tuple _ -> Error.todo "Tpat_tuple"
  | Tpat_value _ -> Error.todo "Tpat_value"
  | Tpat_var (ident, _stuff) -> from_ocaml_binding ~ident
  | Tpat_variant _ -> Error.todo "Tpat_variant"

(** Handle constructors, including lists and the unit *)
and from_ocaml_constructor ~ident:_ ~patterns:_ =
  Error.todo "from_ocaml_constructor"

(** Handle variable bindings *)
and from_ocaml_binding ~ident:_ = Error.todo "from_ocaml_binding"

let from_ocaml_case :
    ctx:Context.t -> case:Typedtree.value Typedtree.case -> Ir.pat_desc list =
 fun ~ctx:_ ~case -> from_ocaml_pattern ~pattern:case.c_lhs
