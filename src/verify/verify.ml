let extract_sources sources =
  let erl = List.hd sources in
  let ml = List.hd (List.tl sources) in
  (erl, ml)

let compare_parsetree a b = if a = b then Ok () else Error `not_equal

let remove_locations parsetree =
  let open Ast_mapper in
  let open Parsetree in
  let map_structure_item sub { pstr_loc = loc; pstr_desc = desc } =
    let open Ast_helper.Str in
    let loc = sub.location sub loc in
    match desc with
    | Pstr_value (_r, vbs) ->
        value ~loc Recursive (List.map (sub.value_binding sub) vbs)
    | Pstr_eval (x, attrs) ->
        let attrs = sub.attributes sub attrs in
        eval ~loc ~attrs (sub.expr sub x)
    | Pstr_primitive vd -> primitive ~loc (sub.value_description sub vd)
    | Pstr_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
    | Pstr_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Pstr_exception ed -> exception_ ~loc (sub.type_exception sub ed)
    | Pstr_module x -> module_ ~loc (sub.module_binding sub x)
    | Pstr_recmodule l -> rec_module ~loc (List.map (sub.module_binding sub) l)
    | Pstr_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Pstr_open x -> open_ ~loc (sub.open_declaration sub x)
    | Pstr_class l -> class_ ~loc (List.map (sub.class_declaration sub) l)
    | Pstr_class_type l ->
        class_type ~loc (List.map (sub.class_type_declaration sub) l)
    | Pstr_include x -> include_ ~loc (sub.include_declaration sub x)
    | Pstr_extension (x, attrs) ->
        let attrs = sub.attributes sub attrs in
        extension ~loc ~attrs (sub.extension sub x)
    | Pstr_attribute x -> attribute ~loc (sub.attribute sub x)
  in

  let mapper =
    {
      Ast_mapper.default_mapper with
      location = (fun _this _l -> Location.none);
      structure_item = map_structure_item;
    }
  in
  mapper.structure mapper parsetree

let verify sources =
  let open Caramel_typing in
  let erl, ml = extract_sources sources in
  let ml_parsetree =
    Ocaml.parse_implementation ~dump_ast:false ~source_file:ml
    |> remove_locations
  in
  Printast.structure 0 Format.std_formatter ml_parsetree;
  Format.fprintf Format.std_formatter "\n\n%!";
  let erl_parsetree = Erlang_as_ocaml.parse ~source_file:erl ~dump_ast:true in
  compare_parsetree erl_parsetree ml_parsetree
