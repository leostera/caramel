open Typedtree
open Types

exception Function_without_body of Typedtree.expression
exception Unsupported_feature
exception Unsupported_expression
exception Unsupported_empty_identifier
exception Unsupported_naming

let maybe e x = match x with
  | Some v -> v
  | None -> raise e

let maybe_unsupported x = maybe Unsupported_feature x

let rec varname_of_string s =
  let name = s |> String.capitalize_ascii in
  match String.get name 0, name with
  | '_', name when name <> "_" ->
      let name = name |> String.to_seq |> List.of_seq |> List.tl |> List.to_seq |> String.of_seq in
      "_" ^ (varname_of_string name)
  | _, _ -> name
let varname_of_ident i = i |> Ident.name |> varname_of_string

let atom_of_string = String.lowercase_ascii
let atom_of_ident i = i |> Ident.name |> atom_of_string

let longident_to_atom x = x |> Longident.last |> atom_of_string
let longident_to_name x =
  match x |> Longident.flatten |> List.rev with
  | [] ->
      raise Unsupported_empty_identifier
  | x :: [] ->
      Erlast.(Var_name x)
  | n_name :: mods ->
      let n_mod = (mods |> List.rev |> String.concat "__")  in
      Erlast.(Qualified_name { n_mod; n_name })

let ocaml_to_erlang_type t =
  let open Erlast in
  match t with
  | "int" -> Atom_name "integer"
  | "bool" -> Atom_name "boolean"
  | "option" -> Qualified_name { n_mod="option"; n_name="t" }
  | "result" -> Qualified_name { n_mod="result"; n_name="t" }
  | u -> Atom_name u

let longident_to_type_name x =
  match x |> Longident.flatten |> List.rev with
  | [] -> raise Unsupported_empty_identifier
  | x :: [] -> ocaml_to_erlang_type x
  | n_name :: mods ->
      let n_mod = (mods |> List.rev |> String.concat "__") |> String.lowercase_ascii in
      match n_mod, n_name with
      | _, x when x="option" || x="result" ->
          Erlast.(Qualified_name { n_mod=x; n_name="t" })
      | "erlang", "process" ->
          Erlast.(Qualified_name { n_mod; n_name="pid"})
      | _, _ ->
          Erlast.(Qualified_name { n_mod; n_name })

let to_erl_op t =  Erlast.(Qualified_name { n_mod = "erlang"; n_name = "'" ^ t ^ "'" })
let ocaml_to_erlang_primitive_op t =
  match t with
  | "!"
  | "++"
  | "-"
  | "--"
  | "/"
  | "<"
  | ">"
  | "*"
  | "+" -> to_erl_op t
  | "<>" -> to_erl_op "=/="
  | "=" -> to_erl_op "=:="
  | "==" -> to_erl_op "=="
  | u -> Erlast.Atom_name u

let const_to_literal const =
  let open Asttypes in
  match const with
  | Const_int int -> Erlast.Lit_integer (string_of_int int)
  | Const_char char -> Erlast.Lit_char (String.make 1 char)
  | Const_string (string, _, _) -> Erlast.Lit_binary string
  | Const_float string -> Erlast.Lit_float string
  | Const_int32 int32 -> Erlast.Lit_integer (Int32.to_string int32)
  | Const_int64 int64 -> Erlast.Lit_integer (Int64.to_string int64)
  | Const_nativeint nativeint -> Erlast.Lit_integer (Nativeint.to_string nativeint)


(** Build the actual functions of an Erlang module
 *)
let build_functions:
  module_name: string ->
  modules: Erlast.t list ->
  Typedtree.structure ->
  Erlast.fun_decl list =
  fun ~module_name ~modules typedtree ->
    (* NOTE: flatten down all the names bound in the parameters
     * to a single list that we can use to quickly check how to
     * normalize them in the function's body.
     *)
    let rec collect_var_names pat =
      let open Erlast in
      let rec collect acc p =
        match p with
        | [] -> acc
        | p :: ps ->
          let subpats = match p with
          | Pattern_list pats -> collect_var_names pats
          | Pattern_tuple pats -> collect_var_names pats
          | Pattern_map pats -> pats |> List.map(fun (_, p) -> p )
          | _ -> [p]
          in collect (subpats @ acc) ps
      in
      collect [] pat
    in

    let name_in_var_names ~var_names name =
      let open Erlast in
      var_names
      |> List.exists (fun pat ->
        match pat with
        | Pattern_binding x -> x = name
        | _ -> false)
    in

    let is_nested_module name =
      let name = name |> Longident.flatten |> List.hd in
      modules |> List.exists (fun Erlast.{ ocaml_name = mn } -> mn = name)
    in

    let rec build_function fd_name cases ~var_names =
      let rec params c acc =
        let acc' = (build_pattern c.c_lhs) :: acc in
        match c.c_rhs.exp_desc with
        | Texp_function { cases = [c']; } -> params c' acc'
        | _ -> acc' |> List.rev
      in

      let fd_arity = match cases with
        | [] -> 0
        | c :: _ -> (params c []) |> List.length
      in
      let fd_cases = cases |> List.map (fun case ->
        (* NOTE: we'll just traverse all the expressions in this case and
         * make sure we collapse as many top-level arguments for this function.
         *)
        let rec body c var_names =
          match c.c_rhs.exp_desc with
          | Texp_function { cases = [c']; } ->
              let pattern =  (build_pattern c'.c_lhs) in
              let var_names = (var_names @ (collect_var_names [pattern])) in
              body c' var_names
          | _ ->
            begin match build_expression c.c_rhs ~var_names with
            | Some exp -> exp
            | _ -> raise (Function_without_body c.c_rhs)
          end
        in

        let fc_lhs = params case [] in
        let fc_rhs = body case (var_names @ (collect_var_names fc_lhs)) in
        let fc_guards = [] in
        Erlast.{ fc_lhs; fc_guards; fc_rhs }
      ) in Some Erlast.{ fd_name; fd_arity; fd_cases }

    (* NOTE: We need a universally quantified k here because this function will
     * be called with several types indexing general_pattern *)
    and build_pattern: type k. k general_pattern -> Erlast.pattern =
      fun pat ->
      match pat.pat_desc with
      | Tpat_var (id, _) ->
          let var_name = id |> varname_of_ident in
          Erlast.Pattern_binding var_name

      | Tpat_value t ->
          (* NOTE: type casting magic! *)
          build_pattern (t :> pattern)

      | Tpat_tuple tuples ->
          Erlast.Pattern_tuple (List.map build_pattern tuples)

      | Tpat_record (fields, _) ->
          Erlast.Pattern_map (fields |> List.map (fun (Asttypes.{txt}, _, pattern) ->
            (longident_to_atom txt, build_pattern pattern)
          ))

      | Tpat_construct ({ txt }, _, _) when longident_to_atom txt = "()" ->
          Erlast.Pattern_tuple []

      | Tpat_construct ({ txt }, _, patterns) when longident_to_atom txt = "::" ->
          Erlast.Pattern_list (List.map build_pattern patterns)

      | Tpat_construct ({ txt }, _, []) ->
          Erlast.Pattern_match (Erlast.Lit_atom (longident_to_atom txt))

      | Tpat_construct ({ txt }, _, patterns) ->
          let tag = Erlast.Pattern_match (Erlast.Lit_atom (longident_to_atom txt)) in
          let values = (List.map build_pattern patterns) in
          Erlast.Pattern_tuple (tag :: values)

      | Tpat_variant (label, None, _) ->
          Erlast.Pattern_match (Erlast.Lit_atom (atom_of_string label))

      | Tpat_variant (label, Some expr, _) ->
          let tag = Erlast.Pattern_match (Erlast.Lit_atom (atom_of_string label)) in
          let value = build_pattern expr in
          Erlast.Pattern_tuple [tag; value]

      | Tpat_constant const ->
          Erlast.Pattern_match (const_to_literal const)


      (* NOTE: here's where the translation of pattern
       * matching at the function level should happen. *)
      | _ ->
          Erlast.Pattern_ignore

    and build_bindings vbs ~var_names =
      match vbs with
      | vb :: [] ->
          Erlast.{
            lb_lhs = build_pattern vb.vb_pat ;
            lb_rhs = build_expression vb.vb_expr ~var_names |> maybe_unsupported;
          }
      | _ ->
          List.iter (Printtyped.value_binding 0 Format.std_formatter) vbs;
          Format.fprintf Format.std_formatter "Caramel does not support \"let and\" bindings!\n";
          raise Unsupported_feature

    and build_expression exp ~var_names =
      match exp.exp_desc with
      | Texp_constant constant ->
          let v = const_to_literal constant  in
          Some (Erlast.Expr_literal v)

      | Texp_ident (_, {txt}, _) when longident_to_atom txt = "__MODULE__" ->
          Some (Erlast.Expr_name (Erlast.Macro_name "MODULE"))

      | Texp_ident (_, {txt}, _) ->
          let name = longident_to_name txt in
          let var_name = txt |> longident_to_atom |> varname_of_string in

          (* NOTE: an identifier may be a currently bound variable name *)
          if (name_in_var_names ~var_names var_name)
          then Some (Erlast.Expr_name (Erlast.Var_name var_name))

          (* NOTE: or it may be a function name of 3 kinds *)
          else begin match name, is_nested_module txt with

            (* NOTE: qualified and local, refering a module that's nested *)
            | Erlast.Qualified_name { n_mod; n_name }, true ->
                let name = Erlast.Qualified_name {
                  n_name=n_name;
                  n_mod=module_name ^ "__" ^ n_mod;
                } in Some (Erlast.Expr_name name)

            (* NOTE: qualified and external, refering to a module that is not nested *)
            | Erlast.Qualified_name _, false ->
                Some (Erlast.Expr_name name)

            (* NOTE: unqualified, and thus refering to a function reference *)
            | _, _ -> Some (Erlast.Expr_fun_ref (atom_of_string var_name))
          end

      | Texp_construct ({ txt }, _, _expr) when longident_to_atom txt = "[]" ->
          Some (Erlast.Expr_list [])

      | Texp_construct ({ txt }, _, _expr) when longident_to_atom txt = "()" ->
          Some (Erlast.Expr_tuple [])

      | Texp_construct ({ txt }, _, []) ->
          Some (Erlast.Expr_name (Atom_name (longident_to_atom txt)))

      (* NOTE: lists are just variants :) *)
      | Texp_construct ({ txt }, _, exprs) when longident_to_atom txt = "::" ->
          let values = exprs |> List.filter_map(build_expression ~var_names) in
          Some (Erlast.Expr_list values)

      (* NOTE: these are actually the variants! and Texp_variant below is for
       * polymorphic ones *)
      | Texp_construct ({ txt }, _, exprs) ->
          let tag = Erlast.Expr_name (Atom_name (longident_to_atom txt)) in
          let values = exprs |> List.filter_map(build_expression ~var_names) in
          Some (Erlast.Expr_tuple (tag :: values))

      | Texp_variant (label, None) ->
          Some (Erlast.Expr_name (Erlast.Atom_name label))

      | Texp_variant (label, Some expr) ->
          let tag = Erlast.Expr_name (Erlast.Atom_name label) in
          let value = build_expression ~var_names expr |> maybe_unsupported in
          Some (Erlast.Expr_tuple [tag; value])

      | Texp_apply (expr, args) ->
          let fa_name =
            match build_expression expr ~var_names |> maybe_unsupported with
            | Erlast.Expr_fun_ref n ->
                Erlast.Expr_name (n |> ocaml_to_erlang_primitive_op)
            | x -> x
          in
          let fa_args = args |> List.map (fun (_, arg) ->
            arg
            |> maybe_unsupported
            |> build_expression ~var_names
            |> maybe_unsupported
          ) in Some (Erlast.Expr_apply { fa_name; fa_args })

      (* NOTE: use `extended_expression` to provide map overrides *)
      | Texp_record { fields; } ->
          Some (Erlast.Expr_map (fields |> Array.to_list |> List.map (fun (field, value) ->
            let value = match value with
            | Kept _ ->
                Format.fprintf Format.std_formatter "record overrides unsupported yet!";
                Printtyped.expression 0 Format.std_formatter exp;
                raise Unsupported_feature
            | Overridden (_, exp) -> begin match build_expression exp ~var_names with
                | None ->
                    Printtyped.expression 0 Format.std_formatter exp;
                    raise Unsupported_feature
                | Some v -> v
                end
            in
            Erlast.{ mf_name = field.lbl_name; mf_value = value }
          )))

      | Texp_tuple exprs ->
          Some (Erlast.Expr_tuple (exprs |> List.filter_map (build_expression ~var_names)))

      | Texp_match (expr, branches, _) ->
        let expr = build_expression expr ~var_names |> maybe_unsupported in
        (* NOTE: match on c_guard here to translate guards *)
        let branches: Erlast.case_branch list = branches |> List.map (fun c ->
          let cb_pattern = build_pattern c.c_lhs in
          let var_names = (collect_var_names [cb_pattern]) @ var_names in
          let cb_expr = build_expression c.c_rhs ~var_names |> maybe_unsupported in
          Erlast.{ cb_pattern; cb_expr }
        )
        in Some (Erlast.Expr_case (expr, branches))

      | Texp_ifthenelse (if_cond, if_true, if_false) ->
          let expr = build_expression ~var_names if_cond |> maybe_unsupported in
          let if_true =
            Erlast.{ cb_pattern = Erlast.Pattern_match (Erlast.Lit_atom "true");
                     cb_expr = build_expression ~var_names if_true
                               |> maybe_unsupported }
          in
          let if_false =
            match if_false with
            | Some if_false ->
                [Erlast.{ cb_pattern = Erlast.Pattern_match (Erlast.Lit_atom "false");
                         cb_expr = build_expression ~var_names if_false
                                   |> maybe_unsupported }]
            | None -> []
          in
          let branches = if_true :: if_false in
          Some (Erlast.Expr_case (expr, branches))

      | Texp_let (_, vbs, expr) ->
          (* NOTE: consider flattening let-ins ?
          let rec flatten e acc =
            match e with
            | Texp_let (_, vbs, e') -> flatten e' (e :: acc)
            | _ -> (e :: acc) |> List.rev
          in
          let bindings = flatten expr [] in
          *)
          let let_binding = build_bindings vbs ~var_names in
          let var_names = (collect_var_names Erlast.([let_binding.lb_lhs])) @ var_names in
          let let_expr = build_expression ~var_names expr |> maybe_unsupported in
          Some (Erlast.Expr_let (let_binding, let_expr))


      | Texp_function { cases } ->
          begin match build_function ~var_names "anonymous" cases with
          | Some f -> Some (Erlast.Expr_fun f)
          | None -> None
          end

      | Texp_sequence (this, next) ->
          let let_binding = Erlast.{
            lb_lhs = Erlast.Pattern_ignore;
            lb_rhs = build_expression this ~var_names |> maybe_unsupported;
          } in 
          let let_expr = build_expression ~var_names next |> maybe_unsupported in
          Some (Erlast.Expr_let (let_binding, let_expr))

      | _ ->
          Printtyped.expression 0 Format.std_formatter exp;
          raise Unsupported_expression
    in

    let build_value vb =
      match vb.vb_pat.pat_desc, vb.vb_expr.exp_desc with
      | Tpat_var (id, _), Texp_function { cases; } ->
          let id = id |> atom_of_ident in
          build_function ~var_names:[] id cases
      | _ -> None
    in

    typedtree.str_items
    |> (List.fold_left (fun acc item  ->
        match item.str_desc with
        | Tstr_value (_, vb) ->
            (List.filter_map build_value vb) @ acc
        | _ -> acc
    ) [])
    |> List.rev

(** Build the types of an Erlang module.
 *)
let build_types:
  Typedtree.structure
  -> Erlast.type_decl list =
  fun typedtree ->
    let rec build_type_kind core_type =
      match core_type.ctyp_desc with
      | Ttyp_any -> Some (Erlast.type_any)

      | Ttyp_var var_name -> Some (Erlast.Type_variable (var_name |> varname_of_string))

      (* NOTE: OCaml works with functions from one type to another, and supports
       * multiargument functions via currying or tuples.
       *
       * Erlang doesn't, so we'll squash the function type into a single function
       * with multiple arguments instead.
       *)
      | Ttyp_arrow (_, param, out) ->
          let rec args t acc = match t.ctyp_desc with
            | Ttyp_arrow (_, p, t') -> args t' (p :: acc)
            | _ -> (t :: acc) |> List.rev
          in
          let args = (args out [param]) |> List.filter_map build_type_kind in
          Some (Erlast.Type_function args)


      (* NOTE: this allows us to export type aliases that may have been made
       * opaque, such as `type opaque = string`, as `-type opaque() :: string().`
       *
       * It is also used for application of type constructors: `type a = string list`
       * gets compiled to `-type a() :: list(string()).`
       *)
      | Ttyp_constr (_, { txt; }, args) ->
          let tc_name = longident_to_type_name txt in
          let tc_args = args |> List.filter_map build_type_kind in
          Some (Erlast.Type_constr { tc_name; tc_args })

      | Ttyp_tuple els ->
          let parts = (els |> List.filter_map build_type_kind) in
          Some (Erlast.Type_tuple parts)

      | Ttyp_variant  (rows, _closed, _labels) ->
          let rec all_rows rs acc =  match rs with
            | [] -> acc |> List.rev
            | r :: rs' -> match r.rf_desc with
              | Tinherit _ctype -> all_rows rs' acc
              | Ttag ( {txt}, _, core_types) ->
                  let vc_name = txt |> atom_of_string in
                  let vc_args = (core_types |> List.filter_map build_type_kind) in
                  let variant = Erlast.{ vc_name; vc_args } in
                  all_rows rs' (variant :: acc)
          in
          let constructors = all_rows rows [] in
          Some (Erlast.Type_variant {constructors})

      (* NOTE: these are two core type constructors that are essentially "links"
       * to follow.
       *
       * The second one `Ttyp_poly (strings, core_typ)` seemed to appear in records.
       *)
      | Ttyp_alias (follow, _)
      | Ttyp_poly (_, follow) -> build_type_kind follow

      | Ttyp_object _
      | Ttyp_class _
      | Ttyp_package _ ->
          Printtyped.core_type 0 Format.std_formatter core_type;
          raise Unsupported_feature
    in

    let build_record labels =
      let fields = labels |> List.map(fun Typedtree.{ ld_id; ld_type } ->
        let rf_name = atom_of_ident ld_id in
        let rf_type = match build_type_kind ld_type with
          | Some t -> t
          | None -> Erlast.type_any
        in Erlast.{ rf_name; rf_type })
      in Erlast.Type_record {fields}
    in

    let build_abstract name params _type_decl core_type =
      match build_type_kind core_type with
      | Some kind -> Some (Erlast.make_named_type name params kind)
      | None -> None
    in

    let build_type_params params =
      params
      |> List.filter_map (fun (core_type, _) ->
          match core_type.ctyp_desc with
          | Ttyp_var name -> Some (name |> varname_of_string)
          | _ -> None
      )
    in

    let build_type type_decl =
      let name = (atom_of_ident type_decl.typ_id) in
      let params = build_type_params type_decl.typ_params in
      match type_decl.typ_kind with

      (* NOTE: turns out that "abstract" here means "only structure, no names!"
       * so this branch will generate the appropriate types for tuples, aliases
       * and actual abstract types.
       * *)
      | Ttype_abstract  ->
          begin match type_decl.typ_manifest with
          | Some abs -> (build_abstract name params type_decl abs)
          | None ->
              let ref = (Erlast.Type_constr { tc_name=Atom_name "reference"; tc_args=[]}) in
              Some (Erlast.make_named_type name params ref)
          end

      | Ttype_record labels ->
          let record = build_record labels in
          Some (Erlast.make_named_type name params (record))

      | Ttype_variant constructors ->
          let constructors = constructors
          |> List.map(fun Typedtree.{ cd_id; cd_args } ->
              let vc_args = match cd_args with
                | Cstr_tuple core_types -> core_types |> List.filter_map build_type_kind
                | Cstr_record labels -> [build_record labels]
              in
              Erlast.{ vc_name = atom_of_ident cd_id; vc_args } )
          in Some (Erlast.make_named_type name params (Erlast.Type_variant {constructors}))

      | _ -> None
    in
    typedtree.str_items
    |> (List.concat_map (fun item  ->
        match item.str_desc with
        | Tstr_type (_, tys)  -> tys
        | _ -> []
    ))
    |> List.filter_map build_type

(** Build the exports table of an Erlang module

    This will look for the signature of the module to determine what to export.

    The type-chain looks like this:
      * Typedtree.structure
      * Types.signature
      * types.signature_item
 *)
let build_exports:
  name:string
  -> Typedtree.structure
  -> Types.signature option
  -> Erlast.export list =
  fun ~name:_ typedtree signature ->
    let rec collect_args = fun value args ->
      match value.desc with
      | Tarrow (_, arg, next, _) -> collect_args next (arg :: args)
      | Tlink t -> collect_args (Btype.repr t) args
      | _ -> args
    in

    let signature = match signature with
      | None -> typedtree.str_type
      | Some x -> x
    in

    signature |> (List.filter_map (fun sig_item ->
      match sig_item  with
      | Sig_value (_, { val_kind = Val_prim _  }, Exported) ->
          None
      | Sig_value (name, vd, Exported) ->
          let name = (atom_of_ident name) in
          let args = collect_args vd.val_type [] in
          let args = match args with
          | ({ desc = Tconstr (p, _, _)} :: rest) when Path.same p Predef.path_unit -> rest
          | _ -> args
          in
          let arity = (List.length args) in
          Format.fprintf Format.std_formatter "%s/%d\n" name arity;
          Some (Erlast.make_fn_export name arity)
      | Sig_type (name, td, _, Exported) ->
          Some (Erlast.make_type_export (atom_of_ident name) td.type_arity)
      | _  -> None
    ))

(** Build a single Erlang module from a Typedtree.structure, and an optionally
    constraining Types.signature.
 *)
let build_module:
  name: string ->
  ocaml_name: string ->
  modules: Erlast.t list ->
  Typedtree.structure ->
  Types.signature option -> Erlast.t =
  fun ~name ~ocaml_name ~modules typedtree signature ->
    let exports = build_exports ~name typedtree signature in
    let types = build_types typedtree in
    let functions = build_functions ~module_name:name ~modules typedtree in
    Erlast.make ~name ~ocaml_name ~exports ~types ~functions

(** Navigate a [Typedtree.structure] and recursively collect all module definitions,
    building up the right prefixed names.

    The type-chain looks like this:
      * Typedtree.structure
      * Typedtree.structure_item
      * Typedtree.structure_item_desc
      * Typedtree.module_binding
      * Typedtree.module_expr
      * Typedtree.module_expr_desc
      * Typedtree.structure -> back to the top again
 *)
let rec find_modules:
  prefix:string
  -> Typedtree.structure
  -> (string * string * Typedtree.structure * (Types.signature option)) list =
  fun ~prefix typedtree ->
    let module_name prefix mb_id = (match mb_id with
          | Some x -> prefix ^ "__" ^ (atom_of_ident x)
          | None -> prefix) |> String.lowercase_ascii
    in
    typedtree.str_items
    |> (List.fold_left (fun acc struct_item ->
        let mbs = (match struct_item.str_desc with
            | Tstr_module mb -> [mb]
            | Tstr_recmodule mbs ->  mbs
            | _ -> [])
            |> (List.concat_map (fun mb ->
                let ocaml_name = match mb.mb_id with
                                 | Some x -> (Ident.name x)
                                 | None -> ""
                in
                let prefix = module_name prefix mb.mb_id in
                match mb.mb_expr.mod_desc with
                  | Tmod_constraint ({ mod_desc = Tmod_structure typedtree },
                                     Mty_signature signature,
                                     _mod_type_constr,
                                     _mod_type_coerc) ->
                      (prefix, ocaml_name, typedtree, Some signature) :: (find_modules ~prefix typedtree)
                  | Tmod_structure typedtree -> (prefix, ocaml_name, typedtree, None) :: (find_modules ~prefix typedtree)
                  | _ -> []
                )) in
        List.concat [mbs; acc]
    ) [])


(** Turn an OCaml Typedtree into a list of Erlang ASTs that can be compiled to
    sources.
*)
let from_typedtree:
  name:string
  -> Typedtree.structure
  -> (Types.signature option)
  -> Erlast.t list =
  fun ~name typedtree signature ->
    let name = atom_of_string name in
    let modules = List.fold_left
      (fun mods (name, ocaml_name, impl, sign) ->
        (build_module ~name ~ocaml_name ~modules:mods impl sign) :: mods)
      []
      (find_modules ~prefix:name typedtree)
    in
    [ modules;
      [build_module ~name ~ocaml_name:name ~modules typedtree signature]
    ] |> List.concat
