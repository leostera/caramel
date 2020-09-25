open Typedtree
open Types

exception Function_without_body of Typedtree.expression

exception Unsupported_feature

exception Unsupported_expression

(*
let _debug = Format.fprintf Format.std_formatter
*)

let maybe e x = match x with Some v -> v | None -> raise e

let maybe_unsupported x = maybe Unsupported_feature x

let const_to_literal const =
  let open Asttypes in
  match const with
  | Const_int int -> Erlang.Ast.Lit_integer (string_of_int int)
  | Const_char char -> Erlang.Ast.Lit_char (String.make 1 char)
  | Const_string (string, _, _) -> Erlang.Ast.Lit_binary string
  | Const_float string -> Erlang.Ast.Lit_float string
  | Const_int32 int32 -> Erlang.Ast.Lit_integer (Int32.to_string int32)
  | Const_int64 int64 -> Erlang.Ast.Lit_integer (Int64.to_string int64)
  | Const_nativeint nativeint ->
      Erlang.Ast.Lit_integer (Nativeint.to_string nativeint)

(* NOTE: flatten down all the names bound in the parameters
 * to a single list that we can use to quickly check how to
 * normalize them in the function's body.
 *)
let rec collect_var_names pat =
  let open Erlang.Ast in
  let rec collect acc p =
    match p with
    | [] -> acc
    | p :: ps ->
        let subpats =
          match p with
          | Pattern_list pats -> collect_var_names pats
          | Pattern_tuple pats -> collect_var_names pats
          | Pattern_map pats -> pats |> List.map (fun (_, p) -> p)
          | _ -> [ p ]
        in
        collect (subpats @ acc) ps
  in
  collect [] pat

let name_in_var_names ~var_names name =
  let open Erlang.Ast in
  var_names
  |> List.exists (fun pat ->
         match pat with Pattern_binding x -> x = name | _ -> false)

let is_nested_module ~modules name =
  let name = name |> Longident.flatten |> List.hd in
  modules |> List.exists (fun Erlang.Ast.{ ocaml_name = mn; _ } -> mn = name)

let rec build_function fd_name cases ~spec ~var_names ~modules ~module_name =
  let rec params c acc =
    let acc' = build_pattern c.c_lhs :: acc in
    match c.c_rhs.exp_desc with
    | Texp_function { cases = [ c' ]; _ } -> params c' acc'
    | _ -> acc' |> List.rev
  in

  let fd_arity =
    match cases with [] -> 0 | c :: _ -> params c [] |> List.length
  in
  let fd_cases =
    cases
    |> List.map (fun case ->
           (* NOTE: we'll just traverse all the expressions in this case and
            * make sure we collapse as many top-level arguments for this function.
            *)
           let rec body c var_names =
             match c.c_rhs.exp_desc with
             | Texp_function { cases = [ c' ]; _ } ->
                 let pattern = build_pattern c'.c_lhs in
                 let var_names = var_names @ collect_var_names [ pattern ] in
                 body c' var_names
             | _ -> (
                 match build_expression c.c_rhs ~var_names ~modules ~module_name with
                 | Some exp -> exp
                 | _ -> raise (Function_without_body c.c_rhs) )
           in

           let fc_name = fd_name in
           let fc_lhs = params case [] in
           let fc_rhs = body case (var_names @ collect_var_names fc_lhs) in
           let fc_guards = [] in
           Erlang.Ast.{ fc_name; fc_lhs; fc_guards; fc_rhs })
  in
  Some Erlang.Ast.{ fd_name; fd_arity; fd_cases; fd_spec = spec }

(* NOTE: We need a universally quantified k here because this function will
 * be called with several types indexing general_pattern *)
and build_pattern : type k. k general_pattern -> Erlang.Ast.pattern =
 fun pat ->
  match pat.pat_desc with
  | Tpat_var (id, _) ->
      let var_name = id |> Names.varname_of_ident in
      Erlang.Ast.Pattern_binding var_name
  | Tpat_value t ->
      (* NOTE: type casting magic! *)
      build_pattern (t :> pattern)
  | Tpat_tuple tuples ->
      Erlang.Ast.Pattern_tuple (List.map build_pattern tuples)
  | Tpat_record (fields, _) ->
      Erlang.Ast.Pattern_map
        ( fields
        |> List.map (fun (Asttypes.{ txt; _ }, _, pattern) ->
               (Names.longident_to_atom txt, build_pattern pattern)) )
  | Tpat_construct ({ txt; _ }, _, _) when Names.longident_to_atom txt = "()" ->
      Erlang.Ast.Pattern_tuple []
  | Tpat_construct ({ txt; _ }, _, patterns) when Names.longident_to_atom txt = "::"
    ->
      Erlang.Ast.Pattern_list (List.map build_pattern patterns)
  | Tpat_construct ({ txt; _ }, _, []) ->
      Erlang.Ast.Pattern_match (Erlang.Ast.Lit_atom (Names.longident_to_atom txt))
  | Tpat_construct ({ txt; _ }, _, patterns) ->
      let tag =
        Erlang.Ast.Pattern_match (Erlang.Ast.Lit_atom (Names.longident_to_atom txt))
      in
      let values = List.map build_pattern patterns in
      Erlang.Ast.Pattern_tuple (tag :: values)
  | Tpat_variant (label, None, _) ->
      Erlang.Ast.Pattern_match (Erlang.Ast.Lit_atom (Names.atom_of_string label))
  | Tpat_variant (label, Some expr, _) ->
      let tag =
        Erlang.Ast.Pattern_match (Erlang.Ast.Lit_atom (Names.atom_of_string label))
      in
      let value = build_pattern expr in
      Erlang.Ast.Pattern_tuple [ tag; value ]
  | Tpat_constant const -> Erlang.Ast.Pattern_match (const_to_literal const)
  (* NOTE: here's where the translation of pattern
   * matching at the function level should happen. *)
  | _ -> Erlang.Ast.Pattern_ignore

and build_bindings vbs ~var_names ~modules ~module_name =
  match vbs with
  | [ vb ] ->
      let lb_lhs = build_pattern vb.vb_pat in
      let lb_rhs =
        build_expression vb.vb_expr ~var_names ~modules ~module_name |> maybe_unsupported
      in
      let lb_rhs =
        match lb_rhs with
        | Erlang.Ast.Expr_let ({ lb_lhs = Erlang.Ast.Pattern_ignore; _ }, _) ->
            Erlang.Ast.Expr_apply
              {
                fa_name =
                  Erlang.Ast.Expr_fun
                    {
                      fd_spec = None;
                      fd_name = "anonymous";
                      fd_arity = 0;
                      fd_cases =
                        [
                          {
                            fc_name = "anonymous";
                            fc_lhs = [];
                            fc_guards = [];
                            fc_rhs = lb_rhs;
                          };
                        ];
                    };
                fa_args = [];
              }
        | _ -> lb_rhs
      in
      Erlang.Ast.{ lb_lhs; lb_rhs }
  | _ ->
      Format.fprintf Format.std_formatter
        "Caramel does not support \"let and\" bindings!\n";
      raise Unsupported_feature

and build_expression exp ~var_names ~modules ~module_name =
  match exp.exp_desc with
  | Texp_constant constant ->
      let v = const_to_literal constant in
      Some (Erlang.Ast.Expr_literal v)
  | Texp_ident (_, { txt; _ }, _) when Names.longident_to_atom txt = "__MODULE__" ->
      Some (Erlang.Ast.Expr_name (Erlang.Ast.Macro_name "MODULE"))
  | Texp_ident (_, { txt; _ }, _) -> (
      let name = Names.longident_to_name txt in
      let var_name = txt |> Names.longident_to_atom |> Names.varname_of_string in

      (* NOTE: an identifier may be a currently bound variable name *)
      if name_in_var_names ~var_names var_name then
        Some (Erlang.Ast.Expr_name (Erlang.Ast.Var_name var_name))
        (* NOTE: or it may be a function name of 3 kinds *)
      else
        match (name, is_nested_module ~modules txt) with
        (* NOTE: qualified and local, refering a module that's nested *)
        | Erlang.Ast.Qualified_name { n_mod; n_name }, true ->
            let name =
              Erlang.Ast.Qualified_name
                { n_name; n_mod = module_name ^ "__" ^ n_mod }
            in
            Some (Erlang.Ast.Expr_name name)
        (* NOTE: qualified and external, refering to a module that is not nested *)
        | Erlang.Ast.Qualified_name _, false -> Some (Erlang.Ast.Expr_name name)
        (* NOTE: unqualified, and thus refering to a function reference *)
        | _, _ -> Some (Erlang.Ast.Expr_fun_ref (Names.atom_of_string var_name)) )
  | Texp_construct ({ txt; _ }, _, _expr) when Names.longident_to_atom txt = "[]" ->
      Some (Erlang.Ast.Expr_list [])
  | Texp_construct ({ txt; _ }, _, _expr) when Names.longident_to_atom txt = "()" ->
      Some (Erlang.Ast.Expr_tuple [])
  | Texp_construct ({ txt; _ }, _, []) ->
      Some (Erlang.Ast.Expr_name (Atom_name (Names.longident_to_atom txt)))
  (* NOTE: lists are just variants :) *)
  | Texp_construct ({ txt; _ }, _, exprs) when Names.longident_to_atom txt = "::" ->
      let values =
        exprs |> List.filter_map (build_expression ~var_names ~modules ~module_name)
      in
      Some (Erlang.Ast.Expr_list values)
  (* NOTE: these are actually the variants! and Texp_variant below is for
   * polymorphic ones *)
  | Texp_construct ({ txt; _ }, _, exprs) ->
      let tag = Erlang.Ast.Expr_name (Atom_name (Names.longident_to_atom txt)) in
      let values =
        exprs |> List.filter_map (build_expression ~var_names ~modules ~module_name)
      in
      Some (Erlang.Ast.Expr_tuple (tag :: values))
  | Texp_variant (label, None) ->
      Some (Erlang.Ast.Expr_name (Erlang.Ast.Atom_name label))
  | Texp_variant (label, Some expr) ->
      let tag = Erlang.Ast.Expr_name (Erlang.Ast.Atom_name label) in
      let value =
        build_expression ~var_names ~modules ~module_name expr |> maybe_unsupported
      in
      Some (Erlang.Ast.Expr_tuple [ tag; value ])
  | Texp_apply (expr, args) ->
      let fa_name =
        match
          build_expression expr ~var_names ~modules ~module_name |> maybe_unsupported
        with
        | Erlang.Ast.Expr_fun_ref n ->
            Erlang.Ast.Expr_name (n |> Names.ocaml_to_erlang_primitive_op)
        | x -> x
      in
      let fa_args =
        args
        |> List.map (fun (_, arg) ->
               arg |> maybe_unsupported
               |> build_expression ~var_names ~modules ~module_name 
               |> maybe_unsupported)
      in
      Some (Erlang.Ast.Expr_apply { fa_name; fa_args })
  (* NOTE: use `extended_expression` to provide map overrides *)
  | Texp_record { fields; _ } ->
      Some
        (Erlang.Ast.Expr_map
           ( fields |> Array.to_list
           |> List.map (fun (field, value) ->
                  let value =
                    match value with
                    | Kept _ ->
                        Format.fprintf Format.std_formatter
                          "record overrides unsupported yet!";
                        raise Unsupported_feature
                    | Overridden (_, exp) -> (
                        match build_expression exp ~var_names ~modules ~module_name with
                        | None -> raise Unsupported_feature
                        | Some v -> v )
                  in
                  Erlang.Ast.{ mf_name = field.lbl_name; mf_value = value }) ))
  | Texp_field (expr, _, { lbl_name; _ }) ->
      let fa_name =
        let n_mod = "maps" in
        let n_name = "get" in
        Erlang.Ast.Expr_name (Qualified_name { n_mod; n_name })
      in
      let fa_args =
        [
          Erlang.Ast.Expr_name (Atom_name lbl_name);
          build_expression ~var_names ~modules ~module_name expr |> maybe_unsupported;
        ]
      in
      Some (Erlang.Ast.Expr_apply { fa_name; fa_args })
  | Texp_tuple exprs ->
      Some
        (Erlang.Ast.Expr_tuple
           (exprs |> List.filter_map (build_expression ~var_names ~modules ~module_name)))
  | Texp_match (expr, branches, _) ->
      let expr =
        build_expression expr ~var_names ~modules ~module_name |> maybe_unsupported
      in
      (* NOTE: match on c_guard here to translate guards *)
      let branches : Erlang.Ast.case_branch list =
        branches
        |> List.map (fun c ->
               let cb_pattern = build_pattern c.c_lhs in
               let var_names = collect_var_names [ cb_pattern ] @ var_names in
               let cb_expr =
                 build_expression c.c_rhs ~var_names ~modules ~module_name
                 |> maybe_unsupported
               in
               Erlang.Ast.{ cb_pattern; cb_expr })
      in
      Some (Erlang.Ast.Expr_case (expr, branches))
  | Texp_ifthenelse (if_cond, if_true, if_false) ->
      let expr =
        build_expression ~var_names ~modules ~module_name if_cond |> maybe_unsupported
      in
      let if_true =
        Erlang.Ast.
          {
            cb_pattern = Erlang.Ast.Pattern_match (Erlang.Ast.Lit_atom "true");
            cb_expr =
              build_expression ~var_names ~modules ~module_name if_true |> maybe_unsupported;
          }
      in
      let if_false =
        match if_false with
        | Some if_false ->
            [
              Erlang.Ast.
                {
                  cb_pattern =
                    Erlang.Ast.Pattern_match (Erlang.Ast.Lit_atom "false");
                  cb_expr =
                    build_expression ~var_names ~modules ~module_name if_false
                    |> maybe_unsupported;
                };
            ]
        | None -> []
      in
      let branches = if_true :: if_false in
      Some (Erlang.Ast.Expr_case (expr, branches))
  | Texp_let (_, vbs, expr) ->
      (* NOTE: consider flattening let-ins ?
         let rec flatten e acc =
           match e with
           | Texp_let (_, vbs, e') -> flatten e' (e :: acc)
           | _ -> (e :: acc) |> List.rev
         in
         let bindings = flatten expr [] in
      *)
      let let_binding = build_bindings vbs ~var_names ~modules ~module_name in
      let var_names =
        collect_var_names Erlang.Ast.[ let_binding.lb_lhs ] @ var_names
      in
      let let_expr =
        build_expression ~var_names ~modules ~module_name expr |> maybe_unsupported
      in
      Some (Erlang.Ast.Expr_let (let_binding, let_expr))
  | Texp_function { cases; _ } -> (
      match build_function ~module_name ~modules ~spec:None ~var_names "anonymous" cases with
      | Some f -> Some (Erlang.Ast.Expr_fun f)
      | None -> None )
  | Texp_sequence (this, next) ->
      let let_binding =
        Erlang.Ast.
          {
            lb_lhs = Erlang.Ast.Pattern_ignore;
            lb_rhs =
              build_expression this ~var_names ~modules ~module_name |> maybe_unsupported;
          }
      in
      let let_expr =
        build_expression ~var_names ~modules ~module_name next |> maybe_unsupported
      in
      Some (Erlang.Ast.Expr_let (let_binding, let_expr))
  | _ -> raise Unsupported_expression

let rec build_type_kind : Types.type_expr -> Erlang.Ast.type_kind =
 fun type_expr ->
  match type_expr.desc with
  | Tarrow (_, param, out, _) -> (
      let rec args t acc =
        match t.desc with
        | Tarrow (_, p, t', _) -> args t' (build_type_kind p :: acc)
        | _ -> List.rev (build_type_kind t :: acc)
      in
      let args = args out [ build_type_kind param ] in
      match args with
      | _ :: xs -> Erlang.Ast.Type_function xs
      | _ -> Erlang.Ast.Type_function args )
  | Tconstr (p, args, _) ->
      let tc_name = Names.path_to_type_name p in
      let tc_args = args |> List.map build_type_kind in
      Erlang.Ast.Type_constr { tc_name; tc_args }
  | Ttuple els ->
      let parts = els |> List.map build_type_kind in
      Erlang.Ast.Type_tuple parts
  | Tlink t -> build_type_kind (Btype.repr t)
  | Tvar (Some name) -> Erlang.Ast.Type_variable name
  | Tvar None -> Erlang.Ast.Type_variable "_"
  | Tnil
  | Tfield (_, _, _, _)
  | Tsubst _ | Tunivar _
  | Tpoly (_, _)
  | Tvariant _ | Tobject _ | Tpackage _ ->
      raise Unsupported_feature

let build_spec :
    Ident.t -> Types.value_description -> Erlang.Ast.type_kind option =
 fun id ({ val_type; _ } as vd) ->
  try Some (build_type_kind val_type)
  with _ ->
    Format.fprintf Format.std_formatter "Unsupported Type Expression:\n";
    Printtyp.value_description id Format.std_formatter vd;
    None

let find_spec : typedtree:Typedtree.structure -> string -> Erlang.Ast.type_kind option =
  fun ~typedtree name ->
  match
    List.filter_map
      (function
        | Types.Sig_value (id, vd, _) -> (
            let type_name = Ident.name id in
            match type_name = name with
            | true -> build_spec id vd
            | false -> None )
        | _ -> None)
      typedtree.str_type
  with
  | [ x ] -> Some x
  | _ -> None

let build_value vb ~modules ~module_name ~typedtree =
  match (vb.vb_pat.pat_desc, vb.vb_expr.exp_desc) with
  | Tpat_var (id, _), Texp_function { cases; _ } ->
      let id = id |> Names.atom_of_ident in
      build_function ~module_name ~modules ~spec:(find_spec ~typedtree id) ~var_names:[] id cases
  | _ -> None

(** Build the actual functions of an Erlang module
 *)
let build_functions :
    module_name:string ->
    modules:Erlang.Ast.t list ->
    Typedtree.structure ->
    Erlang.Ast.fun_decl list =
 fun ~module_name ~modules typedtree ->
  typedtree.str_items
  |> List.fold_left
       (fun acc item ->
         match item.str_desc with
         | Tstr_value (_, vb) -> List.filter_map (build_value ~typedtree ~modules ~module_name) vb @ acc
         | _ -> acc)
       []
  |> List.rev
