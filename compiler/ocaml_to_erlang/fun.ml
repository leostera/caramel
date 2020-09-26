open Erlang.Ast_helper
open Typedtree
open Types

exception Function_without_body of Typedtree.expression

exception Unsupported_expression

(*
let _debug = Format.fprintf Format.std_formatter
*)

let maybe e x = match x with Some v -> v | None -> raise e

let maybe_unsupported x = maybe Error.Unsupported_feature x

let const_to_literal const =
  let open Asttypes in
  match const with
  | Const_int int -> Const.integer (string_of_int int)
  | Const_char char -> Const.char (String.make 1 char)
  | Const_string (string, _, _) -> Const.binary string
  | Const_float string -> Const.float string
  | Const_int32 int32 -> Const.integer (Int32.to_string int32)
  | Const_int64 int64 -> Const.integer (Int64.to_string int64)
  | Const_nativeint nint -> Const.integer (Nativeint.to_string nint)

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
  List.exists
    (fun Erlang.Ast.{ module_name = mn; _ } -> Atom.equal mn name)
    modules

let rec build_function name cases ~spec ~var_names ~modules ~module_name =
  (* NOTE: helper function to collect all parameters *)
  let rec params c acc =
    let acc' = build_pattern c.c_lhs :: acc in
    match c.c_rhs.exp_desc with
    | Texp_function { cases = [ c' ]; _ } -> params c' acc'
    | _ -> acc' |> List.rev
  in

  let mk_case case =
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

    let lhs = params case [] in
    let rhs = body case (var_names @ collect_var_names lhs) in
    FunDecl.case ~lhs ~guard:None ~rhs
  in

  Some (FunDecl.mk ~name ~cases:(List.map mk_case cases) ~spec)

(* NOTE: We need a universally quantified k here because this function will
 * be called with several types indexing general_pattern *)
and build_pattern : type k. k general_pattern -> Erlang.Ast.pattern =
 fun pat ->
  match pat.pat_desc with
  | Tpat_var (id, _) -> Pat.bind (Names.varname_of_ident id)
  | Tpat_value t ->
      (* NOTE: type casting magic! *)
      build_pattern (t :> pattern)
  | Tpat_tuple tuples ->
      Erlang.Ast.Pattern_tuple (List.map build_pattern tuples)
  | Tpat_record (fields, _) ->
      Erlang.Ast.Pattern_map
        ( fields
        |> List.map (fun (Asttypes.{ txt; _ }, _, pattern) ->
               (Names.atom_of_longident txt, build_pattern pattern)) )
      (* FIXME: don't compare atoms like this, just refer to is_unit *)
  | Tpat_construct ({ txt; _ }, _, _)
    when Names.atom_of_longident txt = Atom.mk "()" ->
      Erlang.Ast.Pattern_tuple []
  | Tpat_construct ({ txt; _ }, _, patterns)
    when Names.atom_of_longident txt = Atom.mk "::" ->
      Erlang.Ast.Pattern_list (List.map build_pattern patterns)
  | Tpat_construct ({ txt; _ }, _, []) ->
      Erlang.Ast.Pattern_match
        (Erlang.Ast.Lit_atom (Names.atom_of_longident txt))
  | Tpat_construct ({ txt; _ }, _, patterns) ->
      let tag =
        Erlang.Ast.Pattern_match
          (Erlang.Ast.Lit_atom (Names.atom_of_longident txt))
      in
      let values = List.map build_pattern patterns in
      Erlang.Ast.Pattern_tuple (tag :: values)
  | Tpat_variant (label, None, _) -> Pat.const (Const.atom (Atom.mk label))
  | Tpat_variant (label, Some expr, _) ->
      let tag = Pat.const (Const.atom (Atom.mk label)) in
      let value = build_pattern expr in
      Pat.tuple [ tag; value ]
  | Tpat_constant const -> Erlang.Ast.Pattern_match (const_to_literal const)
  (* NOTE: here's where the translation of pattern
   * matching at the function level should happen. *)
  | _ -> Erlang.Ast.Pattern_ignore

and build_bindings vbs ~var_names ~modules ~module_name =
  match vbs with
  | [ vb ] ->
      let lb_lhs = build_pattern vb.vb_pat in
      let lb_rhs =
        build_expression vb.vb_expr ~var_names ~modules ~module_name
        |> maybe_unsupported
      in
      let lb_rhs =
        match lb_rhs with
        | Erlang.Ast.Expr_let ({ lb_lhs = Erlang.Ast.Pattern_ignore; _ }, _) ->
            Erlang.Ast.Expr_apply
              {
                fa_name =
                  Erlang.Ast.Expr_fun
                    [ { c_lhs = []; c_guard = None; c_rhs = lb_rhs } ];
                fa_args = [];
              }
        | _ -> lb_rhs
      in
      Erlang.Ast.{ lb_lhs; lb_rhs }
  | _ ->
      Format.fprintf Format.std_formatter
        "Caramel does not support \"let and\" bindings!\n";
      raise Error.Unsupported_feature

and build_expression exp ~var_names ~modules ~module_name =
  match exp.exp_desc with
  | Texp_constant constant ->
      let v = const_to_literal constant in
      Some (Erlang.Ast.Expr_literal v)
  | Texp_ident (_, { txt; _ }, _) ->
      let name = Names.name_of_longident txt in
      let var_name = Names.varname_of_longident txt in

      let namespace_qualified_name n_mod n_name =
        let module_name = Atom.concat module_name n_mod in
        match is_nested_module ~modules module_name with
        | true -> Name.qualified ~module_name n_name
        | _ -> name
      in

      (* NOTE: an identifier may be a currently bound variable name or it may be a function name of 3 kinds:
         1. qualified and local, referring to a nested module
         2. qualified and external, refering to a module that is not nested
         3. unqualified, and thus refering to a function reference
      *)
      Some
        ( if name_in_var_names ~var_names var_name then Expr.ident var_name
        else
          match name with
          | Erlang.Ast.Qualified_name { n_mod; n_name } ->
              Expr.ident (namespace_qualified_name n_mod n_name)
          | _ -> Expr.fun_ref ~arity:0 (Names.atom_of_longident txt) )
  | Texp_construct ({ txt; _ }, _, _expr)
    when Names.atom_of_longident txt = Atom.mk "[]" ->
      Some (Erlang.Ast.Expr_list [])
  | Texp_construct ({ txt; _ }, _, _expr)
    when Names.atom_of_longident txt = Atom.mk "()" ->
      Some (Erlang.Ast.Expr_tuple [])
  | Texp_construct ({ txt; _ }, _, []) ->
      Some (Erlang.Ast.Expr_name (Atom_name (Names.atom_of_longident txt)))
  (* NOTE: lists are just variants :) *)
  | Texp_construct ({ txt; _ }, _, exprs)
    when Names.atom_of_longident txt = Atom.mk "::" ->
      let values =
        exprs
        |> List.filter_map (build_expression ~var_names ~modules ~module_name)
      in
      Some (Erlang.Ast.Expr_list values)
  (* NOTE: these are actually the variants! and Texp_variant below is for
   * polymorphic ones *)
  | Texp_construct ({ txt; _ }, _, exprs) ->
      let tag =
        Erlang.Ast.Expr_name (Atom_name (Names.atom_of_longident txt))
      in
      let values =
        exprs
        |> List.filter_map (build_expression ~var_names ~modules ~module_name)
      in
      Some (Expr.tuple (tag :: values))
  | Texp_variant (label, None) -> Some (Expr.ident (Name.atom label))
  | Texp_variant (label, Some expr) ->
      let tag = Expr.ident (Name.atom label) in
      let value =
        build_expression ~var_names ~modules ~module_name expr
        |> maybe_unsupported
      in
      Some (Erlang.Ast.Expr_tuple [ tag; value ])
  | Texp_apply (expr, args) ->
      let name =
        match
          build_expression expr ~var_names ~modules ~module_name
          |> maybe_unsupported
        with
        | Erlang.Ast.Expr_fun_ref { fref_name = n; _ } ->
            Expr.ident (Names.ocaml_to_erlang_primitive_op (Atom.to_string n))
        | x -> x
      in
      let args =
        args
        |> List.map (fun (_, arg) ->
               arg |> maybe_unsupported
               |> build_expression ~var_names ~modules ~module_name
               |> maybe_unsupported)
      in
      Some (Expr.apply name args)
  (* NOTE: use `extended_expression` to provide map overrides *)
  | Texp_record { fields; _ } ->
      Some
        (Expr.map
           ( fields |> Array.to_list
           |> List.map (fun (field, value) ->
                  let value =
                    match value with
                    | Kept _ ->
                        Format.fprintf Format.std_formatter
                          "record overrides unsupported yet!";
                        raise Error.Unsupported_feature
                    | Overridden (_, exp) -> (
                        match
                          build_expression exp ~var_names ~modules ~module_name
                        with
                        | None -> raise Error.Unsupported_feature
                        | Some v -> v )
                  in
                  Erlang.Ast.
                    { mf_name = Atom.mk field.lbl_name; mf_value = value }) ))
  | Texp_field (expr, _, { lbl_name; _ }) ->
      let name =
        let module_name = Atom.mk "maps" in
        let name = Atom.mk "get" in
        Expr.ident (Name.qualified ~module_name name)
      in
      let args =
        [
          Expr.ident (Name.atom lbl_name);
          build_expression ~var_names ~modules ~module_name expr
          |> maybe_unsupported;
        ]
      in
      Some (Expr.apply name args)
  | Texp_tuple exprs ->
      Some
        (Erlang.Ast.Expr_tuple
           ( exprs
           |> List.filter_map
                (build_expression ~var_names ~modules ~module_name) ))
  | Texp_match (expr, branches, _) ->
      let expr =
        build_expression expr ~var_names ~modules ~module_name
        |> maybe_unsupported
      in
      (* NOTE: match on c_guard here to translate guards *)
      let branches =
        List.map
          (fun c ->
            let lhs = build_pattern c.c_lhs in
            let var_names = collect_var_names [ lhs ] @ var_names in
            let rhs =
              build_expression c.c_rhs ~var_names ~modules ~module_name
              |> maybe_unsupported
            in
            FunDecl.case ~lhs:[ lhs ] ~guard:None ~rhs)
          branches
      in
      Some (Erlang.Ast.Expr_case (expr, branches))
  | Texp_ifthenelse (if_cond, if_true, if_false) ->
      let expr =
        build_expression ~var_names ~modules ~module_name if_cond
        |> maybe_unsupported
      in
      let if_true =
        FunDecl.case
          ~lhs:[ Pat.const (Const.atom (Atom.mk "true")) ]
          ~guard:None
          ~rhs:
            ( build_expression ~var_names ~modules ~module_name if_true
            |> maybe_unsupported )
      in
      let if_false =
        match if_false with
        | Some if_false ->
            [
              FunDecl.case
                ~lhs:[ Pat.const (Const.atom (Atom.mk "false")) ]
                ~guard:None
                ~rhs:
                  ( build_expression ~var_names ~modules ~module_name if_false
                  |> maybe_unsupported );
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
        build_expression ~var_names ~modules ~module_name expr
        |> maybe_unsupported
      in
      Some (Erlang.Ast.Expr_let (let_binding, let_expr))
  | Texp_function { cases; _ } -> (
      match
        build_function ~module_name ~modules ~spec:None ~var_names
          (Atom.mk "anonymous") cases
      with
      | Some f -> Some (Expr.fun_ ~cases:f.fd_cases)
      | None -> None )
  | Texp_sequence (this, next) ->
      let let_binding =
        Erlang.Ast.
          {
            lb_lhs = Erlang.Ast.Pattern_ignore;
            lb_rhs =
              build_expression this ~var_names ~modules ~module_name
              |> maybe_unsupported;
          }
      in
      let let_expr =
        build_expression ~var_names ~modules ~module_name next
        |> maybe_unsupported
      in
      Some (Erlang.Ast.Expr_let (let_binding, let_expr))
  | _ -> raise Unsupported_expression

let build_value vb ~modules ~module_name ~typedtree =
  match (vb.vb_pat.pat_desc, vb.vb_expr.exp_desc) with
  | Tpat_var (id, _), Texp_function { cases; _ } ->
      let id = id |> Names.atom_of_ident in
      build_function ~module_name ~modules
        ~spec:(Typespecs.Fun.find_spec ~typedtree id)
        ~var_names:[] id cases
  | _ -> None

(** Build the actual functions of an Erlang module
 *)
let build_functions :
    module_name:Erlang.Ast.atom ->
    modules:Erlang.Ast.t list ->
    Typedtree.structure ->
    Erlang.Ast.fun_decl list =
 fun ~module_name ~modules typedtree ->
  typedtree.str_items
  |> List.fold_left
       (fun acc item ->
         match item.str_desc with
         | Tstr_value (_, vb) ->
             List.filter_map (build_value ~typedtree ~modules ~module_name) vb
             @ acc
         | _ -> acc)
       []
  |> List.rev
