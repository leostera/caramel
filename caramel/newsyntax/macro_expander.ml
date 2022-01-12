open Parsetree
open Parsetree_helper

module Error = struct
  open Sexplib.Std

  type t =
    [ `Match_error of Parsetree.expr * Parsetree.case list
    | `Macro_must_return_quoted_expressions of Parsetree.expr
    | `Macro_must_return_quoted_structure_item of Parsetree.id * Parsetree.expr
    | `Derive_macro_not_found of Parsetree.id
    | `Unbound_macro_name of Parsetree.id
    | `Unbound_value_name of Parsetree.id
    | `Unbound_name_in_unquoted_expression of Parsetree.id 
    | `Field_access_error of Parsetree.id * (Parsetree.id * expr) list 
    ]
  [@@deriving sexp]

  let pp ppf error =
    let sexp = sexp_of_t error in
    Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

  exception Runtime_error of t

  let err e = raise (Runtime_error e)

  let match_error ~expr ~cases = err (`Match_error (expr, cases))

  let macro_must_return_quoted_expressions ~macro =
    err (`Macro_must_return_quoted_expressions macro)

  let unbound_macro_name id = err (`Unbound_macro_name id)

  let unbound_value_name id = err (`Unbound_value_name id)

  let unbound_name_in_unquoted_expression id =
    err (`Unbound_name_in_unquoted_expression id)

  let derive_macro_not_found name = err (`Derive_macro_not_found name)

  let macro_must_return_quoted_structure_item ~name ~macro =
    err (`Macro_must_return_quoted_structure_item (name, macro))

  let field_access_error ~field ~fields =
    err (`Field_access_error (field, fields))
end

module Env = struct
  open Sexplib.Std

  type t = {
    macros : (Parsetree.id, Parsetree.expr) Hashtbl.t;
    vars : (Parsetree.id, Parsetree.expr) Hashtbl.t;
  }

  let empty = { macros = Hashtbl.create 1024; vars = Hashtbl.create 1024 }

  let clear_names ~env = Hashtbl.clear env.vars

  let is_var ~env id = Hashtbl.mem env.vars id

  let find_var ~env id = Hashtbl.find_opt env.vars id

  let get_var ~env id =
    match find_var ~env id with
    | Some value -> value
    | None -> Error.unbound_value_name id

  (* TODO: handle extracting values when pattern matching and binding them *)
  let bind_names ~env bindings =
    List.iter
      (fun ((_, pat), value) ->
        match pat with
        | Pat_bind name -> Hashtbl.add env.vars name value
        | _ -> ())
      bindings;
    env

  let is_macro ~env id = Hashtbl.mem env.macros id

  let find_macro ~env id = Hashtbl.find_opt env.macros id

  let get_macro ~env id =
    match find_macro ~env id with
    | Some macro -> macro
    | None -> Error.unbound_macro_name id

  let bind_macros ~env parsetree =
    List.iter
      (fun item ->
        match item with
        | Str_macro ({ fn_name = Id name; _ } as macro) ->
            Hashtbl.add env.macros (Id name)
              (Expr.lambda ~args:macro.fn_args ~body:macro.fn_body)
        | _ -> ())
      parsetree;

    env
end

module Quoter = struct end

(**
   Small interpreter for a subset of Caramel that is valid for Macro expressions.
*)
module Interpreter = struct
  let rec replace ~args expr =
    match expr with
    | Expr_call (fn, fn_args) ->
        let fn = replace ~args fn in
        let fn_args = List.map (replace ~args) fn_args in
        Expr_call (fn, fn_args)
    | Expr_var name -> (
        match List.assoc_opt (Pat_bind name) args with
        | Some arg -> arg
        | None -> expr)
    | Expr_quote (Quoted_expr expr) -> Macro.quoted_expr (replace ~args expr)
    | Expr_quote _ -> expr
    | Expr_unquote expr -> replace ~args expr
    | Expr_lambda (fn_args, body) ->
        Expr.lambda ~args:fn_args ~body:(replace ~args body)
    | Expr_cons (a, b) ->
        Expr.list ~head:(replace ~args a) ~tail:(replace ~args b)
    | Expr_match (expr, cases) ->
        Expr.match_ ~expr:(replace ~args expr)
          ~cases:
            (List.map
               (fun { cs_lhs; cs_rhs } ->
                 { cs_lhs; cs_rhs = replace ~args cs_rhs })
               cases)
    | Expr_let (pat, body, expr) ->
        Expr.let_ ~pat ~body:(replace ~args body) ~expr:(replace ~args expr)
    | Expr_seq (a, b) -> Expr.seq (replace ~args a) (replace ~args b)
    | Expr_tuple parts -> Expr.tuple ~parts:(List.map (replace ~args) parts)
    | Expr_record fields ->
        Expr.record
          ~fields:(List.map (fun (k, v) -> (k, replace ~args v)) fields)
    | Expr_constructor (name, Ctr_tuple parts) ->
        Expr.constructor_tuple ~name ~parts:(List.map (replace ~args) parts)
    | Expr_constructor (name, Ctr_record fields) ->
        Expr.constructor_record ~name
          ~fields:(List.map (fun (k, v) -> (k, replace ~args v)) fields)
    | Expr_field (expr, field) ->
        Expr.field_access ~expr:(replace ~args expr) ~field
    | Expr_open _ | Expr_literal _ | Expr_nil -> expr

  (** TODO(leandro): handle evaluation of types, modules, etc *)
  let rec eval_str_item ~is_quoted ~env item =
    match item with
    | Str_fun fn ->
        Str_fun { fn with fn_body = eval ~is_quoted ~env fn.fn_body }
    | _ -> item

  and eval ~is_quoted ~env expr =
    match expr with
    (**************** macro reduction *********************************)
    (* NOTE: all evaluations should begin with an Expr_call that is NOT an id *)
    | Expr_call (Expr_lambda (args, body), fn_args) when is_quoted = false ->
        let fn_args = List.map (eval ~is_quoted ~env) fn_args in
        let bindings = List.combine args fn_args in
        let env = Env.bind_names ~env bindings in
        let result = eval ~is_quoted:false ~env body in
        Env.clear_names ~env;
        result
    | Expr_call (Expr_var id, fn_args)
      when Env.is_macro ~env id && is_quoted = false ->
        let macro = Env.get_macro ~env id in
        let macro_call = Expr.call ~name:macro ~args:fn_args in
        eval ~is_quoted:false ~env macro_call
    | Expr_call (name, args) ->
        let args = List.map (eval ~is_quoted ~env) args in
        Expr.call ~name ~args
    (**************** quotation handling *********************************)
    | Expr_quote (Quoted_expr expr) ->
        Macro.quoted_expr (eval ~is_quoted ~env expr)
    | Expr_quote (Quoted_str item) ->
        Macro.quoted_str_item (eval_str_item ~is_quoted ~env item)
    | Expr_unquote expr -> eval ~is_quoted:false ~env expr
    (**************** more reduction *******************************************)
    | Expr_var id when is_quoted = false -> Env.get_var ~env id
    | Expr_match (expr, cases) when is_quoted = false ->
        eval_match ~env (eval ~is_quoted ~env expr) cases
    | Expr_field (value, field) when is_quoted = false ->
        let value = eval ~is_quoted ~env value in
        eval_field_access value field
    (**************** recursion boilerplate :( *********************************)
    | Expr_match (expr, cases) ->
        Expr.match_
          ~expr:(eval ~is_quoted ~env expr)
          ~cases:
            (List.map
               (fun { cs_lhs; cs_rhs } ->
                 { cs_lhs; cs_rhs = eval ~is_quoted ~env cs_rhs })
               cases)
    | Expr_field (expr, field) ->
        Expr.field_access ~expr:(eval ~is_quoted ~env expr) ~field
    | Expr_lambda (args, body) ->
        Expr.lambda ~args ~body:(eval ~is_quoted ~env body)
    | Expr_cons (a, b) ->
        Expr.list ~head:(eval ~is_quoted ~env a) ~tail:(eval ~is_quoted ~env b)
    | Expr_let (pat, body, expr) ->
        Expr.let_ ~pat
          ~body:(eval ~is_quoted ~env body)
          ~expr:(eval ~is_quoted ~env expr)
    | Expr_seq (a, b) ->
        Expr.seq (eval ~is_quoted ~env a) (eval ~is_quoted ~env b)
    | Expr_tuple parts ->
        Expr.tuple ~parts:(List.map (eval ~is_quoted ~env) parts)
    | Expr_record fields ->
        Expr.record
          ~fields:(List.map (fun (k, v) -> (k, eval ~is_quoted ~env v)) fields)
    | Expr_constructor (name, Ctr_tuple parts) ->
        Expr.constructor_tuple ~name
          ~parts:(List.map (eval ~is_quoted ~env) parts)
    | Expr_constructor (name, Ctr_record fields) ->
        Expr.constructor_record ~name
          ~fields:(List.map (fun (k, v) -> (k, eval ~is_quoted ~env v)) fields)
    | Expr_var _ | Expr_open _ | Expr_literal _ | Expr_nil -> expr

  and eval_field_access value field =
    match value with
    | Expr_record fields -> (
        match List.assoc_opt field fields with
        | Some value -> value
        | None -> Error.field_access_error ~field ~fields)
    | _ -> exit 1

  and eval_match ~env expr cases =
    let rec match_ pat expr =
      match (pat, expr) with
      | Pat_bind _, _ -> true
      | Pat_any, _ -> true
      | Pat_nil, Expr_nil -> true
      | Pat_cons (h1, t1), Expr_cons (h2, t2) ->
          if match_ h1 h2 then match_ t1 t2 else false
      | Pat_tuple t1, Expr_tuple t2 ->
          let pairs = List.combine t1 t2 in
          let results = List.map (fun (pat, expr) -> match_ pat expr) pairs in
          List.for_all (fun res -> res == true) results
      | Pat_literal lit1, Expr_literal lit2 -> lit1 = lit2
      | _, _ -> false
    in

    let run_case { cs_lhs; cs_rhs } =
      if match_ cs_lhs expr then Ok cs_rhs else Error `no_match
    in

    let rec check_cases cases =
      match cases with
      | [] -> Error `unexpected_match
      | case :: rest -> (
          match run_case case with
          | Ok result -> Ok result
          | Error `no_match -> check_cases rest)
    in

    match check_cases cases with
    | Ok result -> eval ~is_quoted:false ~env result
    | Error `unexpected_match -> Error.match_error ~expr ~cases

  let reify_structure_item item =
    let name =
      match item with
      | Str_type { typ_name = Id [ str ]; _ } -> str
      | _ -> "unimplemented"
    in
    let name = Expr.lit_str name in

    let fields = [ Expr.field ~name:(Parsetree_helper.id "name") ~expr:name ] in
    Expr.record ~fields
end

let rec expand_expr ~env expr =
  match expr with
  | Expr_call (Expr_var id, args) when Env.is_macro ~env id -> (
      let top_args = List.map (expand_expr ~env) args in
      let macro = Env.get_macro ~env id in

      match macro with
      | Expr_lambda (args, body) -> (
          let bindings = List.combine args top_args in
          let env = Env.bind_names ~env bindings in
          match Interpreter.eval ~is_quoted:false ~env body with
          | Expr_quote (Quoted_expr expr) ->
              Env.clear_names ~env;
              expr
          | _ -> Error.macro_must_return_quoted_expressions ~macro)
      | _ -> Error.unbound_macro_name id)
  (***** recursion boilerplate :( ******)
  | Expr_field (expr, field) ->
      Expr.field_access ~expr:(expand_expr ~env expr) ~field
  | Expr_lambda (args, body) -> Expr.lambda ~args ~body:(expand_expr ~env body)
  | Expr_call (name, args) ->
      let args = List.map (expand_expr ~env) args in
      Expr.call ~name ~args
  | Expr_cons (a, b) ->
      Expr.list ~head:(expand_expr ~env a) ~tail:(expand_expr ~env b)
  | Expr_quote (Quoted_expr expr) -> Macro.quoted_expr (expand_expr ~env expr)
  | Expr_quote _ -> expr
  | Expr_unquote expr -> Macro.unquoted ~expr:(expand_expr ~env expr)
  | Expr_match (expr, cases) ->
      Expr.match_ ~expr:(expand_expr ~env expr)
        ~cases:
          (List.map
             (fun { cs_lhs; cs_rhs } ->
               { cs_lhs; cs_rhs = expand_expr ~env cs_rhs })
             cases)
  | Expr_let (pat, body, expr) ->
      Expr.let_ ~pat ~body:(expand_expr ~env body) ~expr:(expand_expr ~env expr)
  | Expr_seq (a, b) -> Expr.seq (expand_expr ~env a) (expand_expr ~env b)
  | Expr_tuple parts -> Expr.tuple ~parts:(List.map (expand_expr ~env) parts)
  | Expr_constructor (name, Ctr_tuple parts) ->
      Expr.constructor_tuple ~name ~parts:(List.map (expand_expr ~env) parts)
  | Expr_constructor (name, Ctr_record fields) ->
      Expr.constructor_record ~name
        ~fields:(List.map (fun (k, v) -> (k, expand_expr ~env v)) fields)
  | Expr_record fields ->
      Expr.record
        ~fields:(List.map (fun (k, v) -> (k, expand_expr ~env v)) fields)
  | Expr_open _ | Expr_literal _ | Expr_nil | Expr_var _ -> expr

let expand ~env item =
  match item with
  | Str_fun fn -> Str_fun { fn with fn_body = expand_expr ~env fn.fn_body }
  | _ -> item

module Annotation = struct
  let expand_annot ~env:_ ~annot:_ _item = []

  let expand_derive ~env ~annot item =
    let do_expand name item =
      match Env.find_macro ~env name with
      | Some macro -> (
          let item_expr = Interpreter.reify_structure_item item in
          let macro_call = Expr.call ~name:macro ~args:[ item_expr ] in
          let expr = Interpreter.eval ~is_quoted:false ~env macro_call in
          match expr with
          | Expr_quote (Quoted_str str) -> str
          | _ -> Error.macro_must_return_quoted_structure_item ~name ~macro)
      | None -> Error.unbound_macro_name name
    in

    let rec fold_derives kvs item acc =
      match kvs with
      | [] -> acc
      | (name, _) :: rest -> fold_derives rest item (do_expand name item :: acc)
    in
    let derives =
      match annot.ann_desc with Some (Map derives) -> derives | None -> []
    in

    fold_derives derives item []

  (*

  NOTE(leandro): Expand items based on annotation macros. These run _before_
  expression macros since they generate more code (types, funs, externals, etc).

*)
  let expand_annotations ~env item =
    let annotations = Parsetree_helper.Str.annotations item in

    (* NOTE: runs ONLY the derive annotation first *)
    let rec run_derive item annotations acc new_items =
      match annotations with
      | [] -> (new_items, acc)
      | ({ ann_name = Id name; _ } as annot) :: rest when name = [ "derive" ] ->
          run_derive item rest acc (expand_derive ~env ~annot item @ new_items)
      | annot :: rest -> run_derive item rest (annot :: acc) new_items
    in

    (* NOTE: runs all other annotations *)
    let rec run_annot item annotations new_items =
      match annotations with
      | [] -> new_items
      | annot :: rest ->
          run_annot item rest (expand_annot ~env ~annot item @ new_items)
    in

    let new_items, other_annotations = run_derive item annotations [] [] in
    let all_new_items = run_annot item other_annotations new_items in

    item :: all_new_items
end

let run parsetree =
  match
    (* Build env: traverse and find all macros definitions *)
    let env = Env.bind_macros ~env:Env.empty parsetree in
    (* Apply macros:
        find all macro applications,
        look em up in the environment
        use the evaluator to run them
    *)
    parsetree
    |> List.concat_map (Annotation.expand_annotations ~env)
    |> List.map (expand ~env)
  with
  | exception Error.Runtime_error e -> Error (`Runtime_error e)
  | parsetree -> Ok parsetree
