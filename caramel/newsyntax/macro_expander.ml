open Parsetree
open Parsetree_helper

module Error = struct
  open Sexplib.Std

  type t =
    [ `Match_error of Parsetree.expr * Parsetree.case list
    | `Macro_must_return_quoted_expressions of Parsetree.id * Parsetree.expr
    | `Unbound_macro_name of Parsetree.id ]
  [@@deriving sexp]

  let pp ppf error =
    let sexp = sexp_of_t error in
    Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

  exception Runtime_error of t

  let err e = raise (Runtime_error e)

  let match_error ~expr ~cases = err (`Match_error (expr, cases))

  let macro_must_return_quoted_expressions ~name ~macro =
    err (`Macro_must_return_quoted_expressions (name, macro))

  let unbound_macro_name id = err (`Unbound_macro_name id)
end

module Macro_env = struct
  open Sexplib.Std

  let of_parsetree parsetree =
    let tbl = Hashtbl.create 1024 in

    List.iter
      (fun item ->
        match item with
        | Str_macro ({ fn_name = Id name; _ } as macro) ->
            Hashtbl.add tbl (Id name)
              (Expr.lambda ~args:macro.fn_args ~body:macro.fn_body)
        | _ -> ())
      parsetree;

    tbl

  let find_macro ~env fn = Hashtbl.find_opt env fn
end

module Quoter = struct end

(**
   Small interpreter for a subset of Caramel that is valid for Macro expressions.
*)
module Interpreter = struct
  let rec replace ~args expr =
    match expr with
    | Expr_var name -> (
        match List.assoc_opt (Pat_bind name) args with
        | Some arg -> arg
        | None -> expr)
    | Expr_quote (Quoted_expr expr) -> Macro.quoted_expr (replace ~args expr)
    | Expr_quote _ -> expr
    | Expr_unquote expr -> replace ~args expr
    | Expr_lambda (fn_args, body) ->
        Expr.lambda ~args:fn_args ~body:(replace ~args body)
    | Expr_call (fn, fn_args) ->
        let fn = replace ~args fn in
        let fn_args = List.map (replace ~args) fn_args in
        Expr_call (fn, fn_args)
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
    | Expr_open _ | Expr_literal _ | Expr_nil -> expr

  let rec eval ~is_quoted ~env expr =
    match expr with
    | Expr_quote (Quoted_expr expr) ->
        Macro.quoted_expr (eval ~is_quoted:true ~env expr)
    | Expr_quote _ -> expr
    | Expr_unquote expr -> eval ~is_quoted:false ~env expr
    | Expr_lambda (args, body) ->
        Expr.lambda ~args ~body:(eval ~is_quoted ~env body)
    | Expr_call (fn, fn_args) -> (
        let fn_args = List.map (eval ~is_quoted ~env) fn_args in
        match fn with
        | Expr_var id when is_quoted = false -> (
            match Macro_env.find_macro ~env id with
            | Some macro ->
                let macro_call = Expr.call ~name:macro ~args:fn_args in
                eval ~is_quoted ~env macro_call
            | None -> Error.unbound_macro_name id)
        | Expr_lambda (args, body) when is_quoted = false ->
            let arg_pats = List.map (fun (_, pat) -> pat) args in
            let beta_reduced_body =
              replace ~args:(List.combine arg_pats fn_args) body
            in
            eval ~is_quoted ~env beta_reduced_body
        | _ ->
            let fn = eval ~is_quoted ~env fn in
            Expr_call (fn, fn_args))
    | Expr_match (expr, cases) ->
        if is_quoted then
          Expr.match_
            ~expr:(eval ~is_quoted ~env expr)
            ~cases:
              (List.map
                 (fun { cs_lhs; cs_rhs } ->
                   { cs_lhs; cs_rhs = eval ~is_quoted ~env cs_rhs })
                 cases)
        else eval_match ~env (eval ~is_quoted ~env expr) cases
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
    | Expr_open _ | Expr_literal _ | Expr_nil | Expr_var _ -> expr

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
end

let rec expand_expr ~env expr =
  match expr with
  (* NOTE(leandro): when we find a call that is using an identifier,
     we'll look it up as it may be a macro identifier.
  *)
  | Expr_call (Expr_var fn, args) -> (
      let args = List.map (expand_expr ~env) args in
      match Macro_env.find_macro ~env fn with
      | Some macro -> (
          (* NOTE(leandro): if we do find a macro, then we have its associated
             macro transformer function expression, so we'll throw a call to it
             into the interpreter.
          *)
          let macro_call = Expr.call ~name:macro ~args in
          let expr = Interpreter.eval ~is_quoted:false ~env macro_call in
          match expr with
          | Expr_quote (Quoted_expr expr) -> expr
          | _ -> Error.macro_must_return_quoted_expressions ~name:fn ~macro)
      | None -> expr)
  (***** recursion boilerplate :( ******)
  | Expr_lambda (args, body) -> Expr.lambda ~args ~body:(expand_expr ~env body)
  | Expr_call (fn, args) ->
      let name = expand_expr ~env fn in
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

let expand_annot ~env:_ ~annot:_ item = item
(*
  match Macro_env.find_macro ~env annot.ann_name with
  | Some macro -> (
      let inputs = annot.ann_desc in
      let macro_call = Expr.call ~name:macro ~args:[ inputs; item ] in
      let expr = Interpreter.eval ~is_quoted:false ~env macro_call in
      match expr with
      | Expr_quote expr -> expr
      | _ -> Error.macro_must_return_quoted_expressions ~name:fn ~macro)
  | None -> Error.unbound_macro_name annot.ann_name
*)

(*
  NOTE(leandro): Expand items based on annotation macros. These run _before_
  expression macros since they generate more code (types, funs, externals, etc).

*)
let expand_derive ~env item =
  let annotations = Parsetree_helper.Str.annotations item in
  let rec run_annot item annotations =
    match annotations with
    | [] -> item
    | annot :: rest -> run_annot (expand_annot ~env ~annot item) rest
  in
  run_annot item annotations

let expand ~env item =
  match expand_derive ~env item with
  | Str_fun fn -> Str_fun { fn with fn_body = expand_expr ~env fn.fn_body }
  | _ -> item

let run parsetree =
  match
    (* Build env: traverse and find all macros definitions *)
    let env = Macro_env.of_parsetree parsetree in
    (* Apply macros:
        find all macro applications,
        look em up in the environment
        use the evaluator to run them
    *)
    List.map (expand ~env) parsetree
  with
  | exception Error.Runtime_error e -> Error (`Runtime_error e)
  | parsetree -> Ok parsetree
