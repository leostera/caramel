open Core_erlang.Ast
open Lambda

exception Unsupported_top_level_lambda_value

exception Unsupported_expression

exception Unsupported_feature

exception Unsupported_primitive_operation

(*
let _debug = Format.fprintf Format.std_formatter
*)

let maybe e x = match x with Some v -> v | None -> raise e

let maybe_unsupported x = maybe Unsupported_feature x

let safe_atom u = match u.[0] with 'a' .. 'z' -> u | _ -> "'" ^ u ^ "'"

let rec varname_of_string s =
  let name = s |> String.capitalize_ascii in
  match (name.[0], name) with
  | '_', name when name <> "_" ->
      let name =
        name |> String.to_seq |> List.of_seq |> List.tl |> List.to_seq
        |> String.of_seq
      in
      "_" ^ varname_of_string name
  | _, _ -> name

let varname_of_ident i = i |> Ident.name |> varname_of_string

let atom_of_string = String.lowercase_ascii

let atom_of_ident i = i |> Ident.name |> atom_of_string

let is_primop = function Praise _ -> true | _ -> false

(* Build an Core_erlang.Ast.Exp_primop from a Lambda.Lprim *)
let lambda_prim_to_primop prim =
  match prim with _ -> raise Unsupported_primitive_operation

let lambda_prim_to_fun prim =
  match prim with
  | Paddint -> Expr_literal (Lit_atom "+")
  | _ -> raise Unsupported_primitive_operation

(* Build an arbitrary Core_erlang.Ast.expr out of a Lambda.lambda *)
let rec mk_expr : Lambda.lambda -> Core_erlang.Ast.expr =
 fun lambda ->
  match lambda with
  (* Some LPrim values are actually Core Erlang primops, some other
   * are just qualified calls to the erlang module *)
  | Lprim (prim, args, _) when is_primop prim ->
      let pop_name = lambda_prim_to_primop prim in
      let pop_args = args |> List.map mk_expr in
      Core_erlang.Ast.Expr_primop { pop_name; pop_args }
  | Lprim (prim, args, _) ->
      let qc_fun = lambda_prim_to_fun prim in
      let qc_args = args |> List.map mk_expr in
      let qc_mod = Core_erlang.Ast.Expr_literal (Lit_atom "erlang") in
      Core_erlang.Ast.Expr_qualified_call { qc_mod; qc_fun; qc_args }
  | Lvar id -> Core_erlang.Ast.Expr_var (varname_of_ident id)
  | _ -> raise Unsupported_expression

(* Build a Core_erlang.Ast.fun_expr out of a Lambda.lambda *)
let mk_fun_expr : Lambda.lambda -> Core_erlang.Ast.fun_expr =
 fun lambda ->
  match lambda with
  | Lfunction { params; body; _ } ->
      let fe_arity = params |> List.length in
      let fe_vars = params |> List.map (fun (id, _) -> varname_of_ident id) in
      let fe_body = mk_expr body in
      Core_erlang.Ast.{ fe_arity; fe_vars; fe_body }
  | _ -> raise Unsupported_top_level_lambda_value

(** Build one Core Erlang definition out of Lambda.lambda term
 *)
let mk_definition : Lambda.lambda -> Core_erlang.Ast.fun_def =
 fun lambda ->
  match lambda with
  | Llet (_let_kind, _value_kind, id, body, _next) ->
      let fd_body = mk_fun_expr body in
      let fd_name =
        { fn_name = atom_of_ident id; fn_arity = fd_body.fe_arity }
      in
      Core_erlang.Ast.{ fd_name; fd_body }
  | _ -> raise Unsupported_top_level_lambda_value

(** Build Core Erlang definitions out of Lambda.lambda term
 *)
let mk_definitions : Lambda.lambda -> Core_erlang.Ast.fun_def list =
 fun lambda ->
  let extract l = match l with Lprim (_, lambda, _) -> lambda | _ -> [] in

  extract lambda |> List.map mk_definition

(** Turn an OCaml Lambda program into Core Erlang
 *)
let from_lambda : module_name:string -> Lambda.lambda -> Core_erlang.Ast.t =
 fun ~module_name lambda ->
  let module_name = atom_of_string module_name in
  let m_defs = mk_definitions lambda in
  let m_fnames = m_defs |> List.map (fun { fd_name; _ } -> fd_name) in
  {
    m_filename = module_name ^ ".core";
    m_name = module_name;
    m_attributes = [];
    m_fnames;
    m_defs;
  }
