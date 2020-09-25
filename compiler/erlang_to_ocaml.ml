open Erlang.Ast
open Ast_helper
open Asttypes

exception Cannot_join_no_patterns
exception Unsupported_constant
exception Unsupported_pattern
exception Unsupported_expression
exception Unsupported_name of Erlang.Ast.name
exception Invalid_name of Erlang.Ast.name

let mk_lid name = Location.{ txt = Longident.Lident name; loc = Location.none }

let mk_label name = name

(* Well Known Names *)
module Wk = struct
  let cons = mk_lid "::"
end

let mk_constant lit =
  match lit with
  | Lit_integer int -> Const.integer int
  | Lit_char char -> Const.char (String.get char 0)
  | Lit_binary bin -> Const.string bin
  | Lit_float float -> Const.float float
  | Lit_atom _ -> raise Unsupported_constant

let mk_name name =
  match name with
  | Macro_name _ -> raise (Unsupported_name name)
  | Var_name name -> Exp.ident (mk_lid (String.lowercase_ascii name))
  | Atom_name name -> Exp.ident (mk_lid name)
  | Qualified_name {n_mod; n_name} ->
      match Longident.unflatten [String.capitalize_ascii n_mod; n_name] with
      | None -> raise (Invalid_name name)
      | Some t -> Exp.ident Location.{ txt = t; loc = Location.none }

let rec mk_expression expr =
  match expr with
  | Expr_fun_ref name -> Exp.ident (mk_lid name)
  | Expr_name name -> mk_name name
  | Expr_literal (Lit_atom atom) -> Exp.variant (mk_label atom) None
  | Expr_literal literal -> Exp.constant (mk_constant literal)
  | Expr_let ({lb_lhs; lb_rhs}, expr) ->
      Exp.let_
        Nonrecursive
        [Vb.mk (mk_pattern lb_lhs) (mk_expression lb_rhs)]
        (mk_expression expr)
  | Expr_apply {fa_name; fa_args} ->
      Exp.apply (mk_expression fa_name)
        (List.map (fun arg -> (Nolabel, mk_expression arg)) fa_args)
  | Expr_list xs -> mk_list_expr (List.map mk_expression xs)
  | Expr_cons (lhs, xs) -> mk_list_expr ((List.map mk_expression lhs) @ [mk_expression xs])
  | Expr_case (expr, branches) ->
      Exp.match_ (mk_expression expr) (List.map mk_match_case branches)
  | Expr_tuple els -> Exp.tuple (List.map mk_expression els)
  | Expr_fun fun_decl -> mk_fun fun_decl
  | Expr_recv recv -> mk_recv recv
  | _ -> raise Unsupported_expression

and mk_recv { rcv_cases; _ } =
  Exp.match_
    (Exp.apply
      (Exp.ident (mk_lid "recv"))
      [ Nolabel, Exp.construct (mk_lid "()") None ])
    (List.map mk_match_case rcv_cases)

and mk_match_case { cb_pattern; cb_expr } =
  Exp.case (mk_pattern cb_pattern) (mk_expression cb_expr)

and mk_list_expr ls =
  match ls with
  | [] -> Exp.construct (mk_lid "[]") None
  | x :: xs -> Exp.construct (mk_lid "::") (Some (Exp.tuple [x; mk_list_expr xs]))

and mk_list_pat ls =
  match ls with
  | [] -> Pat.construct (mk_lid "[]") None
  | x :: xs -> Pat.construct (mk_lid "::") (Some (Pat.tuple [x; mk_list_pat xs]))

and mk_pattern pattern =
  match pattern with
  | Pattern_ignore -> Pat.any ()
  | Pattern_binding name -> Pat.var { txt = (String.lowercase_ascii name); loc = Location.none }
  | Pattern_tuple [] -> Pat.construct (mk_lid "()") None
  | Pattern_tuple ((Pattern_match (Lit_atom tag)) :: []) ->
      Pat.variant tag None
  | Pattern_tuple ((Pattern_match (Lit_atom tag)) :: pats) ->
      Pat.variant tag (Some (mk_pattern (Pattern_tuple pats)))
  | Pattern_tuple [x] -> mk_pattern x
  | Pattern_tuple pats -> Pat.tuple (List.map mk_pattern pats)
  | Pattern_list xs -> mk_list_pat (List.map mk_pattern xs)
  | Pattern_cons (lhs, xs) -> mk_list_pat ((List.map mk_pattern lhs) @ [mk_pattern xs])
  | Pattern_match (Lit_atom atom) -> Pat.variant (mk_label atom) None
  | Pattern_match literal -> Pat.constant (mk_constant literal)
  | _ -> raise Unsupported_pattern

and mk_fun_args pats =
  match pats with
  | [] -> Pat.construct (mk_lid "()") None
  | [x] -> x
  | xs -> Pat.tuple xs

and mk_fun_case { fc_lhs; fc_rhs; _ } =
  Exp.case
    (mk_fun_args (List.map mk_pattern fc_lhs))
    (mk_expression fc_rhs)

and mk_fun { fd_cases; _ } =
  Exp.function_ (List.map mk_fun_case fd_cases)

and mk_fun_value ({ fd_name; _ } as f) =
  let pat = Pat.var { txt = fd_name; loc = Location.none } in
  let expr = mk_fun f in
  Str.value Recursive [(Vb.mk pat expr)]

let to_parsetree : Erlang.Ast.t -> Parsetree.structure =
  fun { ocaml_name; functions; _ } ->
    let module_name = { txt = (Some ocaml_name); loc = Location.none } in
    let str = List.map mk_fun_value functions in
    let me = Mod.structure str in
    let mb = Mb.mk module_name me in
    let ast = [ Str.module_ mb ] in
    Ast_invariants.structure ast;
    ast
