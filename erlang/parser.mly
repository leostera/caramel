/* The parser definition */
%{
open Ast

type parse_error =
  | Unknown_type_visibility of string
  | Functions_must_have_clauses
  | Expression_is_invalid_pattern of expr

exception Parse_error of parse_error
exception Error

let throw x =
  begin match x with
  | Unknown_type_visibility v ->
      Format.fprintf Format.std_formatter "Unknown_type_visibility: %s" v
  | Functions_must_have_clauses ->
      Format.fprintf Format.std_formatter "Functions_must_have_clauses"
  | Expression_is_invalid_pattern expr ->
      Format.fprintf Format.std_formatter "Expression_is_invalid_pattern: \n";
      Printer.pp_expression "" Format.std_formatter expr ~module_:Ast.empty
  end;
  Format.fprintf Format.std_formatter "%!";
  raise (Parse_error x)

let rec expr_to_pattern expr =
  match expr with
  | Expr_name (Var_name name) -> Pattern_binding name
  | Expr_name (Atom_name atom) -> Pattern_match (Lit_atom atom)
  | Expr_literal literal -> Pattern_match literal
  | Expr_list exprs -> Pattern_list (List.map expr_to_pattern exprs)
  | Expr_tuple exprs -> Pattern_tuple (List.map expr_to_pattern exprs)
  | _ -> throw (Expression_is_invalid_pattern expr)

%}

/* Tokens */

%token <string> ATOM
%token <string> CHAR
%token <string> INTEGER
%token <string> FLOAT
%token <string> STRING
%token <string> VARIABLE
%token ARROW
%token BINARY_CLOSE
%token BINARY_OPEN
%token CASE
%token COLON
%token COLON_COLON
%token COMMA
%token DASH
%token DOT
%token END
%token EQUAL
%token FUN
%token LEFT_BRACE
%token LEFT_BRACKET
%token LEFT_PARENS
%token OF
%token PIPE
%token RIGHT_BRACE
%token RIGHT_BRACKET
%token RIGHT_PARENS
%token SEMICOLON
%token SLASH

%token EOF

/* Entry points */

%start module_file
%type <(Ast.t, _) result> module_file
%%

(* An .erl file. *)
let module_file :=
  is = module_item+; EOF;
  { Ast.of_module_items is }

let module_item :=
  | ~ = module_attribute; { Module_attribute module_attribute }
  | ~ = type_decl; { Type_decl type_decl }
  | ~ = fun_decl; { Function_decl fun_decl }

(** Module Attributes *)
let name_with_arity :=
  | name = ATOM; SLASH; arity = INTEGER;
    { Expr_tuple [ Expr_literal (Lit_atom name);
                   Expr_literal (Lit_integer arity)] }

let module_attribute :=
  | DASH; atr_name = ATOM; atr_value = parens(module_attribute_value); DOT;
    { { atr_name; atr_value} }

let module_attribute_value :=
  | LEFT_BRACE; name = ATOM; COMMA; els = list(name_with_arity); RIGHT_BRACE;
    { Expr_tuple [ Expr_literal (Lit_atom name); Expr_list els ] }

  | els = list(name_with_arity); { Expr_list els }

  | atom = ATOM; { Expr_literal (Lit_atom atom) }

(** Type Language *)
let type_decl :=
  | DASH;
    typ_visibility = type_visibility;
    typ_name = ATOM;
    typ_params = parlist(VARIABLE); COLON_COLON;
    typ_kind = type_kind;
    { { typ_visibility
      ; typ_name
      ; typ_params
      ; typ_kind
      } }

let type_visibility :=
  | atom = ATOM; {
    match atom with
    | "opaque" -> Opaque
    | "type" -> Visible
    | _ -> throw (Unknown_type_visibility atom)
  }

let type_kind :=
  | t = type_expr_fun ; { t }
  | t = type_expr_tuple ; { t }
  | t = type_expr_variant ; { t }

let type_expr :=
  | t = type_expr_fun; { t }
  | t = type_expr_constr; { t }
  | t = type_expr_var; { t }
  | t = type_expr_tuple; { t }

let type_expr_fun :=
  | args = parlist(type_expr); COLON_COLON; ret = type_expr;
    { Type_function (args @ [ret]) }

let type_expr_constr :=
  | tc_name = name; tc_args = parlist(type_expr);
    { Type_constr { tc_name; tc_args } }

let type_expr_var :=
  | n = VARIABLE; { Type_variable n }

let type_expr_tuple :=
  | t = tuple(type_expr); { Type_tuple t }

let type_expr_variant :=
  | constructors = separated_list(PIPE, type_expr); {
    let constructors = constructors
    |> List.map (function
      | Type_constr constr -> Constructor constr
      | x -> Extension x )
    in
    Type_variant { constructors; }
  }

(** Function Declarations *)
let fun_decl :=
  | cases = separated_list(SEMICOLON, fun_case); DOT; {
    let main, cases = match cases with
      | [] -> throw Functions_must_have_clauses
      | x :: xs -> x, xs
    in
    { fd_name = main.fc_name
    ; fd_arity = main.fc_lhs |> List.length
    ; fd_cases = main :: cases
    }
  }

let fun_case :=
  | fc_name = ATOM; fc_lhs = parlist(expr); ARROW; fc_rhs = expr; {
      { fc_name
      ; fc_lhs = List.map expr_to_pattern fc_lhs
      ; fc_guards = []
      ; fc_rhs
      }
    }

(**
 * Expressions
 *)

let expr :=
  | e = expr_apply; { e }
  | e = expr_let; { e }
  | e = expr_name; { e }
  | e = expr_literal; { e }
  | e = expr_fun_ref; { e }
  | e = expr_list; { e }
  | e = expr_case; { e }
  | e = expr_tuple; { e }
  | e = expr_fun; { e }

let expr_let :=
  | lb_lhs = expr; EQUAL; lb_rhs = expr; COMMA; next = expr;
    { Expr_let ({ lb_lhs = expr_to_pattern lb_lhs; lb_rhs}, next) }

let expr_name  :=
  | n = name; { Expr_name n }

let expr_literal :=
  | ~ = literal; { Expr_literal literal }

(* NOTE: this should be `name` instead of `atom` and the arity should be
 * captured here as well. *)
let expr_fun_ref :=
  | FUN; name = ATOM; SLASH; INTEGER; { Expr_fun_ref name }

let expr_apply :=
  | ~ = apply_name; fa_args = parlist(expr);
    { Expr_apply { fa_name = Expr_name apply_name; fa_args } }

let expr_list := l = list(expr); { Expr_list l }

let expr_tuple := t = tuple(expr); { Expr_tuple t }

let expr_case :=
  CASE; ~ = expr; OF;
  cases = separated_list(SEMICOLON, case_branch);
  END;
  { Expr_case (expr, cases) }

let case_branch :=
  pat = expr; ARROW; cb_expr = expr;
    { { cb_pattern = expr_to_pattern pat; cb_expr; } }

let expr_fun :=
  | FUN; ~ = fun_decl; END; { Expr_fun fun_decl }

(**
 * Constructors
 *)
let list(a) := LEFT_BRACKET; els = separated_list(COMMA, a); RIGHT_BRACKET; { els }
let tuple(a) := LEFT_BRACE; els = separated_list(COMMA, a); RIGHT_BRACE; { els }

(**
 * Terminals
 *)

let literal :=
  | c = CHAR; { Lit_char c }
  | BINARY_OPEN; s = STRING; BINARY_CLOSE; { Lit_binary s }
  | n = INTEGER; { Lit_integer n }
  | n = FLOAT; { Lit_float n }
  | a = ATOM; { Lit_atom a }

let name :=
  | n = VARIABLE; { Var_name n}
  | n = apply_name; { n }

let apply_name :=
  | atom = ATOM; { Atom_name atom }
  | n_mod = ATOM; COLON; n_name = ATOM; { Qualified_name { n_mod; n_name } }

(**
 * Helpers
 *)
let parens(t) := LEFT_PARENS; els = t; RIGHT_PARENS; { els }
let parlist(t) := els = parens(separated_list(COMMA, t)); { els }
