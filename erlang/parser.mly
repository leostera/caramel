/* The parser definition */
%{
open Ast

type parse_error =
  | Unknown_type_visibility of string
  | Functions_must_have_clauses 

exception Parse_error of parse_error

let throw x = raise (Parse_error x)

%}

/* Tokens */

%token ARROW
%token BINARY_CLOSE
%token BINARY_OPEN
%token CASE
%token COLON
%token EQUAL
%token COLON_COLON
%token COMMA
%token DASH
%token DOT
%token END
%token EQUAL_ARROW
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
%token <string> NUMBER
%token <string> STRING
%token <string> CHAR
%token <string> ATOM
%token <string> BINARY_STRING
%token <string> VARIABLE

%token EOF
%token EOL

/* Entry points */

%start module_file
%type <(Ast.t, _) result> module_file
%%

(* An .erl file. *)
module_file: separated_list(EOL, module_item) EOF { Ast.of_module_items $1 };

module_item:
| module_attribute { Module_attribute $1 }
| type_decl { Type_decl $1 }
| fun_decl { Function_decl $1 }
;

(** Module Attributes *)
name_with_arity:
  atom SLASH number
    { Expr_tuple [ Expr_literal (Lit_atom $1);
                   Expr_literal (Lit_integer $3)] }
;

module_attribute:
  DASH atom parens(module_attribute_value) DOT
    { { atr_name = $2 ; atr_value = $3 } }
;

module_attribute_value:
| LEFT_BRACE atom COMMA list(name_with_arity) RIGHT_BRACE
    { Expr_tuple [ Expr_literal (Lit_atom $2); Expr_list $4 ] }

| list(name_with_arity)
    { Expr_list $1 }

| atom
    { Expr_literal (Lit_atom $1) }

| list(atom)
    { Expr_list (List.map (fun x -> Expr_literal (Lit_atom x)) $1) }
;

(** Type Language *)
type_decl:
| DASH type_visibility atom parlist(VARIABLE) COLON_COLON type_kind
  {
      { typ_visibility = $2
      ; typ_name = $3
      ; typ_params = $4
      ; typ_kind = $6
      }
  }
;

type_visibility:
  atom {
    match $1 with
    | "opaque" -> Opaque
    | "type" -> Visible
    | _ -> throw (Unknown_type_visibility $1)
  }
;

type_kind:
| type_expr_fun { $1 }
| type_expr_tuple { $1 }
| type_expr_variant { $1 }
;

type_expr:
| type_expr_fun { $1 }
| type_expr_constr { $1 }
| type_expr_var { $1 }
| type_expr_tuple { $1 }
;

type_expr_fun:
  parlist(type_expr)
  COLON_COLON
  type_expr
  { Type_function ($1 @ [$3]) }
;

type_expr_constr:
  name parlist(type_expr) {
    Type_constr { tc_name = $1 ; tc_args = $2 }
  }
;

type_expr_var:
  n = VARIABLE { Type_variable n }
;

type_expr_tuple:
  tuple(type_expr) { Type_tuple $1 }
;

type_expr_variant:
  separated_list(PIPE, type_expr) {
    let constructors = $1
    |> List.map (function
      | Type_constr constr -> Constructor constr
      | x -> Extension x )
    in
    Type_variant { constructors; }
  }
;

(** Function Declarations *)
fun_decl:
  separated_list(SEMICOLON, fun_case)
  DOT
  {
    let main, cases = match $1 with
      | [] -> throw Functions_must_have_clauses
      | x :: xs -> x, xs
    in
    { fd_name = main.fc_name
    ; fd_arity = main.fc_lhs |> List.length
    ; fd_cases = main :: cases
    }
  }
;

fun_case:
  ATOM parlist(pattern) ARROW expr_let
  { { fc_name = $1
    ; fc_lhs = $2
    ; fc_guards = []
    ; fc_rhs = $4
    }
  }
;

(**
 * Patterns
 *)
pattern:
| name = VARIABLE {
    match name with
    | "_" -> Pattern_ignore
    | _ -> Pattern_binding name
  }
| tuple(pattern) {
    Pattern_tuple $1
  }
| list(pattern) { Pattern_list $1 }
| atom { Pattern_match (Lit_atom $1) }
;

(**
 * Expressions
 *)

expr:
| expr_let { $1 }
| expr_name { $1 }
| expr_literal { $1 }
| expr_fun_ref { $1 }
| expr_apply { $1 }
| expr_list { $1 }
| expr_case { $1 }
| expr_tuple { $1 }
| expr_fun { $1 }
;

expr_let:
| pattern EQUAL expr COMMA expr
    { Expr_let ({ lb_lhs = $1; lb_rhs = $3 }, $5) }
;

expr_name :
  n = name
    { Expr_name n }
;

expr_literal:
  literal
    { Expr_literal $1 }
;

(* NOTE: this should be `name` instead of `atom` and the arity should be
 * captured here as well. *)
expr_fun_ref:
  FUN atom SLASH NUMBER
    { Expr_fun_ref $2 }
;

expr_apply:
  expr parlist(expr)
    { Expr_apply { fa_name = $1; fa_args = $2 } }
;

expr_list:
  list(expr)
    { Expr_list $1 }
;

expr_tuple:
  tuple(expr)
    { Expr_tuple $1 }
;

expr_case:
  CASE expr OF
  separated_list(SEMICOLON, case_branch)
  END
  { Expr_case ($2, $4) }
;

case_branch:
  pattern ARROW expr
    { { cb_pattern = $1; cb_expr = $3 } }
;

expr_fun: fun_decl { Expr_fun $1 };

(**
 * Constructors
 *)
list(a):
  LEFT_BRACKET separated_list(COMMA, a) RIGHT_BRACKET
    { $2 }
;

tuple(a):
  LEFT_BRACE separated_list(COMMA, a) RIGHT_BRACE
    { $2 }
;

(**
 * Terminals
 *)

literal:
| c = CHAR { Lit_char c }
| BINARY_OPEN s = STRING BINARY_CLOSE { Lit_binary s }
| n = NUMBER { Lit_integer n }
| a = atom { Lit_atom a }

name:
| n = VARIABLE { Var_name n}
| atom { Atom_name $1 }
| atom COLON atom { Qualified_name { n_mod = $1; n_name = $3 } }
;

number: n = NUMBER { n };
atom: name = ATOM { name };

(**
 * Helpers
 *)
parens(t): LEFT_PARENS t RIGHT_PARENS { $2 } ;
parlist(t): parens(separated_list(COMMA, t)) { $1 };
