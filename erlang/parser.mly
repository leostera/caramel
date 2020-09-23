/* The parser definition */
%{
open Ast
%}

/* Tokens */

%token DOT
%token COMMA
%token COLON_COLON
%token SLASH
%token SEMICOLON
%token LEFT_PARENS
%token RIGHT_PARENS
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_BRACE
%token RIGHT_BRACE
%token DASH
%token ARROW
%token EQUAL_ARROW
%token BINARY_OPEN
%token BINARY_CLOSE
%token <string> NUMBER
%token <string> STRING
%token <string> ATOM
%token <string> BINARY_STRING
%token <string> VARIABLE

%token EOF

/* Entry points */

%start module_file
%type <Ast.t> module_file
%%

(* An .erl file. *)
module_file: module_item* EOF {
  $1
};

module_item:
| module_attribute { Module_attribute $1 }
| type_decl { Type_decl $1 }
| function_decl { Function_decl $1 }
;

(** Module Attributes *)
name_with_arity: atom SLASH number { ($1, $3) } ;

module_attribute:
  DASH atom parens(module_attribute_value) {
    let Lit_atom atr_name = $2 in
    { atr_name; atr_value = $3 }
  }
;

module_attribute_value:
| LEFT_BRACE atom COMMA list(name_with_arity) RIGHT_BRACE {
    Expr_tuple [ Expr_literal $2; Expr_list $4 ]
  }
| list(name_with_arity) { Expr_list $1 }
| atom { Expr_literal $1 }
| list(atom) { Expr_list $1 }
;

(** Type Language *)
type_decl:
| DASH atom atom
    parens(separated_list(COMMA, VARIABLE))
    COLON_COLON type_kind {
      { typ_visibility = $2
      ; typ_name = $3
      ; typ_params = $4
      ; typ_kind = $5
      }
  }
;

type_kind:
| type_function { Type_function $1 }
| type_tuple { Type_tuple $1 }
| type_map { Type_record $1 }
| type_variant { Type_variant $1 }
;

type_expr:
| Type_function type_kind list
| Type_constr type_constr
| Type_variable atom
| Type_tuple type_kind list
| Type_record { fields : record_field list }
| Type_variant { constructors : variant_constructor list }
;

(** Function Declarations *)
function_decl:
  option(separated_list(SEMICOLON, function_clause))
  function_clause DOT
  { $1 }
;

function_clause:
  name = ATOM
  parens(separated_list(COMMA, pattern))
  ARROW
  separated_list(COMMA, expr)
  { name }
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
| atom { Pattern_match $1 }
;

(**
 * Expressions
 *)

expr:
  function_call { $1 }
| binary { $1 }
;

binary :
  BINARY_OPEN
  str = STRING
  BINARY_CLOSE
  {}
;

function_call:
  name = ATOM
  LEFT_PARENS
  binary
  RIGHT_PARENS
  {
  }
;

(**
 * Constructors
 *)
list(a):
| LEFT_BRACKET separated_list(COMMA, a) RIGHT_BRACKET { $1 }
;

tuple(a):
| LEFT_BRACE separated_list(COMMA, a) RIGHT_BRACE { $2 }
;

(**
 * Terminals
 *)

number: n = NUMBER { n };
atom: name = ATOM { name };

(**
 * Helpers
 *)
parens(t):
| LEFT_PARENS t RIGHT_PARENS { $2 }
