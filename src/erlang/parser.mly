/* The parser definition */
%{
open Ast
open Ast_helper

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
      Format.fprintf Format.std_formatter "Expression_is_invalid_pattern: ";
      Printer.pp_expression "" Format.std_formatter expr ~module_:Mod.empty
  end;
  Format.fprintf Format.std_formatter "\n%!";
  raise (Parse_error x)

let rec expr_to_pattern expr =
  match expr with
  | Expr_name (Var_name name) -> Pat.bind (Name.var name)
  | Expr_name (Atom_name atom) -> Pattern_match (Lit_atom atom)
  | Expr_literal literal -> Pattern_match literal
  | Expr_list exprs -> Pattern_list (List.map expr_to_pattern exprs)
  | Expr_cons (lhs, rhs) -> Pattern_cons (List.map expr_to_pattern lhs, expr_to_pattern rhs)
  | Expr_tuple exprs -> Pattern_tuple (List.map expr_to_pattern exprs)
  | _ -> throw (Expression_is_invalid_pattern expr)

%}

/* Tokens */

%token <string> ATOM
%token <string> CHAR
%token <string> FLOAT
%token <string> INTEGER
%token <string> STRING
%token <string> VARIABLE
%token AFTER
%token ARROW
%token BANG
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
%token RECEIVE
%token RIGHT_BRACE
%token RIGHT_BRACKET
%token RIGHT_PARENS
%token SEMICOLON
%token SLASH

%token EOF

/* Entry points */

%start module_file
%type <Ast.t> module_file
%%

(* An .erl file. *)
let module_file :=
  is = module_item+; EOF;
  { Mod.of_structure (List.rev is) }

let module_item :=
  | ~ = module_attribute; { Module_attribute module_attribute }
  | ~ = type_decl; { Type_decl type_decl }
  | ~ = fun_decl; { Function_decl fun_decl }

(** Module Attributes *)
let name_with_arity :=
  | name = atom; SLASH; arity = INTEGER;
    { Expr.tuple [ Expr.const (Const.atom name); Expr.const (Const.integer arity)] }

let module_attribute :=
  | DASH; atr_name = atom; atr_value = parens(module_attribute_value); DOT;
    { { atr_name; atr_value} }

let module_attribute_value :=
  | LEFT_BRACE; name = atom; COMMA; els = list(name_with_arity); RIGHT_BRACE;
    { Expr.tuple [ Expr.const (Const.atom name); els ] }

  | els = list(name_with_arity); { els }

  | atom = atom; { Expr_literal (Lit_atom atom) }

(** Type Language *)
let type_decl :=
  | DASH;
    visibility = type_visibility;
    name = atom;
    params = parlist(name); COLON_COLON;
    kind = type_kind;
    { Type.mk ~visibility ~name ~params ~kind  }

let type_visibility :=
  | atom = ATOM; {
    match atom with
    | "opaque" -> Type.opaque
    | "type" -> Type.visible
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
  | args = parlist(type_expr); COLON_COLON; return = type_expr;
    { Type.fun_ ~args ~return }

let type_expr_constr :=
  | ~ = name; args = parlist(type_expr);
    { Type.apply ~args ~name }

let type_expr_var :=
  | n = VARIABLE; { Type.var (Name.var n) }

let type_expr_tuple :=
  | t = tuple(type_expr); { Type.tuple t }

let type_expr_variant :=
  | constructors = separated_list(PIPE, type_expr); {
    constructors
    |> List.map (function
      | Type_constr { tc_args=args; tc_name=name } -> Type.constr ~args ~name
      | x -> Extension x )
    |> Type.variant
  }

(** Function Declarations *)
let fun_decl :=
  | cases = separated_list(SEMICOLON, named_fun_case); DOT; {
    let name, cases = match cases with
      | [] -> throw Functions_must_have_clauses
      | (name, _) :: _ ->
          (name, List.map (fun (_, c) -> c ) cases)
    in
    FunDecl.mk ~name ~cases ~spec:None
  }

let named_fun_case := ~ = atom; ~ = fun_case; <>

let fun_case :=
  | lhs = parlist(expr); ARROW; rhs = expr;
    { FunDecl.case ~lhs:(List.map expr_to_pattern lhs) ~guard:None ~rhs }

(**
 * Expressions
 *)

let expr :=
  | e = expr_send; { e }
  | e = expr_recv; { e }
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
    { Expr.let_  (Expr.let_bind (expr_to_pattern lb_lhs) lb_rhs) next }
  | lb_lhs = expr; EQUAL; lb_rhs = expr;
    { Expr.let_  (Expr.let_bind (expr_to_pattern lb_lhs) lb_rhs) lb_lhs }

let expr_recv :=
  | RECEIVE; cases = separated_list(SEMICOLON, case_branch); END;
    { Expr.recv ~cases ~after:None }

  | RECEIVE;
      cases = separated_list(SEMICOLON, case_branch);
    AFTER;
      after = case_branch;
    END;
    { Expr.recv ~cases ~after:(Some after) }

let expr_send :=
  | pid = expr; BANG; msg = expr;
  { let send = Expr.ident (Name.qualified ~module_name:(Atom.mk "erlang") (Atom.mk "send")) in
    Expr.apply send [ pid; msg ]
  }

let expr_name  :=
  | n = name; { Expr_name n }

let expr_literal :=
  | ~ = literal; { Expr_literal literal }

let expr_fun_ref :=
  | FUN; name = atom; SLASH; arity = INTEGER;
    { Expr.fun_ref name ~arity:(int_of_string arity) }

let expr_apply :=
  | ~ = name; fa_args = parlist(expr);
    { Expr_apply { fa_name = Expr_name name; fa_args } }

let expr_list := l = list(expr); { l }

let expr_tuple := t = tuple(expr); { Expr_tuple t }

let expr_case :=
  CASE; ~ = expr; OF;
  cases = separated_list(SEMICOLON, case_branch);
  END;
  { Expr_case (expr, cases) }

let case_branch :=
  | lhs = expr; ARROW; rhs = expr;
    { FunDecl.case ~lhs:[expr_to_pattern lhs] ~rhs ~guard:None }
  | lhs = separated_list(COMMA, expr); ARROW; rhs = expr;
    { FunDecl.case ~lhs:(List.map expr_to_pattern lhs) ~rhs ~guard:None }

let expr_fun :=
  | FUN; cases = separated_list(SEMICOLON, fun_case); END;
    { Expr.fun_ ~cases }

(**
 * Constructors
 *)
let list(a) :=
  (* NOTE: matches [1,2,3] *)
  | LEFT_BRACKET; els = separated_list(COMMA, a); RIGHT_BRACKET;
    { Expr.list els }

    (* NOTE: matches [1,2 | Rest] *)
  | LEFT_BRACKET; el1 = separated_list(COMMA, a); PIPE; el2 = a; RIGHT_BRACKET;
    { Expr.cons el1 el2 }


let tuple(a) := LEFT_BRACE; els = separated_list(COMMA, a); RIGHT_BRACE; { els }

(**
 * Terminals
 *)

let literal :=
  | c = CHAR; { Const.char c }
  | BINARY_OPEN; s = STRING; BINARY_CLOSE; { Const.binary s }
  | n = INTEGER; { Const.integer n }
  | n = FLOAT; { Const.float n }
  | a = ATOM; { Const.atom (Atom.mk a) }

let name :=
  | n = VARIABLE; { Name.var n }
  | a = ATOM; { Name.atom a }
  | module_name = atom; COLON; n = atom; { Name.qualified ~module_name n }

let atom := a = ATOM; { Atom.mk a }

(**
 * Helpers
 *)
let parens(t) := LEFT_PARENS; els = t; RIGHT_PARENS; { els }
let parlist(t) := els = parens(separated_list(COMMA, t)); { els }
