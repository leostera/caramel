%{
open Erl_ast
open Erl_ast_helper

type parse_error =
  | Expression_is_invalid_pattern of expr
  | Arrow_types_must_start_with_spec
  | Unsupported_error_class
  | Functions_must_have_clauses
  | Unknown_type_visibility of string

exception Parse_error of parse_error
exception Error

let throw x =
  begin match x with
  | Unsupported_error_class ->
      Format.fprintf Format.std_formatter "Error classes must be 'throw' or 'error'"
  | Arrow_types_must_start_with_spec ->
      Format.fprintf Format.std_formatter "Arrow types must start with -spec "
  | Unknown_type_visibility v ->
      Format.fprintf Format.std_formatter "Unknown_type_visibility: %s" v
  | Functions_must_have_clauses ->
      Format.fprintf Format.std_formatter "Functions_must_have_clauses"
  | Expression_is_invalid_pattern expr ->
      Format.fprintf Format.std_formatter "Expression_is_invalid_pattern: ";
      Sexplib.Sexp.pp_hum_indent 2 Format.std_formatter (sexp_of_expr expr);
      Format.fprintf Format.std_formatter "\n\n";
      Erl_printer.pp_expression "" Format.std_formatter expr ~module_:Mod.empty
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
  | Expr_nil -> Pattern_list []
  | Expr_let ({ lb_lhs; lb_rhs = (Expr_name (Var_name name))}, _) -> Pattern_with_name (lb_lhs, (Name.var name))
  | _ -> throw (Expression_is_invalid_pattern expr)

%}

(******************************************************************************
 *
 * Tokens
 *
 ******************************************************************************)

(* Tokens with a Value *)
%token <string * Parse_info.t> CHAR ATOM FLOAT INTEGER STRING VARIABLE COMMENT

(* Keyword Tokens *)
%token <Parse_info.t> AFTER AND ANDALSO BAND BEGIN BNOT BOR BSL BSR BXOR CASE
CATCH DIV END FUN IF NOT OF OR ORELSE RECEIVE REM THROW TRY WHEN XOR

(* Symbol Tokens *)
%token <Parse_info.t> ARROW ARROW_BACK BANG
BINARY_CLOSE BINARY_OPEN
COMMA SEMICOLON COLON COLON_COLON DOT
LEFT_BRACE RIGHT_BRACE
LEFT_BRACKET RIGHT_BRACKET
LEFT_PARENS RIGHT_PARENS
PIPE SLASH DASH UNDERSCORE
LT GT LTE GTE
PLUS STAR PLUS_PLUS MINUS_MINUS
EQUAL EQUAL_EQUAL SLASH_EQUAL EQUAL_COLON_EQUAL EQUAL_SLASH_EQUAL COLON_EQUAL

(* EOF *)
%token <Parse_info.t> EOF

%start <Erl_ast.structure> module_file
%%

(******************************************************************************
 *
 * Rule declarations
 *
 ******************************************************************************)

let module_file := is = module_item+; EOF; { is }

let module_item :=
  | ~ = comment; { Module_comment comment }
  | ~ = module_attribute; { Module_attribute module_attribute }
  | ~ = type_decl; { Type_decl type_decl }
  | ~ = fun_decl; { Function_decl fun_decl }

(** Module Attributes *)
let module_attribute :=
  | DASH; atr_name = atom; atr_value = parens(expr); DOT;
    { { atr_name; atr_value} }

(** Type Language *)
let type_decl :=
  | DASH;
    kind = type_kind;
    name = atom;
    params = parlist(type_expr); COLON_COLON;
    expr = type_expr;
    _ = type_constraint?;
    DOT;
    (* NOTE: should we constraint params to only names here? *)
    { Type.mk ~kind ~name ~params ~expr  }
  | DASH;
    kind = type_kind;
    name = atom;
    params = parlist(type_expr); ARROW;
    expr = type_expr;
    _ = type_constraint?;
    DOT;
    {
      (match kind with
      | Spec -> ()
      | _ -> throw Arrow_types_must_start_with_spec);
      Type.mk ~kind ~name ~params ~expr
    }

let type_kind :=
  | (atom, _) = ATOM; {
    match atom with
    | "opaque" -> Type.opaque
    | "type" -> Type.type_
    | "spec" -> Type.spec
    | _ -> throw (Unknown_type_visibility atom)
  }

(* TODO: constraints are currently being ignored *)
let type_constraint :=
  | WHEN; cs = separated_nonempty_list(COMMA, type_expr); { cs }

let type_expr :=
  (* NOTE: save this name *)
  | _ = name; COLON_COLON; t = type_expr; { t }

  | (n, _) = VARIABLE; { Type.var (Name.var n) }

  | ~ = literal ; { Type.const literal }

  | t = tuple(type_expr); { Type.tuple t }

  | t = delimited(LEFT_BRACKET, type_expr, RIGHT_BRACKET);
    { Type.list t }
  | LEFT_BRACKET; t = type_expr; COMMA; DOT; DOT; DOT; RIGHT_BRACKET;
    { Type.list t }

  | FUN; LEFT_PARENS; args = parlist(type_expr); ARROW; return = type_expr; RIGHT_PARENS;
    { Type.fun_ ~args ~return }

  | ~ = name; args = parlist(type_expr);
    { Type.apply ~args ~name }

  | t = type_expr; PIPE; t2 = type_expr;
    { Type.variant [t; t2] }
  | LEFT_PARENS; t = type_expr; PIPE; t2 = type_expr; RIGHT_PARENS;
    { Type.variant [t; t2] }

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
  | lhs = parlist(expr); guard = guard?; ARROW; rhs = expr;
    { FunDecl.case ~lhs:(List.map expr_to_pattern lhs) ~guard ~rhs }

let guard :=
  | WHEN; exprs = separated_list(SEMICOLON | COMMA, expr); { exprs }

(**
 * Expressions
 *)

let expr :=
  | e = expr_op; { e }
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
  | e = expr_comment; { e }
  | e = expr_if; { e }
  | e = expr_try_catch; { e }

let expr_comment :=
  | c = comment; e = expr; { Expr.comment c e }

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

let expr_if :=
  | IF; clauses = separated_list(SEMICOLON, if_branch); END;
    { Expr.if_ ~clauses }

let if_branch :=
  | lhs = separated_nonempty_list(SEMICOLON, separated_list(COMMA, expr)); ARROW; rhs = expr; { (lhs, rhs) }


let expr_send :=
  | pid = expr; BANG; msg = expr;
  { let send = Expr.ident (Name.qualified ~module_name:(Atom.mk "erlang") (Atom.mk "send")) in
    Expr.apply send [ pid; msg ]
  }

let expr_name  :=
  | n = name; { Expr.ident n }

let expr_literal :=
  | ~ = literal; { Expr_literal literal }

let expr_fun_ref :=
  | FUN; name = atom; SLASH; (arity,_) = INTEGER;
    { Expr.fun_ref name ~arity:(int_of_string arity) }

let expr_apply :=
  | ~ = name; fa_args = parlist(expr);
    { Expr.apply (Expr.ident name) fa_args }
  | THROW; fa_args = parlist(expr);
    { let throw = Expr.ident (Name.qualified ~module_name:(Atom.mk "erlang") (Atom.mk "throw")) in
      Expr.apply throw fa_args }

let expr_list := l = list(expr); { l }

let expr_tuple := t = tuple(expr); { Expr.tuple t }

let expr_case :=
  | CASE; ~ = expr; OF; cases = separated_list(SEMICOLON, case_branch); END;
    { Expr_case (expr, cases) }

let case_branch :=
  | lhs = expr; ARROW; guard = guard?; rhs = expr;
    { FunDecl.case ~lhs:[expr_to_pattern lhs] ~rhs ~guard }
  | lhs = separated_list(COMMA, expr); guard = guard?; ARROW; rhs = expr;
    { FunDecl.case ~lhs:(List.map expr_to_pattern lhs) ~rhs ~guard }

let expr_fun :=
  | FUN; cases = separated_list(SEMICOLON, fun_case); END;
    { Expr.fun_ ~cases }

let expr_try_catch :=
  | TRY; ~ = expr; CATCH; catch = separated_nonempty_list(SEMICOLON, catch); END;
    { Expr.try_ expr ~catch }

let catch :=
  | class_ = catch_class?; p = expr; guard = guard?; ARROW; rhs = expr;
    { let pattern = expr_to_pattern p in
      let lhs = [Pat.catch ~class_ ~stacktrace:None pattern] in
      FunDecl.case ~lhs ~guard ~rhs
    }
  (*
  | class_ = catch_class?; p = expr; COLON; ~ = name; guard = guard?; ARROW; rhs = expr;
    { let pattern = expr_to_pattern p in
      let lhs = [Pat.catch ~class_ ~stacktrace:(Some name) pattern] in
      FunDecl.case ~lhs ~guard ~rhs
    }
    *)

let catch_class :=
  | (n, _) = VARIABLE; COLON; {
      match n with
      | "_" -> Pat.catch_class_throw
      | _ -> throw Unsupported_error_class
    }
  | THROW; COLON; { Pat.catch_class_throw }

let expr_op :=
  | op = operator; rhs = expr; {
    let op = Expr.ident (Name.qualified ~module_name:(Atom.mk "erlang") op) in
    let args = [rhs] in
    Expr.apply op args
  }
  | lhs = expr; op = operator; rhs = expr; {
    let op = Expr.ident (Name.qualified ~module_name:(Atom.mk "erlang") op) in
    let args = [lhs; rhs] in
    Expr.apply op args
  }

(**
 * Comments
 *)
let comment :=
  | (text, _) = COMMENT; { Comment text }

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


let tuple(a) := els = delimited(LEFT_BRACE, separated_list(COMMA, a), RIGHT_BRACE); { els }

(**
 * Terminals
 *)

let literal :=
  | (c, _) = CHAR; { Const.char c }
  | BINARY_OPEN; (s, _) = STRING; BINARY_CLOSE; { Const.binary s }
  | (n, _) = INTEGER; { Const.integer n }
  | (n, _) = FLOAT; { Const.float n }
  | (a, _) = ATOM; { Const.atom (Atom.mk a) }

let name :=
  | (n, _) = VARIABLE; { Name.var n }
  | (a, _) = ATOM; { Name.atom (Atom.mk a) }
  | module_name = atom; COLON; n = atom; { Name.qualified ~module_name n }

let atom := (a, _) = ATOM; { Atom.mk a }

let operator :=
  | AND; { Atom.mk "and" }
  | ANDALSO; { Atom.mk "andalso" }
  | BAND; { Atom.mk "band" }
  | BNOT; { Atom.mk "bnot" }
  | BOR; { Atom.mk "bor" }
  | BSL; { Atom.mk "bsl" }
  | BSR; { Atom.mk "bsr" }
  | BXOR; { Atom.mk "bxor" }
  | COLON_EQUAL; { Atom.mk ":=" }
  | DASH; { Atom.mk "-" }
  | DIV; { Atom.mk "div" }
  | EQUAL_COLON_EQUAL; { Atom.mk "=:=" }
  | EQUAL_EQUAL; { Atom.mk "==" }
  | EQUAL_SLASH_EQUAL; { Atom.mk "=/=" }
  | GT; { Atom.mk ">" }
  | GTE; { Atom.mk ">=" }
  | LT; { Atom.mk "<" }
  | LTE; { Atom.mk "=<" }
  | MINUS_MINUS; { Atom.mk "--" }
  | NOT; { Atom.mk "not" }
  | OR; { Atom.mk "or" }
  | ORELSE; { Atom.mk "orelse" }
  | PLUS; { Atom.mk "+" }
  | PLUS_PLUS; { Atom.mk "++" }
  | REM; { Atom.mk "rem" }
  | SLASH; { Atom.mk "/" }
  | SLASH_EQUAL; { Atom.mk "/=" }
  | STAR; { Atom.mk "*" }
  | XOR; { Atom.mk "xor" }

(**
 * Helpers
 *)
let parens(t) := els = delimited(LEFT_PARENS, t, RIGHT_PARENS); { els }
let parlist(t) := els = parens(separated_list(COMMA, t)); { els }
