type ident = string

type literal =
  | Atom of ident
  | String of ident
  (* | Binary of ident *)
  | Char of char
  | Integer of int
  | Flat of float

type name =
  | Atom of ident
  | Var of ident

type arity =
  | Fixed of int
  | Var of ident

type binary_op =
    (* ! *)
  | Send
  | Or_else
  | And_also
  | Equal
  | Not_equal
  | Less_than_or_equal
  | Less_than
  | Greater_than_or_equal
  | Greater_than
  | Strict_equal
  | Strict_not_equal
  | Append
  | Remove
  | Add
  | Sub
  | Binary_or
  | Binary_xor
  | Binary_shift_left
  | Binary_shift_right
  | Or
  | Xor
  | Divide
  | Multiply
  | Div
  | Rem
  | Binary_and
  | And

type unary_op =
  | Plus
  | Minus
  | Bnot
  | Not

type type_ = unit

type type_guard = {
  name: name;
  type_: type_;
}

type type_sig = {
  params: type_ list;
  return: type_;
  guards: type_guard list option;
}

type type_spec = {
  module_: ident option;
  function_: ident;
  signatures: type_sig list;
}

type type_def = {
  is_opaque: bool;
  name: ident;
  params: name list;
  body: type_
}

type expression =
  | Var of ident
  | Literal of literal
  | Function_call of function_call
  | Nil
  | Cons of (expression * expression)
  | Tuple of expression list
  (*
  | Binary of Binary
  | Catch of Catch
  | Map of Map
  | Map_projection of MapProjection
  | Map_update of MapUpdate
  | Match of Match
  | Record_access of RecordAccess
  | Record_index of RecordIndex
  | Record_update of RecordUpdate
  | Try of Try
  *)
  | Record of record
  | Apply of apply
  | Binary_expression of expression * binary_op * expression
  | Unary_expression of unary_op * expression
  | If of if_expression
  | Case of case_expression
  | Receive of receive_expression
  | Fun of function_decl

and clause = {
  pattern: expression;
  guard: guard list option;
  clause_body: expression list;
}

and if_clause =  {
  if_guards: guard list option;
  if_body: expression list;
}

and if_expression = if_clause list

and case_expression = {
  expression: expression;
  clasues: clause list;
}

and receive_after_expression = {
  timeout: expression;
  after_body: expression list;
}

and receive_expression = {
  receive_clasues: clause list option;
  after: receive_after_expression option ;
}

and apply = {
  func: expression;
  args: expression list;
}

and record_field = {
  name: ident;
  value: expression option;
  type_: type_ option
}

and record = {
  record_name: ident;
  fields: record_field list
}

and guard = expression list

and function_call = {
  (* We can and should be able to represent local and unresolved calls like:
   *   f(1) instead of m:f(1)
   *   M(1) instead of f(1), and
   *   M:F(1) instead of m:f(1)
   *
   * So both atom and names should be allowed at this point.
   *)
  module_: name option;
  function_name: name;
  arity: arity
}

and function_clause = {
  params: expression list;
  fun_guards: guard list option;
  fun_body: expression list;
}

and function_decl =
  | Named of { name: ident; arity: arity; clauses: function_clause list; spec: type_spec option; }
  | Lambda of { arity: arity; clauses: function_clause list; }

type t = {
  file_name: string;
}

let make name = { file_name = name ^ ".erl" }
