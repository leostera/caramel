open Sexplib.Std

type t =
  | Ampersand
  | And
  | Any
  | Arrow
  | At
  | Atom of string
  | Bang
  | Brace_left
  | Brace_right
  | Bracket_left
  | Bracket_right
  | Caret
  | Colon
  | Comma
  | Dash
  | Dot
  | Dot_dot
  | Dot_dot_dot
  | EOF
  | Equal
  | External
  | Fn
  | Fun_pipe
  | Greater_or_equal
  | Greater_than
  | Id of string
  | Lesser_or_equal
  | Lesser_than
  | Let
  | Macro
  | Match
  | Module
  | Not_equal
  | Open
  | Or
  | Parens_left
  | Parens_right
  | Pipe
  | Plus
  | Integer of string
  | Plus_plus
  | Pub
  | Quote
  | Semicolon
  | Slash
  | Comment of string
  | Star
  | String of string
  | Type
  | Type_var of string
  | Unquote
  | Unquote_splicing
[@@deriving sexp]

type tokens = t list [@@deriving sexp]

(*******************************************************************************)

let find_keyword str =
  match str with
  | "_" -> Some Any
  | "external" -> Some External
  | "fn" -> Some Fn
  | "let" -> Some Let
  | "macro" -> Some Macro
  | "match" -> Some Match
  | "module" -> Some Module
  | "open" -> Some Open
  | "pub" -> Some Pub
  | "quote" -> Some Quote
  | "type" -> Some Type
  | "unquote" -> Some Unquote
  | "unquote_splicing" -> Some Unquote_splicing
  | _ -> None

let is_op t =
  match t with
  | Ampersand | And | Dash | Equal | Greater_or_equal | Greater_than
  | Lesser_or_equal | Lesser_than | Not_equal | Or | Plus_plus | Plus | Slash
  | Star | Caret | Fun_pipe ->
      true
  | _ -> false

(*******************************************************************************)

let pp ppf t =
  let sexp = sexp_of_t t in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let pp_tokens ppf t =
  let sexp = sexp_of_tokens t in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

(*******************************************************************************)

let to_string t =
  match t with
  | Ampersand -> "&"
  | And -> "&&"
  | Any -> "_"
  | Arrow -> "->"
  | At -> "@"
  | Atom n -> ":" ^ n
  | Bang -> "!"
  | Brace_left -> "{"
  | Brace_right -> "}"
  | Bracket_left -> "["
  | Bracket_right -> "]"
  | Caret -> "^"
  | Colon -> ":"
  | Comma -> ","
  | Dash -> "-"
  | Dot -> "."
  | Dot_dot -> ".."
  | Dot_dot_dot -> "..."
  | EOF -> "\0"
  | Equal -> "="
  | External -> "external"
  | Fn -> "fn"
  | Fun_pipe -> "|>"
  | Greater_or_equal -> ">="
  | Greater_than -> ">"
  | Id id -> id
  | Lesser_or_equal -> "<="
  | Lesser_than -> "<"
  | Let -> "let"
  | Macro -> "macro"
  | Match -> "match"
  | Module -> "module"
  | Not_equal -> "!="
  | Open -> "open"
  | Or -> "||"
  | Parens_left -> "("
  | Parens_right -> ")"
  | Pipe -> "|"
  | Plus -> "+"
  | Plus_plus -> "++"
  | Pub -> "pub"
  | Quote -> "quote"
  | Semicolon -> ";"
  | Slash -> "/"
  | Star -> "*"
  | String str -> str
  | Type -> "type"
  | Integer i -> i
  | Comment c -> "//" ^ c
  | Type_var var -> "'" ^ var
  | Unquote -> "unquote"
  | Unquote_splicing -> "unquote_splicing"
