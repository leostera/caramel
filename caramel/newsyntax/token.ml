open Sexplib.Std

type t =
  | Any
  | Arrow
  | At
  | Atom of string
  | Brace_left
  | Brace_right
  | Bracket_left
  | Bracket_right
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
  | Greater_than
  | Id of string
  | Lesser_than
  | Match
  | Parens_left
  | Parens_right
  | Pipe
  | Pub
  | Semicolon
  | String of string
  | Type
  | Type_var of string
[@@deriving sexp]

type tokens = t list [@@deriving sexp]

(*******************************************************************************)

let find_keyword str =
  match str with
  | "_" -> Some Any
  | "external" -> Some External
  | "fn" -> Some Fn
  | "match" -> Some Match
  | "pub" -> Some Pub
  | "type" -> Some Type
  | _ -> None

(*******************************************************************************)

let pp ppf t =
  let sexp = sexp_of_t t in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let pp_tokens ppf t =
  let sexp = sexp_of_tokens t in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp
