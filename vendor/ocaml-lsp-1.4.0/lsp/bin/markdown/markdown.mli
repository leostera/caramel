(** Trivial parser to extract ```typescript sections out of markdown docs. *)
open! Import

(** Extracts all typescript sections *)
val read_typescript : Lexing.lexbuf -> string list
