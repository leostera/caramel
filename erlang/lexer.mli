type error =
  | Unterminated_string
  | Invalid_literal of string

exception Error of error * Location.t

val token : Lexing.lexbuf -> Parser.token

val pp_err : Format.formatter -> error -> unit
