(**
   This module deals with misc. aspects of the syntax such as which
   strings are keywords.
*)

let keywords = []

let is_keyword str =
  match str with
  | "not" | "and" | "or" | "div" | "rem" | "band" | "bor" | "bxor" | "bnot"
  | "bsl" | "bsr" ->
      true
  | _ -> false
