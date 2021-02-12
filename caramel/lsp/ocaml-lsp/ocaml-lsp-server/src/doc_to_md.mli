type t =
  | Raw of string
  | Markdown of string

val translate : string -> t
