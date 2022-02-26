open Sexplib.Std

type t = [ `Unknown of string ] [@@deriving sexp]

type report = { mutable diagnostics : t list } [@@deriving sexp]

let current = { diagnostics = [] }

let add d = current.diagnostics <- d :: current.diagnostics

let pp ppf =
  let sexp = sexp_of_report current in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let report () =
  match current.diagnostics with
  | [] -> Ok ()
  | _ ->
      pp Format.err_formatter;
      Error ()
