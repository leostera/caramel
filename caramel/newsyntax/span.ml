open Sexplib.Std

module Position = struct
  type t = { filename : string; line_num : int; col_num : int; offset : int }
  [@@deriving sexp]

  let none = { filename = "_none_"; line_num = 0; col_num = 0; offset = 0 }

  let pp ppf t =
    let sexp = sexp_of_t t in
    Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp
end

type t = { start_pos : Position.t; end_pos : Position.t; token : Token.t }
[@@deriving sexp]

let make ~start_pos ~end_pos ~token = { start_pos; end_pos; token }

let token t = t.token

let start_pos t = t.start_pos

let end_pos t = t.end_pos

let of_token ~token =
  { start_pos = Position.none; end_pos = Position.none; token }

let eof = of_token ~token:Token.EOF

type spans = t list [@@deriving sexp]

let pp ppf span =
  let sexp = sexp_of_t span in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let pp_spans ppf spans =
  let sexp = sexp_of_spans spans in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp
