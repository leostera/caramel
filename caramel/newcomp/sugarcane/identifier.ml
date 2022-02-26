open Sexplib.Std

type t = { path : string list; unique_name : string; source_name : string }
[@@deriving sexp]

let empty = { path = []; unique_name = ""; source_name = "" }

let source_name t = t.source_name

let unique_name t = t.unique_name

let pp ppf t =
  let sexp = sexp_of_t t in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let is_erasable_unit id = id.source_name = "_caramel_erasable_unit"

let to_string t = String.concat "." (List.rev (t.source_name :: t.path))

let clean str =
  str
  |> Str.global_replace (Str.regexp "*") "_"
  |> Str.global_replace (Str.regexp "'") "_prime"

let of_ident id =
  let unique_name = clean (Ident.unique_name id) in

  let source_name =
    match Ident.name id with
    | "prim" | "*match*" -> unique_name
    | src -> clean src
  in

  { empty with unique_name; source_name }

let is_module t =
  match String.get t.source_name 0 with 'A' .. 'Z' -> true | _ -> false

let namespace t ts = { t with path = List.map (fun t -> t.source_name) ts }

let join a b = { a with path = a.path @ b.path }

let module_name_of_string str =
  let str = "Caramel." ^ String.capitalize_ascii str in
  { empty with unique_name = str; source_name = str }

let to_argument t =
  { t with source_name = String.capitalize_ascii t.source_name }

let to_variable = to_argument

let match_failure =
  { empty with unique_name = "Match_failure"; source_name = "Match_failure" }

let is_match_failure id =
  let id = of_ident id in
  id = match_failure

let is_unit id = id.unique_name = "unit"
