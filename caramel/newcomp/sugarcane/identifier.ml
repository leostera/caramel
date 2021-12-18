open Sexplib.Std

type t = { path : string list; unique_name : string; source_name : string }
[@@deriving sexp]

let empty = { path = []; unique_name = ""; source_name = "" }

let to_string t = String.concat "." (List.rev (t.source_name :: t.path))

let of_ident id =
  { path = []; unique_name = Ident.unique_name id; source_name = Ident.name id }

let is_module t =
  match String.get t.source_name 0 with 'A' .. 'Z' -> true | _ -> false

let namespace t ts = { t with path = List.map (fun t -> t.source_name) ts }

let join a b = { a with path = a.path @ b.path }

let module_name_of_string str =
  let str = String.capitalize_ascii str in
  { empty with unique_name = str; source_name = str }
