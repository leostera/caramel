type t = Interface | Implementation

let of_file_path s =
  match Filename.extension (Fpath.to_string s) with
  | ".ml" -> Ok Implementation
  | ".mli" -> Ok Interface
  | ext -> Error (`Invalid_extension ext)

let to_string t = match t with Interface -> "intf" | Implementation -> "impl"
