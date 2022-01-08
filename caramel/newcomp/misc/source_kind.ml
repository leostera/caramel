type t = Interface | Implementation

let of_file_path s =
  match Filename.extension (Fpath.to_string s) with
  | ".caramel" | ".ml" -> Ok Implementation
  | ".mli" -> Ok Interface
  | ext -> Error (`Invalid_extension ext)

let to_string t = match t with Interface -> "intf" | Implementation -> "impl"

let print_error (`Invalid_extension ext) =
  Format.fprintf Format.std_formatter "Invalid file extension: %s\n%!" ext
