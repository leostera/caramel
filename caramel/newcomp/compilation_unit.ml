type t = { source_file : Fpath.t; source_kind : Source_kind.t }

let from_source s =
  Source_kind.of_file_path s
  |> Result.map (fun source_kind -> Ok { source_file = s; source_kind })
