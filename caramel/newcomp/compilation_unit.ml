type t = { source_file : Fpath.t; source_kind : Source_kind.t }

let from_source s =
  Source_kind.of_file_path s
  |> Result.map (fun source_kind -> Ok { source_file = s; source_kind })
  |> Result.join

let file_name t = Fpath.filename t.source_file

let module_name t = Fpath.filename (Fpath.rem_ext ~multi:true t.source_file)

let source_kind t = t.source_kind

let source_file t = Fpath.to_string t.source_file

let pp ppf t =
  let open Sexplib in
  let sexpr =
    Sexp.(
      List
        [
          List [ Atom "source_file"; Atom (source_file t) ];
          List
            [ Atom "source_kind"; Atom (Source_kind.to_string t.source_kind) ];
        ])
  in
  Sexp.pp_hum_indent 2 ppf sexpr
