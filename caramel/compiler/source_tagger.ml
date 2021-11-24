open Target

exception Unsupported_file_type_for_target of (Target.t * string * string)

type tag =
  | Ml of string
  | Mli of string
  | Other of (Target.t * string * string)

let pp ppf = function
  | Ml file -> Format.fprintf ppf "Ml %s" file
  | Mli file -> Format.fprintf ppf "Mli %s" file
  | Other _ -> Format.fprintf ppf "Other"

let tag target filename =
  match target with
  | Erlang -> (
      match Filename.extension filename with
      | ".ml" -> Ml filename
      | ".mli" -> Mli filename
      | ext -> Other (target, filename, ext))

let assert_all_sources_are_supported sources =
  List.iter
    (function
      | Other (tgt, file, ext) ->
          Format.fprintf Format.std_formatter
            "Attempted to compile %s, but %s files are not supported with the \
             target flag: --target=%s%!"
            file ext (Target.to_string tgt);
          exit 1
      | _ -> ())
    sources

let prepare ~sources ~target =
  let tagged_sources = List.map (tag target) sources in
  assert_all_sources_are_supported tagged_sources;
  let ml_sources =
    tagged_sources
    |> List.filter_map (function Ml f | Mli f -> Some f | Other _ -> None)
    |> List.map (tag target)
  in
  ml_sources
