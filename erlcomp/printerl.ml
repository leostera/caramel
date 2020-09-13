open Erlast

let pp ppf m =
  Format.fprintf ppf "-module(%s).\n\n" m.module_name;
  m.exports
  |> (List.iter (fun export ->
      Format.fprintf ppf "-%s([%s/%d]).\n"
      begin match export.export_type with
      | Export_function -> "export"
      | Export_type -> "export_type"
      end export.name export.arity))
