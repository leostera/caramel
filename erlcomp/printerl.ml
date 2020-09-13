open Erlast

let pp_exports ppf exports =
  let (fn_exports, type_exports) = exports
    |> List.sort (Stdlib.compare)
    |> List.partition (function { export_type = Export_function } -> true
                              | { export_type = Export_type     } -> false )
  in
  type_exports
  |> (List.iter (fun { name; arity } ->
      Format.fprintf ppf "-export_type([%s/%d]).\n" name arity));
  Format.fprintf ppf "\n";
  fn_exports
  |> (List.iter (fun { name; arity } ->
      Format.fprintf ppf "-export([%s/%d]).\n" name arity));
  ()

let pp ppf m =
  Format.fprintf ppf "-module(%s).\n\n" m.module_name;
  pp_exports ppf m.exports

