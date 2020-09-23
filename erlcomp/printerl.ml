open Erlast

let pp ppf m =
  Format.fprintf ppf "-module(%s).\n\n" m.module_name;
  m.exports
  |> (List.iter (fun export ->
      Format.fprintf ppf "-export([%s/%d]).\n" export.name export.arity))
