open Erlast

let pp_exports ppf exports =
  let (fn_exports, type_exports) = exports
    |> List.sort (Stdlib.compare)
    |> List.partition (function { exp_type = Export_function } -> true
                              | { exp_type = Export_type     } -> false )
  in
  type_exports
  |> (List.iter (fun { exp_name; exp_arity } ->
      Format.fprintf ppf "-export_type([%s/%d]).\n" exp_name exp_arity));
  Format.fprintf ppf "\n";
  fn_exports
  |> (List.iter (fun { exp_name; exp_arity } ->
      Format.fprintf ppf "-export([%s/%d]).\n" exp_name exp_arity));
  ()

let pp_types ppf types =
  types
  |> (List.iter (fun
    { typ_name; typ_kind } ->
      Format.fprintf ppf "-type %s() :: " typ_name;
      (match typ_kind with
      | Type_record { fields } ->
          begin match fields with
          | [] -> Format.fprintf ppf "#{}";
          | f :: fs -> begin
              Format.fprintf ppf "#{ %s :: any()\n" f;
              fs |> List.iter (fun f -> Format.fprintf ppf ", %s :: any()\n" f );
              Format.fprintf ppf "}";
          end
          end
      | Type_variant { constructors } ->
          let c = List.hd constructors in
          let cs = List.tl constructors in
          Format.fprintf ppf "%s \n" c;
          cs |> List.iter (fun c -> Format.fprintf ppf "| %s\n" c );
      );
      Format.fprintf ppf ".\n";
  ))

let pp ppf m =
  Format.fprintf ppf "-module(%s).\n\n" m.module_name;
  pp_exports ppf m.exports;
  pp_types ppf m.types;
  ()

