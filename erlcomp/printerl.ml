open Erlast

module H = struct
  let pad n = String.make n ' '
end

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

let rec pp_variant_constructor ppf c =
  begin match c.vc_args with
  | [] -> Format.fprintf ppf "%s" c.vc_name;
  | _ ->
      Format.fprintf ppf "{%s" c.vc_name;
      c.vc_args
      |> (List.iter (fun arg ->
          Format.fprintf ppf ", ";
          pp_type_kind "" ppf arg));
      Format.fprintf ppf "}";
  end

and pp_type_kind prefix ppf typ_kind =
  begin match typ_kind with
  | Type_variable var_name ->
      Format.fprintf ppf "%s" var_name;

  | Type_constr { tc_name; tc_args} ->
      Format.fprintf ppf "%s(" tc_name;
      begin match tc_args with
      | [] -> ()
      | a :: args ->
          pp_type_kind prefix ppf a;
          args |> List.iter( fun arg ->
            Format.fprintf ppf ", ";
            pp_type_kind prefix ppf arg);
      end;
      Format.fprintf ppf ")";

  | Type_tuple parts ->
      Format.fprintf ppf "{";
      let p = List.hd parts in
      let ps = List.tl parts in
      pp_type_kind prefix ppf p;
      ps |> List.iter (fun p ->
        Format.fprintf ppf ", ";
        pp_type_kind prefix ppf p;
      );
      Format.fprintf ppf "}";

  | Type_record { fields } ->
      let padding = H.pad ((String.length prefix) + 1) in
      begin match fields with
      | [] -> Format.fprintf ppf "#{}";
      | Erlast.{ rf_name } :: fs -> begin
          Format.fprintf ppf "#{ %s :: any()\n" rf_name;
          fs |> List.iter (fun Erlast.{ rf_name } ->
            Format.fprintf ppf "%s, %s :: any()\n" padding rf_name );
          Format.fprintf ppf "%s}" padding;
      end
      end

  | Type_variant { constructors } ->
      let padding = H.pad ((String.length prefix) - 2) in
      let c = List.hd constructors in
      let cs = List.tl constructors in
      pp_variant_constructor ppf c;
      Format.fprintf ppf "\n";
      cs |> List.iter (fun c ->
        Format.fprintf ppf "%s| " padding;
        pp_variant_constructor ppf c;
        Format.fprintf ppf "\n");
      Format.fprintf ppf "%s" padding;

  end

let pp_types ppf types =
  types
  |> (List.iter (fun
    { typ_name; typ_kind; typ_params } ->
      let params = typ_params |> (String.concat ", ") in
      let prefix = Format.sprintf "-type %s(%s) :: " typ_name params in
      Format.fprintf ppf "%s" prefix;
      pp_type_kind prefix ppf typ_kind;
      Format.fprintf ppf ".\n\n"
  ))

let pp ppf m =
  Format.fprintf ppf "-module(%s).\n\n" m.module_name;
  pp_exports ppf m.exports;
  pp_types ppf m.types;
  ()

