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
  Format.fprintf ppf "\n";
  ()

let rec pp_variant_constructor prefix ppf c =
  begin match c.vc_args with
  | [] -> Format.fprintf ppf "%s" c.vc_name;
  | _ ->
      let tag = Format.sprintf "{%s" c.vc_name in
      Format.fprintf ppf "%s" tag;
      c.vc_args
      |> (List.iter (fun arg ->
          Format.fprintf ppf ", ";
          pp_type_kind (prefix^tag) ppf arg));
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
      | Erlast.{ rf_name; rf_type } :: fs -> begin
          Format.fprintf ppf "#{ %s => " rf_name;
          pp_type_kind prefix ppf rf_type;
          match fs with
          | [] -> Format.fprintf ppf " }";
          | fs -> begin
            Format.fprintf ppf "\n";
            fs |> List.iter (fun Erlast.{ rf_name; rf_type } ->
              Format.fprintf ppf "%s, %s => " padding rf_name;
              pp_type_kind prefix ppf rf_type;
              Format.fprintf ppf "\n");
            Format.fprintf ppf "%s}" padding;
          end;
      end
      end

  | Type_variant { constructors } ->
      let padding = H.pad ((String.length prefix) - 2) in
      let c = List.hd constructors in
      let cs = List.tl constructors in
      pp_variant_constructor prefix ppf c;
      Format.fprintf ppf "\n";
      cs |> List.iter (fun c ->
        Format.fprintf ppf "%s| " padding;
        pp_variant_constructor (prefix ^ "| ") ppf c;
        Format.fprintf ppf "\n");
      Format.fprintf ppf "%s" padding;

  | Type_function args ->
      let sgra = args |> List.rev in
      let return = sgra |> List.hd in
      let args = sgra |> List.tl |> List.rev in
      Format.fprintf ppf "fun((";
      begin match args with
      | [] -> ()
      | x :: xs ->
          pp_type_kind prefix ppf x;
          xs |> List.iter( fun arg ->
            Format.fprintf ppf ", ";
            pp_type_kind prefix ppf arg);
      end;
      Format.fprintf ppf ") -> ";
      pp_type_kind prefix ppf return;
      Format.fprintf ppf ")";


  end

let pp_pattern_match ppf pm =
  begin match pm with
  | Pattern_ignore -> Format.fprintf ppf "_" ;
  | Pattern_binding name -> Format.fprintf ppf "%s" name;
  end

let pp_fun_case ppf { fc_lhs; } =
  begin match fc_lhs with
  | [] -> Format.fprintf ppf "()"
  | p :: ps ->
      Format.fprintf ppf "(";
      pp_pattern_match ppf p;
      ps |> List.iter( fun pat ->
        Format.fprintf ppf ", ";
        pp_pattern_match ppf pat);
      Format.fprintf ppf ") -> ok";
  end

let pp_fun_cases ppf fd_cases =
  begin match fd_cases with
  | [] -> Format.fprintf ppf "() -> ok"
  | c :: [] -> pp_fun_case ppf c
  | c :: cs ->
      pp_fun_case ppf c;
      cs |> List.iter( fun case ->
        Format.fprintf ppf ";\n";
        pp_fun_case ppf case);
  end

let pp_function ppf { fd_name; fd_cases; } =
  let prefix = Format.sprintf "%s" fd_name in
  Format.fprintf ppf "%s" prefix;
  pp_fun_cases ppf fd_cases;
  Format.fprintf ppf ".\n\n"

let pp_functions ppf funcs =
  funcs |> (List.iter (pp_function ppf))

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
  pp_functions ppf m.functions;
  ()

