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

let rec pp_pattern_match ppf pm =
  begin match pm with
  | Pattern_ignore -> Format.fprintf ppf "_" ;

  | Pattern_binding name -> Format.fprintf ppf "%s" name;

  | Pattern_match name -> Format.fprintf ppf "%s" name;

  | Pattern_tuple [] -> Format.fprintf ppf "{}";

  | Pattern_tuple (p :: []) ->
      Format.fprintf ppf "{";
      pp_pattern_match ppf p;
      Format.fprintf ppf "}";

  | Pattern_tuple (p :: ps) ->
      Format.fprintf ppf "{";
      pp_pattern_match ppf p;
      ps
      |> (List.iter (fun p ->
          Format.fprintf ppf ", ";
          pp_pattern_match ppf p));
      Format.fprintf ppf "}";

  | Pattern_list [] -> Format.fprintf ppf "[]";

  | Pattern_list (p :: []) ->
      Format.fprintf ppf "[";
      pp_pattern_match ppf p;
      Format.fprintf ppf "]";

  | Pattern_list (p :: ps) ->
      Format.fprintf ppf "[";
      pp_pattern_match ppf p;
      ps
      |> (List.iter (fun p ->
          Format.fprintf ppf " | ";
          pp_pattern_match ppf p));
      Format.fprintf ppf "]";

  | Pattern_map [] -> Format.fprintf ppf "#{}";

  | Pattern_map ((name, pat) :: []) ->
      Format.fprintf ppf "#{ %s := " name;
      pp_pattern_match ppf pat;
      Format.fprintf ppf " }";

  | Pattern_map ((name, pat) :: ps) ->
      Format.fprintf ppf "#{ %s := " name;
      pp_pattern_match ppf pat;
      ps
      |> (List.iter (fun (name, p) ->
          Format.fprintf ppf ", %s := " name;
          pp_pattern_match ppf p));
      Format.fprintf ppf " }";
  end

let rec pp_expression prefix ppf expr =
  Format.fprintf ppf "%s" prefix;
  begin match expr with
  | Expr_name name -> Format.fprintf ppf "%s" name;

  | Expr_tuple [] -> Format.fprintf ppf "{}";

  | Expr_tuple (e :: []) ->
      Format.fprintf ppf "{";
      pp_expression prefix ppf e;
      Format.fprintf ppf "}";

  | Expr_tuple (e :: es) ->
      Format.fprintf ppf "{";
      pp_expression "" ppf e;
      es
      |> (List.iter (fun e ->
          Format.fprintf ppf ", ";
          pp_expression "" ppf e));
      Format.fprintf ppf "}";

  | Expr_list [] -> Format.fprintf ppf "[]";

  | Expr_list (e :: []) ->
      Format.fprintf ppf "[";
      pp_expression prefix ppf e;
      Format.fprintf ppf "]";

  | Expr_list (e :: es) ->
      Format.fprintf ppf "[";
      pp_expression "" ppf e;
      es
      |> (List.iter (fun e ->
          Format.fprintf ppf ", ";
          pp_expression "" ppf e));
      Format.fprintf ppf "]";

  | Expr_case (expr, branches) ->
      Format.fprintf ppf "case ";
      pp_expression "" ppf expr;
      Format.fprintf ppf " of";
      begin match branches with
      | [] -> ()
      | Erlast.{ cb_pattern; cb_expr } :: bs -> begin
          let prefix = prefix ^ "  " in
          Format.fprintf ppf "\n%s" prefix;
          pp_pattern_match ppf cb_pattern;
          Format.fprintf ppf " -> ";
          pp_expression "" ppf cb_expr;
          match bs with
          | [] -> ()
          | bs -> begin
            bs |> List.iter (fun Erlast.{ cb_pattern; cb_expr } ->
              Format.fprintf ppf ";\n%s" prefix;
              pp_pattern_match ppf cb_pattern;
              Format.fprintf ppf " -> ";
              pp_expression "" ppf cb_expr;
            );
          end;
      end;
      Format.fprintf ppf "\n%send" prefix;


      end

  | Expr_map fields ->
      let padding = H.pad ((String.length prefix) + 1) in
      begin match fields with
      | [] -> Format.fprintf ppf "#{}";
      | Erlast.{ mf_name; mf_value } :: fs -> begin
          Format.fprintf ppf "#{ %s => " mf_name;
          pp_expression "" ppf mf_value;
          match fs with
          | [] -> Format.fprintf ppf " }";
          | fs -> begin
            Format.fprintf ppf "\n";
            fs |> List.iter (fun Erlast.{ mf_name; mf_value } ->
              Format.fprintf ppf "%s, %s => " padding mf_name;
              pp_expression "" ppf mf_value;
              Format.fprintf ppf "\n");
            Format.fprintf ppf "%s}" padding;
          end;
      end
      end
  end

let pp_fun_case _prefix ppf { fc_lhs; fc_rhs } =
  begin match fc_lhs with
  | [] -> Format.fprintf ppf "()"
  | p :: ps ->
      Format.fprintf ppf "(";
      pp_pattern_match ppf p;
      ps |> List.iter( fun pat ->
        Format.fprintf ppf ", ";
        pp_pattern_match ppf pat );
      Format.fprintf ppf ") ->";
      let prefix = (begin match fc_rhs with
      | Expr_map _
      | Expr_case _ -> Format.fprintf ppf "\n";  "  "
      | Expr_list _
      | Expr_tuple _
      | Expr_name _ -> " ";
      end) in
      pp_expression prefix ppf fc_rhs;
  end

let pp_fun_cases prefix ppf fd_name fd_cases =
  begin match fd_cases with
  | [] -> Format.fprintf ppf "() -> ok"
  | c :: [] -> pp_fun_case prefix ppf c
  | c :: cs ->
      pp_fun_case prefix ppf c;
      cs |> List.iter( fun case ->
        Format.fprintf ppf ";\n%s" fd_name;
        pp_fun_case prefix ppf case);
  end

let pp_function ppf { fd_name; fd_cases; } =
  let prefix = Format.sprintf "%s" fd_name in
  Format.fprintf ppf "%s" prefix;
  pp_fun_cases prefix ppf fd_name fd_cases;
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

