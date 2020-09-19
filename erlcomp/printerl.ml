open Erlast

exception Undefined_function_reference of string
exception Function_without_cases of string
exception Type_constructors_must_be_atoms_or_qualified_names of Erlast.name

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

  | Type_constr { tc_name=Atom_name "unit"; } ->
      Format.fprintf ppf "ok";

  | Type_constr { tc_name; tc_args} ->
      let tc_name = match tc_name with
      | Atom_name x -> x |> String.lowercase_ascii
      | Qualified_name {n_mod;n_name} -> Format.sprintf "%s:%s" n_mod n_name
      | _ -> raise (Type_constructors_must_be_atoms_or_qualified_names tc_name)
      in
      Format.fprintf ppf "%s(" tc_name;
      begin match tc_name, tc_args with
      | _, [] -> ()
      (* NOTE: need to find a better way to map these types and their arities *)
      | "erlang:pid", _ -> ()
      | _, a :: args ->
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

  | Pattern_tuple [] -> Format.fprintf ppf "ok";

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

let pp_literal ppf lit =
  let str = match lit with
  | Lit_integer string -> string
  | Lit_char string -> "'" ^ string ^ "'"
  | Lit_binary string -> "<<\"" ^ (String.escaped string) ^ "\">>"
  | Lit_float string -> string
  in
  Format.fprintf ppf "%s" str

let rec pp_expression prefix ppf expr ~module_ =
  Format.fprintf ppf "%s" prefix;
  begin match expr with
  | Expr_name (Var_name name) ->
      Format.fprintf ppf "%s" (String.capitalize_ascii name);

  | Expr_name (Atom_name name) ->
      Format.fprintf ppf "%s" (String.lowercase_ascii name);

  | Expr_name (Macro_name name) ->
      Format.fprintf ppf "?%s" name;

  | Expr_name (Qualified_name { n_mod; n_name }) ->
      (* TODO: lookup n_mod and n_name in a global table of symbols to
       * figure out what it actually translates to since it could be a external
       * call!
       *)
      Format.fprintf ppf "%s:%s" (String.lowercase_ascii n_mod) n_name;

  | Expr_literal lit -> pp_literal ppf lit;

  | Expr_fun { fd_name; fd_cases } ->
    Format.fprintf ppf "fun\n  %s" prefix;
    begin match fd_cases with
    | [] -> raise (Function_without_cases fd_name);
    | c :: [] -> pp_fun_case prefix ppf c ~module_
    | c :: cs ->
        pp_fun_case prefix ppf c ~module_;
        cs |> List.iter( fun case ->
          Format.fprintf ppf ";\n  %s" prefix;
          pp_fun_case prefix ppf case ~module_);
    end;
    Format.fprintf ppf "\n%send" prefix;


  | Expr_let (binding, expr) ->
      begin match binding.lb_lhs with
      | Pattern_ignore -> ()
      | _ ->
        pp_pattern_match ppf binding.lb_lhs;
        Format.fprintf ppf " = ";
      end;
      pp_expression "" ppf binding.lb_rhs ~module_;
      Format.fprintf ppf ",\n";
      pp_expression prefix ppf expr ~module_;

  | Expr_fun_ref "__caramel_recv" ->
      Format.fprintf ppf "fun (T) -> ";
      Format.fprintf ppf   "receive X -> {some, X} ";
      Format.fprintf ppf   "after T -> none ";
      Format.fprintf ppf   "end ";
      Format.fprintf ppf "end";

  | Expr_fun_ref name ->
      begin match Erlast.find_fun_by_name ~module_ name with
      | None -> ()
      | Some { fd_arity } ->
          Format.fprintf ppf "fun %s/%d" name fd_arity;
      end

  | Expr_apply { fa_name; fa_args } ->
      pp_expression "" ppf fa_name ~module_;
      begin match fa_args with
      | [] -> Format.fprintf ppf "()"
      | (Expr_tuple []) :: [] -> Format.fprintf ppf "()"
      | exp :: args ->
          Format.fprintf ppf "(";
          pp_expression "" ppf exp ~module_;
          args |> (List.iter (fun e ->
            Format.fprintf ppf ", ";
            pp_expression "" ppf e ~module_));
          Format.fprintf ppf ")";
      end

  | Expr_tuple [] -> Format.fprintf ppf "ok";

  | Expr_tuple (e :: []) ->
      Format.fprintf ppf "{";
      pp_expression prefix ppf e ~module_;
      Format.fprintf ppf "}";

  | Expr_tuple (e :: es) ->
      Format.fprintf ppf "{";
      pp_expression "" ppf e ~module_;
      es
      |> (List.iter (fun e ->
          Format.fprintf ppf ", ";
          pp_expression "" ppf e ~module_));
      Format.fprintf ppf "}";

  | Expr_list [] -> Format.fprintf ppf "[]";

  | Expr_list (e :: []) ->
      Format.fprintf ppf "[";
      pp_expression prefix ppf e ~module_;
      Format.fprintf ppf "]";

  | Expr_list (e :: es) ->
      Format.fprintf ppf "[";
      pp_expression "" ppf e ~module_;
      es
      |> (List.iter (fun e ->
          Format.fprintf ppf " | ";
          pp_expression "" ppf e ~module_));
      Format.fprintf ppf "]";

  | Expr_case (expr, branches) ->
      Format.fprintf ppf "case ";
      pp_expression "" ppf expr ~module_;
      Format.fprintf ppf " of";
      begin match branches with
      | [] -> ()
      | Erlast.{ cb_pattern; cb_expr } :: bs -> begin
          let prefix = prefix ^ "  " in
          Format.fprintf ppf "\n%s" prefix;
          pp_pattern_match ppf cb_pattern;
          Format.fprintf ppf " -> ";
          pp_expression "" ppf cb_expr ~module_;
          match bs with
          | [] -> ()
          | bs -> begin
            bs |> List.iter (fun Erlast.{ cb_pattern; cb_expr } ->
              Format.fprintf ppf ";\n%s" prefix;
              pp_pattern_match ppf cb_pattern;
              Format.fprintf ppf " -> ";
              pp_expression "" ppf cb_expr ~module_;
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
          pp_expression "" ppf mf_value ~module_;
          match fs with
          | [] -> Format.fprintf ppf " }";
          | fs -> begin
            Format.fprintf ppf "\n";
            fs |> List.iter (fun Erlast.{ mf_name; mf_value } ->
              Format.fprintf ppf "%s, %s => " padding mf_name;
              pp_expression "" ppf mf_value ~module_;
              Format.fprintf ppf "\n");
            Format.fprintf ppf "%s}" padding;
          end;
      end
      end
  end

and pp_fun_args ppf args =
  match args with
  | [] -> ()
  | arg :: [] ->
      if arg = Pattern_tuple []
      then ()
      else pp_pattern_match ppf arg
  | p :: ps ->
      pp_pattern_match ppf p;
      ps |> List.iter( fun pat ->
        Format.fprintf ppf ", ";
        pp_pattern_match ppf pat );

and pp_fun_case _prefix ppf { fc_lhs; fc_rhs } ~module_ =
  begin match fc_lhs with
  | [] -> Format.fprintf ppf "()"
  | args ->
      Format.fprintf ppf "(";
      pp_fun_args ppf args;
      Format.fprintf ppf ") ->";
      let prefix = (begin match fc_rhs with
      | Expr_map _
      | Expr_let _
      | Expr_case _ -> Format.fprintf ppf "\n";  "  "
      | Expr_fun _
      | Expr_apply _
      | Expr_fun_ref _
      | Expr_list _
      | Expr_tuple _
      | Expr_literal _
      | Expr_name _ -> " ";
      end) in
      pp_expression prefix ppf fc_rhs ~module_;
  end

let pp_fun_cases prefix ppf fd_name fd_cases ~module_ =
  begin match fd_cases with
  | [] -> raise (Function_without_cases fd_name);
  | c :: [] -> pp_fun_case prefix ppf c ~module_
  | c :: cs ->
      pp_fun_case prefix ppf c ~module_;
      cs |> List.iter( fun case ->
        Format.fprintf ppf ";\n%s" fd_name;
        pp_fun_case prefix ppf case ~module_);
  end

let pp_function ppf { fd_name; fd_cases; }  ~module_=
  let prefix = Format.sprintf "%s" fd_name in
  Format.fprintf ppf "%s" prefix;
  pp_fun_cases prefix ppf fd_name fd_cases ~module_;
  Format.fprintf ppf ".\n\n"

let pp_functions ppf funcs  ~module_=
  funcs |> (List.iter (pp_function ppf ~module_))

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

let pp ppf ({ module_name; exports; types; functions } as m) =
  match exports with
  | [] -> ()
  | _ -> begin
    Format.fprintf ppf "-module(%s).\n" module_name;
    pp_exports ppf exports;
    pp_types ppf types;
    pp_functions ppf functions ~module_:m;
    ()
  end

