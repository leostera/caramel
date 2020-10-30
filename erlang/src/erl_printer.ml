open Erl_ast_helper
open Erl_ast

exception Invalid_case_branch

exception Undefined_function_reference of string

exception Function_without_cases of string

exception Lambda_without_cases

exception Function_spec_with_less_than_2_parts of string

exception Type_constructors_must_be_atoms_or_qualified_names of Erl_ast.name

exception Invalid_case_expresion_without_branches

exception Invalid_receive_expresion_without_branches

exception Unknown_support_function

exception Invalid_cons_with_no_left_hand_side

module H = struct
  let pad n =
    let n = if n < 0 then 0 else n in
    String.make n ' '
end

let pp_atom ppf (Atom atom) = Format.fprintf ppf "%s" atom

let rec name_to_string name =
  match name with
  | Var_name name -> String.capitalize_ascii name
  | Atom_name (Atom name) -> name
  | Qualified_name { n_mod; n_name } ->
      Format.sprintf "%s:%s" (name_to_string n_mod) (name_to_string n_name)

let is_caramel_support { fa_name; _ } =
  match fa_name with
  | Expr_name
      (Qualified_name
        {
          n_mod = Atom_name (Atom "caramel");
          n_name = Atom_name (Atom "binary_concat");
        }) ->
      true
  | _ -> false

(*
 * Pretty printing functions
 *)

let pp_exports ppf exports =
  let fn_exports, type_exports =
    exports |> List.sort Stdlib.compare
    |> List.partition (function
         | { exp_type = Export_function; _ } -> true
         | { exp_type = Export_type; _ } -> false)
  in
  type_exports
  |> List.iter (fun { exp_name = Atom exp_name; exp_arity; _ } ->
         Format.fprintf ppf "-export_type([%s/%d]).\n" exp_name exp_arity);
  Format.fprintf ppf "\n";
  fn_exports
  |> List.iter (fun { exp_name = Atom exp_name; exp_arity; _ } ->
         Format.fprintf ppf "-export([%s/%d]).\n" exp_name exp_arity);
  Format.fprintf ppf "\n";
  ()

let rec pp_caramel_support_function _prefix ppf { fa_name; fa_args; _ } ~module_
    =
  match (fa_name, fa_args) with
  | ( Expr_name
        (Qualified_name
          {
            n_mod = Atom_name (Atom "caramel");
            n_name = Atom_name (Atom "binary_concat");
            _;
          }),
      [ lhs; rhs ] ) ->
      Format.fprintf ppf "<< (";
      pp_expression "" ppf lhs ~module_;
      Format.fprintf ppf ")/binary, (";
      pp_expression "" ppf rhs ~module_;
      Format.fprintf ppf ")/binary >>"
  | _, _ -> raise Unknown_support_function

and pp_comment ppf (Comment c) = Format.fprintf ppf "%%%% %s" c

and pp_type_expr prefix ppf typ_expr =
  match typ_expr with
  | Type_const lit -> pp_literal ppf lit
  | Type_variable var_name -> pp_name ppf var_name
  | Type_constr { tc_name = Atom_name (Atom "unit"); _ } ->
      Format.fprintf ppf "ok"
  | Type_constr { tc_name; tc_args; _ } ->
      pp_name ppf tc_name;
      Format.fprintf ppf "(";
      ( match (tc_name, tc_args) with
      | _, [] -> ()
      | _, a :: args ->
          pp_type_expr prefix ppf a;
          args
          |> List.iter (fun arg ->
                 Format.fprintf ppf ", ";
                 pp_type_expr prefix ppf arg) );
      Format.fprintf ppf ")"
  | Type_list p ->
      Format.fprintf ppf "[";
      pp_type_expr prefix ppf p;
      Format.fprintf ppf "]"
  | Type_tuple parts ->
      Format.fprintf ppf "{";
      let p = List.hd parts in
      let ps = List.tl parts in
      pp_type_expr prefix ppf p;
      ps
      |> List.iter (fun p ->
             Format.fprintf ppf ", ";
             pp_type_expr prefix ppf p);
      Format.fprintf ppf "}"
  | Type_map fields ->
      let padding = H.pad (String.length prefix) in
      Format.fprintf ppf "#{ ";
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n%s , " padding)
        (fun ppf { tmf_presence; tmf_name; tmf_value } ->
          Format.fprintf ppf "%a %s %a" (pp_type_expr prefix) tmf_name
            (match tmf_presence with Optional -> ":=" | Mandatory -> "=>")
            (pp_type_expr prefix) tmf_value)
        ppf fields;
      if List.length fields == 1 then Format.fprintf ppf " }"
      else Format.fprintf ppf "\n%s }" padding
  (* FIXME: records should be printed as records, not maps! *)
  | Type_record (name, fields) ->
      Format.fprintf ppf "#%a{" pp_name name;
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
        (fun ppf { rf_name; rf_type } ->
          Format.fprintf ppf "%a => %a" pp_atom rf_name (pp_type_expr prefix)
            rf_type)
        ppf fields;
      Format.fprintf ppf "}"
  | Type_variant tks ->
      let padding = H.pad (String.length prefix - 2) in
      let t = List.hd tks in
      let ts = List.tl tks in
      pp_type_expr prefix ppf t;
      Format.fprintf ppf "\n";
      ts
      |> List.iter (fun c ->
             Format.fprintf ppf "%s| " padding;
             pp_type_expr (prefix ^ "| ") ppf c;
             Format.fprintf ppf "\n");
      Format.fprintf ppf "%s" padding
  | Type_function { tyfun_args = args; tyfun_return = return } ->
      Format.fprintf ppf "fun((";
      ( match args with
      | [] -> ()
      | x :: xs ->
          pp_type_expr prefix ppf x;
          xs
          |> List.iter (fun arg ->
                 Format.fprintf ppf ", ";
                 pp_type_expr prefix ppf arg) );
      Format.fprintf ppf ") -> ";
      pp_type_expr prefix ppf return;
      Format.fprintf ppf ")"

and pp_pattern_match ppf pm =
  match pm with
  | Pattern_catch (None, pat, None) -> pp_pattern_match ppf pat
  | Pattern_catch (None, pat, Some n) ->
      Format.fprintf ppf "%a:%a" pp_pattern_match pat pp_name n
  | Pattern_catch (Some class_, pat, None) ->
      Format.fprintf ppf "%a:%a" pp_name class_ pp_pattern_match pat
  | Pattern_catch (Some class_, pat, Some n) ->
      Format.fprintf ppf "%a:%a:%a" pp_name class_ pp_pattern_match pat pp_name
        n
  | Pattern_ignore -> Format.fprintf ppf "_"
  | Pattern_with_name (pat, name) ->
      pp_pattern_match ppf pat;
      Format.fprintf ppf " = ";
      pp_pattern_match ppf name
  | Pattern_binding name -> pp_name ppf name
  | Pattern_match lit -> pp_literal ppf lit
  | Pattern_tuple [] -> Format.fprintf ppf "ok"
  | Pattern_tuple [ p ] ->
      Format.fprintf ppf "{";
      pp_pattern_match ppf p;
      Format.fprintf ppf "}"
  | Pattern_tuple (p :: ps) ->
      Format.fprintf ppf "{";
      pp_pattern_match ppf p;
      ps
      |> List.iter (fun p ->
             Format.fprintf ppf ", ";
             pp_pattern_match ppf p);
      Format.fprintf ppf "}"
  | Pattern_cons (l, r) ->
      Format.fprintf ppf "[";
      ( match l with
      | [] -> raise Invalid_cons_with_no_left_hand_side
      | x :: xs ->
          pp_pattern_match ppf x;
          List.iter
            (fun e ->
              Format.fprintf ppf ", ";
              pp_pattern_match ppf e)
            xs );
      Format.fprintf ppf " | ";
      pp_pattern_match ppf r;
      Format.fprintf ppf "]"
  | Pattern_list [] -> Format.fprintf ppf "[]"
  | Pattern_list [ p ] ->
      Format.fprintf ppf "[";
      pp_pattern_match ppf p;
      Format.fprintf ppf "]"
  | Pattern_list (p :: ps) ->
      Format.fprintf ppf "[";
      pp_pattern_match ppf p;
      ps
      |> List.iter (fun p ->
             Format.fprintf ppf " | ";
             pp_pattern_match ppf p);
      Format.fprintf ppf "]"
  | Pattern_map [] -> Format.fprintf ppf "#{}"
  | Pattern_map [ (name, pat) ] ->
      Format.fprintf ppf "#{ %a := " pp_pattern_match name;
      pp_pattern_match ppf pat;
      Format.fprintf ppf " }"
  | Pattern_map ((name, pat) :: ps) ->
      Format.fprintf ppf "#{ %a := " pp_pattern_match name;
      pp_pattern_match ppf pat;
      ps
      |> List.iter (fun (name, p) ->
             Format.fprintf ppf ", %a := " pp_pattern_match name;
             pp_pattern_match ppf p);
      Format.fprintf ppf " }"

and pp_literal ppf lit =
  match lit with
  | Lit_float str | Lit_integer str -> Format.fprintf ppf "%s" str
  | Lit_char str -> Format.fprintf ppf "'%s'" str
  | Lit_binary str -> Format.fprintf ppf "<<\"%s\">>" (String.escaped str)
  | Lit_string str -> Format.fprintf ppf "\"%s\"" (String.escaped str)
  | Lit_atom atom -> pp_atom ppf atom

and pp_name ppf name =
  match name with
  | Var_name name -> Format.fprintf ppf "%s" (String.capitalize_ascii name)
  | Atom_name name -> pp_atom ppf name
  | Qualified_name { n_mod; n_name; _ } ->
      (* TODO: lookup n_mod and n_name in a global table of symbols to
       * figure out what it actually translates to since it could be a external
       * call!
       *)
      Format.fprintf ppf "%a:%a" pp_name n_mod pp_name n_name

and pp_expression_list prefix ppf expressions ~module_ =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
    (pp_expression prefix ~module_)
    ppf expressions

and pp_if_case_branch prefix ppf (lhs, rhs) ~module_ =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf ";\n")
    (pp_expression_list prefix ~module_)
    ppf lhs;
  Format.fprintf ppf " -> ";
  pp_expression "" ppf rhs ~module_

and pp_if_case_branches prefix ppf branches ~module_ =
  let prefix = prefix ^ "  " in
  Format.fprintf ppf "\n%s" prefix;
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf ";\n")
    (pp_if_case_branch prefix ~module_)
    ppf branches

and pp_case_guard ppf guard ~module_ =
  match guard with
  | None -> ()
  | Some exprs ->
      Format.fprintf ppf " when ";
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf " ,")
        (fun ppf expr -> pp_expression "" ppf expr ~module_)
        ppf exprs

and pp_case_branches prefix ppf branches ~module_ =
  match branches with
  | { c_lhs = [ c_lhs ]; c_rhs; c_guard } :: bs -> (
      let prefix = prefix ^ "  " in
      Format.fprintf ppf "\n%s" prefix;
      pp_pattern_match ppf c_lhs;
      pp_case_guard ppf c_guard ~module_;
      Format.fprintf ppf " -> ";
      pp_expression "" ppf c_rhs ~module_;
      match bs with
      | [] -> ()
      | bs ->
          bs
          |> List.iter (function
               | { c_lhs = [ c_lhs ]; c_rhs; c_guard } ->
                   Format.fprintf ppf ";\n%s" prefix;
                   pp_pattern_match ppf c_lhs;
                   pp_case_guard ppf c_guard ~module_;
                   Format.fprintf ppf " -> ";
                   pp_expression "" ppf c_rhs ~module_
               | _ -> raise Invalid_case_branch) )
  | _ -> raise Invalid_case_branch

and pp_expression prefix ppf expr ~module_ =
  Format.fprintf ppf "%s" prefix;
  match expr with
  | Expr_macro macro -> Format.fprintf ppf "?%s" macro
  | Expr_catch expr ->
      Format.fprintf ppf "catch %a" (pp_expression prefix ~module_) expr
  | Expr_try { try_expr = e; try_catch; try_after } ->
      Format.fprintf ppf "try\n%s" prefix;
      pp_expression prefix ppf e ~module_;
      ( match try_catch with
      | Some catches ->
          Format.fprintf ppf "\n%scatch\n%s" prefix prefix;
          pp_case_branches prefix ppf catches ~module_
      | _ -> () );
      ( match try_after with
      | Some afterbody ->
          Format.fprintf ppf "\n%safter\n%s" prefix prefix;
          pp_expression prefix ppf afterbody ~module_
      | _ -> () );
      Format.fprintf ppf "end"
  | Expr_comment (c, e) ->
      pp_comment ppf c;
      pp_expression prefix ppf e ~module_
  | Expr_name name -> pp_name ppf name
  | Expr_literal lit -> pp_literal ppf lit
  | Expr_fun fd_cases ->
      Format.fprintf ppf "fun\n  %s" prefix;
      ( match fd_cases with
      | [] -> raise Lambda_without_cases
      | [ c ] -> pp_fun_case prefix ppf c ~module_
      | c :: cs ->
          pp_fun_case prefix ppf c ~module_;
          cs
          |> List.iter (fun case ->
                 Format.fprintf ppf ";\n  %s" prefix;
                 pp_fun_case prefix ppf case ~module_) );
      Format.fprintf ppf "\n%send" prefix
  | Expr_let (binding, expr) ->
      ( match binding.lb_lhs with
      | Pattern_ignore -> ()
      | _ ->
          pp_pattern_match ppf binding.lb_lhs;
          Format.fprintf ppf " = " );
      pp_expression "" ppf binding.lb_rhs ~module_;
      Format.fprintf ppf ",\n";
      pp_expression prefix ppf expr ~module_
  | Expr_fun_ref { fref_name = Atom_name (Atom "'__caramel_recv'"); _ } ->
      Format.fprintf ppf "fun (T) -> ";
      Format.fprintf ppf "receive X -> {some, X} ";
      Format.fprintf ppf "after T -> none ";
      Format.fprintf ppf "end ";
      Format.fprintf ppf "end"
  | Expr_fun_ref { fref_name = name; fref_arity } ->
      Format.fprintf ppf "fun %a/%d" pp_name name fref_arity
  | Expr_apply apply when is_caramel_support apply ->
      pp_caramel_support_function prefix ppf apply ~module_
  | Expr_apply { fa_name; fa_args; _ } -> (
      pp_expression "" ppf fa_name ~module_;
      match fa_args with
      | [] -> Format.fprintf ppf "()"
      | [ Expr_tuple [] ] -> Format.fprintf ppf "()"
      | exp :: args ->
          Format.fprintf ppf "(";
          pp_expression "" ppf exp ~module_;
          args
          |> List.iter (fun e ->
                 Format.fprintf ppf ", ";
                 pp_expression "" ppf e ~module_);
          Format.fprintf ppf ")" )
  | Expr_tuple [] -> Format.fprintf ppf "ok"
  | Expr_tuple [ e ] ->
      Format.fprintf ppf "{";
      pp_expression prefix ppf e ~module_;
      Format.fprintf ppf "}"
  | Expr_tuple (e :: es) ->
      Format.fprintf ppf "{";
      pp_expression "" ppf e ~module_;
      es
      |> List.iter (fun e ->
             Format.fprintf ppf ", ";
             pp_expression "" ppf e ~module_);
      Format.fprintf ppf "}"
  | Expr_nil | Expr_list [] -> Format.fprintf ppf "[]"
  | Expr_list [ e ] ->
      Format.fprintf ppf "[";
      pp_expression prefix ppf e ~module_;
      Format.fprintf ppf "]"
  | Expr_cons (l, r) ->
      Format.fprintf ppf "[";
      ( match l with
      | [] -> raise Invalid_cons_with_no_left_hand_side
      | x :: xs ->
          pp_expression "" ppf x ~module_;
          List.iter
            (fun e ->
              Format.fprintf ppf ", ";
              pp_expression "" ppf e ~module_)
            xs );
      Format.fprintf ppf " | ";
      pp_expression "" ppf r ~module_;
      Format.fprintf ppf "]"
  | Expr_list (e :: es) ->
      Format.fprintf ppf "[";
      pp_expression "" ppf e ~module_;
      es
      |> List.iter (fun e ->
             Format.fprintf ppf " | ";
             pp_expression "" ppf e ~module_);
      Format.fprintf ppf "]"
  | Expr_recv { rcv_cases; rcv_after } ->
      Format.fprintf ppf "receive";
      pp_case_branches prefix ppf rcv_cases ~module_;
      ( match rcv_after with
      | None -> ()
      | Some cb ->
          Format.fprintf ppf "after";
          pp_case_branches prefix ppf [ cb ] ~module_ );
      Format.fprintf ppf "end"
  | Expr_if branches ->
      Format.fprintf ppf "if ";
      pp_if_case_branches prefix ppf branches ~module_;
      Format.fprintf ppf "\n%send" prefix
  | Expr_case (expr, branches) ->
      Format.fprintf ppf "case ";
      pp_expression "" ppf expr ~module_;
      Format.fprintf ppf " of";
      pp_case_branches prefix ppf branches ~module_;
      Format.fprintf ppf "\n%send" prefix
  | Expr_map fields -> (
      let padding = H.pad (String.length prefix + 1) in
      match fields with
      | [] -> Format.fprintf ppf "#{}"
      | { mf_name; mf_value; _ } :: fs -> (
          Format.fprintf ppf "#{ %a => " (pp_expression "" ~module_) mf_name;
          pp_expression "" ppf mf_value ~module_;
          match fs with
          | [] -> Format.fprintf ppf " }"
          | fs ->
              Format.fprintf ppf "\n";
              fs
              |> List.iter (fun { mf_name; mf_value; _ } ->
                     Format.fprintf ppf "%s, %a => " padding
                       (pp_expression "" ~module_)
                       mf_name;
                     pp_expression "" ppf mf_value ~module_;
                     Format.fprintf ppf "\n");
              Format.fprintf ppf "%s}" padding ) )
  | Expr_map_update (m, fields) -> (
      let padding = H.pad (String.length prefix + 1) in
      pp_expression "" ppf m ~module_;
      match fields with
      | [] -> Format.fprintf ppf "#{}"
      | { mf_name; mf_value; _ } :: fs -> (
          Format.fprintf ppf "#{ %a := " (pp_expression "" ~module_) mf_name;
          pp_expression "" ppf mf_value ~module_;
          match fs with
          | [] -> Format.fprintf ppf " }"
          | fs ->
              Format.fprintf ppf "\n";
              fs
              |> List.iter (fun { mf_name; mf_value; _ } ->
                     Format.fprintf ppf "%s, %a := " padding
                       (pp_expression "" ~module_)
                       mf_name;
                     pp_expression "" ppf mf_value ~module_;
                     Format.fprintf ppf "\n");
              Format.fprintf ppf "%s}" padding ) )

and pp_fun_args ppf args =
  match args with
  | [] -> ()
  | [ arg ] -> if arg = Pattern_tuple [] then () else pp_pattern_match ppf arg
  | p :: ps ->
      pp_pattern_match ppf p;
      ps
      |> List.iter (fun pat ->
             Format.fprintf ppf ", ";
             pp_pattern_match ppf pat)

and pp_fun_case _prefix ppf { c_lhs; c_rhs; _ } ~module_ =
  Format.fprintf ppf "(";
  pp_fun_args ppf c_lhs;
  Format.fprintf ppf ") ->";
  let prefix =
    match c_rhs with
    | Expr_if _ | Expr_try _ | Expr_comment _ | Expr_map _ | Expr_let _
    | Expr_case _ | Expr_recv _ ->
        Format.fprintf ppf "\n";
        "  "
    | Expr_map_update (_, _)
    | Expr_catch _ | Expr_macro _ | Expr_nil | Expr_fun _ | Expr_apply _
    | Expr_fun_ref _ | Expr_list _ | Expr_tuple _ | Expr_cons _ | Expr_literal _
    | Expr_name _ ->
        " "
  in
  pp_expression prefix ppf c_rhs ~module_

let pp_fun_cases prefix ppf fd_name fd_cases ~module_ =
  match fd_cases with
  | [] -> raise (Function_without_cases (Atom.to_string fd_name))
  | [ c ] -> pp_fun_case prefix ppf c ~module_
  | c :: cs ->
      pp_fun_case prefix ppf c ~module_;
      cs
      |> List.iter (fun case ->
             Format.fprintf ppf ";\n%a" pp_atom fd_name;
             pp_fun_case prefix ppf case ~module_)

let pp_fun_spec prefix ppf name args return =
  Format.fprintf ppf "-spec %a(" pp_atom name;
  ( match args with
  | [] -> ()
  | x :: xs ->
      pp_type_expr prefix ppf x;
      xs
      |> List.iter (fun arg ->
             Format.fprintf ppf ", ";
             pp_type_expr prefix ppf arg) );
  Format.fprintf ppf ") -> %a.\n" (pp_type_expr prefix) return

let pp_function ppf { fd_name; fd_cases; fd_spec; _ } ~module_ =
  let prefix = Format.sprintf "%s" (Atom.to_string fd_name) in
  ( match fd_spec with
  | Some (Type_function { tyfun_args; tyfun_return }) ->
      pp_fun_spec prefix ppf fd_name tyfun_args tyfun_return
  | _ -> () );
  Format.fprintf ppf "%s" prefix;
  pp_fun_cases prefix ppf fd_name fd_cases ~module_;
  Format.fprintf ppf ".\n\n"

let pp_functions ppf funcs ~module_ =
  funcs |> List.iter (pp_function ppf ~module_)

let pp_types ppf types =
  types
  |> List.iter (fun { typ_kind; typ_name; typ_expr; typ_params; _ } ->
         let visibility =
           match typ_kind with
           | Opaque -> "opaque"
           | Type -> "type"
           | Spec -> "spec"
           | Callback -> "callback"
         in
         let params =
           let buf = Buffer.create 1024 in
           let f = Format.formatter_of_buffer buf in
           Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
             (pp_type_expr "") f typ_params;
           Format.fprintf f "%!";
           Buffer.contents buf
         in
         let prefix =
           Format.sprintf "-%s %s(%s) :: " visibility (Atom.to_string typ_name)
             params
         in
         Format.fprintf ppf "%s" prefix;
         pp_type_expr prefix ppf typ_expr;
         Format.fprintf ppf ".\n\n")

let pp ppf ({ module_name; exports; types; functions; _ } as m) =
  match exports with
  | [] -> ()
  | _ ->
      Format.fprintf ppf "-module(%a).\n" pp_atom module_name;
      pp_exports ppf exports;
      pp_types ppf types;
      pp_functions ppf functions ~module_:m;
      ()

let to_source_file erlmod =
  let _ = print_string ("Compiling " ^ erlmod.file_name ^ "\t") in
  let erlfile = erlmod.file_name in
  let oc = open_out_bin erlfile in
  ( try
      let f = Format.formatter_of_out_channel oc in
      Format.fprintf f "%% Source code generated with Caramel.\n";
      Format.fprintf f "%a@\n%!" pp erlmod
    with _ -> Sys.remove erlfile );
  print_string "OK\n";
  close_out oc

let to_sources erlmods =
  erlmods
  |> List.filter (fun { exports; _ } -> exports <> [])
  |> List.iter to_source_file
