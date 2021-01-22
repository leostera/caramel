open Core_ast

exception Can_not_print_that_yet

let aux_pp_list ppf fmt = function
  | [] -> ()
  | x :: xs ->
      fmt x;
      List.iter
        (fun v ->
          Format.fprintf ppf ",";
          fmt v)
        xs

let pp_var ppf x = Format.fprintf ppf "_%s" x

let pp_literal ppf lit =
  match lit with
  | Lit_integer int -> Format.fprintf ppf "%d" int
  | Lit_atom atom -> Format.fprintf ppf "'%s'" atom
  | _ -> raise Can_not_print_that_yet

let rec pp_expr ppf fe_body =
  match fe_body with
  | Expr_literal lit -> pp_literal ppf lit
  | Expr_var var -> pp_var ppf var
  | Expr_qualified_call { qc_mod; qc_fun; qc_args; _ } ->
      Format.fprintf ppf "call ";
      pp_expr ppf qc_mod;
      Format.fprintf ppf ":";
      pp_expr ppf qc_fun;
      Format.fprintf ppf "(";
      aux_pp_list ppf (pp_expr ppf) qc_args;
      Format.fprintf ppf ")"
  | _ -> raise Can_not_print_that_yet

let pp_fun_expr ppf { fe_vars; fe_body; _ } =
  Format.fprintf ppf "fun (";
  aux_pp_list ppf (pp_var ppf) fe_vars;
  Format.fprintf ppf ") -> ";
  pp_expr ppf fe_body

let pp_fname ppf { fn_name; fn_arity; _ } =
  Format.fprintf ppf "'%s'/%d" fn_name fn_arity

let pp_fun_def ppf { fd_name; fd_body; _ } =
  pp_fname ppf fd_name;
  Format.fprintf ppf " = ";
  pp_fun_expr ppf fd_body

let pp ppf core =
  Format.fprintf ppf "module '%s' " core.m_name;
  Format.fprintf ppf "[\n";
  List.iter
    (fun fname ->
      Format.fprintf ppf "  ";
      pp_fname ppf fname;
      Format.fprintf ppf ",\n")
    core.m_fnames;
  Format.fprintf ppf "  'module_info'/0,\n";
  Format.fprintf ppf "  'module_info'/1]\n";
  Format.fprintf ppf "attributes []";
  Format.fprintf ppf "\n\n";
  List.iter (pp_fun_def ppf) core.m_defs;
  Format.fprintf ppf "%s"
    {|'module_info'/0 =
  ( fun () ->
  call 'erlang':'get_module_info'
      ('expected')
    -| [{'function',{'module_info',0; }; }] )

'module_info'/1 =
  ( fun (_0) ->
  call 'erlang':'get_module_info'
      ('expected', ( _0
         -| [{'function',{'module_info',1; }; }] ))
    -| [{'function',{'module_info',1; }; }] )

end |}

let to_source_file coremod =
  let _ = print_string ("Compiling " ^ coremod.m_filename ^ "\t") in
  let corefile = coremod.m_filename in
  let oc = open_out_bin corefile in
  (try
     let f = Format.formatter_of_out_channel oc in
     Format.fprintf f "%% Source code generated with Caramel.\n";
     Format.fprintf f "%a@\n%!" pp coremod
   with _ -> Sys.remove corefile);
  print_string "OK\n";
  close_out oc

let to_sources coremod = List.iter to_source_file coremod
