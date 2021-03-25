open Cmdliner

module Info = struct
  let make ~name ~doc ~description =
    let man =
      [
        `S "DESCRIPTION";
        `P description;
        `S "SEE ALSO";
        `P "ocaml(1) erlang";
        `S "AUTHORS";
        `P "Leandro Ostera.";
        `S "LICENSE";
        `P "Copyright (C) 2020, Abstract Machines Lab Sweden AB";
        `P "Erlang is licensed under Apache License 2.0";
      ]
    in
    let version =
      match Version.git_version with
      | "" -> Version.s
      | v -> Printf.sprintf "%s+git-%s" Version.s v
    in
    Term.info name ~version ~doc ~man
end

module Rebind = struct
  let errored = ref false

  open Erlang.Ast

  let name = "rebind"

  let doc = "Helper command to parse sources and dump ASTs"

  let description = {| |}

  let info = Info.make ~name ~doc ~description

  let add_or_complain ~symbols name =
    match Hashtbl.find_opt symbols name with
    | None -> Hashtbl.add symbols name 0
    | Some _ ->
        Format.fprintf Format.std_formatter "We found a rebinding of: %a\n%!"
          Erlang.Printer.pp_name name;
        errored := true

  let rec check_rebind_in_pattern ~symbols pat =
    match pat with
    | Pattern_binding name -> add_or_complain ~symbols name
    | Pattern_cons (ph, pt) ->
        List.iter (check_rebind_in_pattern ~symbols) (ph @ [ pt ])
    | Pattern_ignore -> ()
    | Pattern_list pats -> List.iter (check_rebind_in_pattern ~symbols) pats
    | Pattern_map kvs ->
        List.iter (fun (k, _) -> check_rebind_in_pattern ~symbols k) kvs
    | Pattern_match _ -> ()
    | Pattern_tuple pats -> List.iter (check_rebind_in_pattern ~symbols) pats
    | Pattern_catch (_, _, _) -> ()
    | Pattern_with_name (pl, pr) ->
        List.iter (check_rebind_in_pattern ~symbols) [ pl; pr ]

  let rec check_rebind_in_expression ~symbols expr =
    match expr with
    | Expr_case (_, cases) -> List.iter (check_rebind_in_case ~symbols) cases
    | Expr_apply _ -> ()
    | Expr_catch _ -> ()
    | Expr_comment _ -> ()
    | Expr_cons (_, expr) -> check_rebind_in_expression ~symbols expr
    | Expr_fun cases -> List.iter (check_rebind_in_case ~symbols) cases
    | Expr_fun_ref _ -> ()
    | Expr_if cases ->
        List.iter
          (fun (_, expr) -> check_rebind_in_expression ~symbols expr)
          cases
    | Expr_let ({ lb_lhs; lb_rhs }, expr) ->
        check_rebind_in_pattern ~symbols lb_lhs;
        check_rebind_in_expression ~symbols lb_rhs;
        check_rebind_in_expression ~symbols expr
    | Expr_list exprs -> List.iter (check_rebind_in_expression ~symbols) exprs
    | Expr_literal _ -> ()
    | Expr_macro _ -> ()
    | Expr_map kvs ->
        List.iter
          (fun { mf_value; _ } -> check_rebind_in_expression ~symbols mf_value)
          kvs
    | Expr_map_update (expr, kvs) ->
        check_rebind_in_expression ~symbols expr;
        List.iter
          (fun { mf_value; _ } -> check_rebind_in_expression ~symbols mf_value)
          kvs
    | Expr_name _ -> ()
    | Expr_nil -> ()
    | Expr_recv { rcv_cases; _ } ->
        List.iter (check_rebind_in_case ~symbols) rcv_cases
    | Expr_seq exprs ->
      List.iter (check_rebind_in_expression ~symbols) exprs
    | Expr_try _ -> ()
    | Expr_tuple exprs -> List.iter (check_rebind_in_expression ~symbols) exprs

  and check_rebind_in_case { c_lhs; c_rhs; _ } ~symbols =
    List.iter (check_rebind_in_pattern ~symbols) c_lhs;
    check_rebind_in_expression ~symbols c_rhs

  let check_rebinds module_item =
    match module_item with
    | Module_comment _ -> ()
    | Module_attribute _ -> ()
    | Type_decl _ -> ()
    | Function_decl { fd_cases; _ } ->
        List.iter
          (fun case ->
            let symbols : (Erlang.Ast.name, int) Hashtbl.t =
              Hashtbl.create 1024
            in
            check_rebind_in_case case ~symbols)
          fd_cases

  let check source =
    match Erlang.Parse.from_file source with
    | Ok structure ->
        List.iter check_rebinds structure;
        if !errored then 1 else 0
    | Error (`Parser_error err) ->
        Format.fprintf Format.std_formatter "ERROR: %s%!\n" err;
        Format.fprintf Format.std_formatter "\n%!";
        1

  let run sources =
    List.fold_left
      (fun exit_code src -> if exit_code = 0 then check src else exit_code)
      0 sources

  let cmd =
    let sources =
      Arg.(
        non_empty & pos_all string []
        & info [] ~docv:"SOURCES" ~doc:"A list of source files to parse")
    in
    (Term.(pure run $ sources), info)
end

module Help = struct
  let info name =
    let doc = "Erlang AST dump tool" in
    let description =
      "A small tool to inspect the concrete AST of Erlang sources"
    in
    Info.make ~name ~doc ~description

  let cmd = (Term.(ret (const (`Help (`Pager, None)))), info "erlcheck")
end

let () =
  let cmds = [ Rebind.cmd ] in
  Term.(exit_status @@ eval_choice Help.cmd cmds)
