(* Exported modules *)
module Lexer = Lexer
module Parse_error = Parse_error
module Parser = Parser
module Parsetree = Parsetree
module Macro_expander = Macro_expander

type parse_unit = {
  source_file : Fpath.t;
  debug : bool;
  dump_tokens : bool;
  dump_parsetree : bool;
  dump_caml : bool;
  dump_macro_env : bool;
  dump_expanded : bool;
}

let parse
    {
      source_file;
      dump_tokens;
      dump_parsetree;
      dump_caml;
      dump_macro_env;
      dump_expanded;
      _;
    } =
  let ( let* ) = Result.bind in

  let* contents = Bos.OS.File.read source_file in
  let lexer = Lexer.make ~filename:(Fpath.to_string source_file) ~contents in

  (* NOTE: used for debugging purposes *)
  let* () =
    if dump_tokens then (
      let* tokens = Lexer.tokenize lexer in
      Logs.debug (fun f -> f "%a" Token.pp_tokens tokens);
      Error `Early_exit)
    else Ok ()
  in

  let* parser = Parser.make ~lexer in
  let* parsetree = Parser.parse parser in

  (* NOTE: used for debugging purposes *)
  let* () =
    if dump_parsetree then (
      Logs.debug (fun f -> f "%a" Parsetree.pp parsetree);
      Error `Early_exit)
    else Ok ()
  in

  let* parsetree = Macros.run { dump_macro_env; dump_expanded; parsetree } in

  let caml_parsetree = Translate.to_caml_parsetree parsetree in

  (* NOTE: used for debugging purposes *)
  let* () =
    if dump_caml then (
      Logs.debug (fun f -> f "%a" Pprintast.structure caml_parsetree);
      Error `Early_exit)
    else Ok ()
  in

  Ok caml_parsetree
