open Cmdliner
open Caramel_newsyntax

let name = "parse"

let doc = "Parse Caramel code"

let description = {||}

let info = Info.make ~name ~doc ~description

let run file dump_tokens dump_parsetree dump_caml debug =
  match
    Syntax.parse
      {
        source_file = Fpath.v file;
        debug;
        dump_tokens;
        dump_parsetree;
        dump_caml;
      }
  with
  | Ok _ -> 0
  | Error `Early_exit -> 0
  | Error (`Lexer_error err) ->
      Logs.err (fun f -> f "Lexing error: %a" Syntax.Lexer.pp_error err);
      1
  | Error (`Parse_error err) ->
      Logs.err (fun f -> f "Parsing error: %a" Syntax.Parser.Error.pp err);
      2
  | Error (`Msg msg) ->
      Logs.err (fun f -> f "Msg: %s" msg);
      99

let cmd =
  let file =
    Arg.(
      value & opt non_dir_file ""
      & info [ "f"; "file" ] ~docv:"FILE" ~doc:"A source file to parse")
  in
  let dump_tokens =
    Arg.(value & flag & info [ "dump-tokens" ] ~docv:"DUMP_TOKENS")
  in
  let dump_parsetree =
    Arg.(value & flag & info [ "dump-parsetree" ] ~docv:"DUMP_PARSETREE")
  in
  let dump_caml = Arg.(value & flag & info [ "dump-caml" ] ~docv:"DUMP_CAML") in
  ( Term.(
      pure run $ file $ dump_tokens $ dump_parsetree $ dump_caml
      $ Common_flags.debug),
    info )
