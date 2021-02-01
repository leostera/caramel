let from_file parse source_file =
  let ic = open_in_bin source_file in
  let lexbuf = Lexing.from_channel ic in
  (* match Erl_parser.module_file Erl_lexer.token lexbuf with *)
  match parse Erl_lexer.token lexbuf with
  | exception exc ->
      let msg =
        Printf.sprintf "In %s, at offset %d: syntax error.\n  %s%!" source_file
          (Lexing.lexeme_start lexbuf)
          (Printexc.to_string exc)
      in
      Error (`Parser_error msg)
  | x -> Ok x

let exprs_from_file source_file = 
  let parse = Erl_parser.exprs_file in
  from_file parse source_file

let from_file source_file = 
  let parse = Erl_parser.module_file in
  from_file parse source_file
