let from_file source_file =
  let ic = open_in_bin source_file in
  let lexbuf = Lexing.from_channel ic in
  match Core_parser.annotated_module Core_lexer.read lexbuf with
  | exception exc ->
      let pos = (Lexing.lexeme_start_p lexbuf) in
      let msg =
        Printf.sprintf "In %s, at line %d, character %d: syntax error.\n  %s%!" source_file
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol)
          (Printexc.to_string exc)
      in
      Error (`Parser_error msg)
  | x -> Ok x
