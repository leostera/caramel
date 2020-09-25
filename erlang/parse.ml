let from_file source_file =
  let ic = open_in_bin source_file in
  let lexbuf = Lexing.from_channel ic in
  match Parser.module_file Lexer.token lexbuf with
  | exception Lexer.Error (err, loc) -> Error (`Lexer_error (err, loc))
  | exception Parser.Error ->
      let msg =
        Printf.sprintf "In %s, at offset %d: syntax error.\n%!"
          source_file
          (Lexing.lexeme_start lexbuf)
      in
      Error (`Parser_error msg)
  | exception exc ->
      let msg =
        Printf.sprintf "In %s, at offset %d: syntax error.\n  %s%!"
          source_file
          (Lexing.lexeme_start lexbuf)
          (Printexc.to_string exc)
      in
      Error (`Parser_error msg)
  | x -> x
