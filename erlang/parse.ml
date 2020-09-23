(* Entry points in the parser *)

(* Skip tokens to the end of the phrase *)

let last_token = ref Parser.EOF

let token lexbuf =
  let token = Lexer.token lexbuf in
  last_token := token;
  token
