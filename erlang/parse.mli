val from_file :
  string ->
  ( Ast.t,
    [> `Lexer_error of Lexer.error * Location.t | `Parser_error of string ] )
  result
