type t =
  | AFTER of Parse_info.t
  | ARROW of Parse_info.t
  | ATOM of string * Parse_info.t
  | BANG of Parse_info.t
  | BINARY_CLOSE of Parse_info.t
  | BINARY_OPEN of Parse_info.t
  | CASE of Parse_info.t
  | CHAR of string * Parse_info.t
  | COLON of Parse_info.t
  | COLON_COLON of Parse_info.t
  | COMMA of Parse_info.t
  | DASH of Parse_info.t
  | DOT of Parse_info.t
  | END of Parse_info.t
  | EQUAL of Parse_info.t
  | FLOAT of string * Parse_info.t
  | FUN of Parse_info.t
  | INTEGER of string * Parse_info.t
  | LEFT_BRACE of Parse_info.t
  | LEFT_BRACKET of Parse_info.t
  | LEFT_PARENS of Parse_info.t
  | OF of Parse_info.t
  | PIPE of Parse_info.t
  | RECEIVE of Parse_info.t
  | RIGHT_BRACE of Parse_info.t
  | RIGHT_BRACKET of Parse_info.t
  | RIGHT_PARENS of Parse_info.t
  | SEMICOLON of Parse_info.t
  | SLASH of Parse_info.t
  | STRING of string * Parse_info.t
  | VARIABLE of string * Parse_info.t
