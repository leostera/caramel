type t =
  | AFTER of Parse_info.t
  | AND of Parse_info.t
  | ANDALSO of Parse_info.t
  | ARROW of Parse_info.t
  | FAT_ARROW of Parse_info.t
  | ATOM of string * Parse_info.t
  | BAND of Parse_info.t
  | BANG of Parse_info.t
  | BEGIN of Parse_info.t
  | BINARY_CLOSE of Parse_info.t
  | BINARY_OPEN of Parse_info.t
  | BNOT of Parse_info.t
  | BOR of Parse_info.t
  | BSL of Parse_info.t
  | BSR of Parse_info.t
  | BXOR of Parse_info.t
  | CASE of Parse_info.t
  | CATCH of Parse_info.t
  | CHAR of string * Parse_info.t
  | COLON of Parse_info.t
  | COLON_COLON of Parse_info.t
  | COLON_EQUAL of Parse_info.t
  | COMMA of Parse_info.t
  | COMMENT of string * Parse_info.t
  | DASH of Parse_info.t
  | DIV of Parse_info.t
  | DOT of Parse_info.t
  | END of Parse_info.t
  | EQUAL of Parse_info.t
  | EQUAL_COLON_EQUAL of Parse_info.t
  | EQUAL_EQUAL of Parse_info.t
  | EQUAL_SLASH_EQUAL of Parse_info.t
  | FLOAT of string * Parse_info.t
  | FUN of Parse_info.t
  | GT of Parse_info.t
  | GTE of Parse_info.t
  | HASH of Parse_info.t
  | IF of Parse_info.t
  | INTEGER of string * Parse_info.t
  | LEFT_BRACE of Parse_info.t
  | LEFT_BRACKET of Parse_info.t
  | LEFT_PARENS of Parse_info.t
  | LT of Parse_info.t
  | LTE of Parse_info.t
  | MACRO of string * Parse_info.t
  | MINUS of Parse_info.t
  | MINUS_MINUS of Parse_info.t
  | NOT of Parse_info.t
  | OF of Parse_info.t
  | OR of Parse_info.t
  | ORELSE of Parse_info.t
  | PIPE of Parse_info.t
  | PLUS of Parse_info.t
  | PLUS_PLUS of Parse_info.t
  | RECEIVE of Parse_info.t
  | REM of Parse_info.t
  | RIGHT_BRACE of Parse_info.t
  | RIGHT_BRACKET of Parse_info.t
  | RIGHT_PARENS of Parse_info.t
  | SEMICOLON of Parse_info.t
  | SLASH of Parse_info.t
  | SLASH_EQUAL of Parse_info.t
  | STAR of Parse_info.t
  | STRING of string * Parse_info.t
  | THROW of Parse_info.t
  | TRY of Parse_info.t
  | VARIABLE of string * Parse_info.t
  | WHEN of Parse_info.t
  | XOR of Parse_info.t
