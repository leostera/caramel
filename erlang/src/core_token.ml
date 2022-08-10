type t =
    | AFTER of Parse_info.t 
    | APPLY of Parse_info.t
    | ARROW of Parse_info.t
    | ATOM of string * Parse_info.t
    | ATTRIBUTES of Parse_info.t
    | CALL of Parse_info.t
    | CASE of Parse_info.t
    | CATCH of Parse_info.t
    | CHAR of char * Parse_info.t
    | COLON of Parse_info.t
    | COMMA of Parse_info.t
    | DASH_PIPE of Parse_info.t
    | DO of Parse_info.t
    | END of Parse_info.t
    | EOF of Parse_info.t
    | EQUAL of Parse_info.t
    | FLOAT of float * Parse_info.t
    | FUN of Parse_info.t
    | HASH of Parse_info.t
    | IN of Parse_info.t
    | INTEGER of int * Parse_info.t
    | LEFT_ANGLE of Parse_info.t
    | LEFT_BRACE of Parse_info.t
    | LEFT_BRACKET of Parse_info.t
    | LEFT_PARENS of Parse_info.t
    | LET of Parse_info.t
    | LETREC of Parse_info.t
    | MODULE of Parse_info.t
    | OF of Parse_info.t
    | PIPE of Parse_info.t
    | PRIMOP of Parse_info.t
    | RECEIVE of Parse_info.t
    | RIGHT_ANGLE of Parse_info.t
    | RIGHT_BRACE of Parse_info.t
    | RIGHT_BRACKET of Parse_info.t
    | RIGHT_PARENS of Parse_info.t
    | SLASH of Parse_info.t
    | STRING of string * Parse_info.t
    | TRY of Parse_info.t
    | VARIABLE of string * Parse_info.t
    | WHEN of Parse_info.t
