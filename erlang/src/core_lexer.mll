{
open Core_parser

exception Error of (Parse_info.t * string)

let error lexbuf e =
    let info = Parse_info.t_of_lexbuf lexbuf in
    raise (Error (info, e))

let next_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
        { pos with pos_bol = pos.pos_cnum;
                   pos_lnum = pos.pos_lnum + 1
        }

let char_of_octal o =
    let v = int_of_string o in
    let v = (v / 100) * 64 + ((v / 10) mod 10) * 8 + (v mod 10) in
    char_of_int v

let char_of_ctrlchar c = char_of_int ((int_of_char c) - 64)

let char_of_escapechar = function
    | 'b' -> '\b'
    | 'd' -> '\127'
    | 'e' -> '\027'
    | 'f' -> '\012'
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 's' -> ' '
    | 't' -> '\t'
    | 'v' -> '\011'
    | '"' -> '"'
    | '\'' -> '\''
    | '\\' -> '\\'
    | _ -> '\000'

let tokinfo lexbuf = Parse_info.t_of_lexbuf lexbuf
}

let sign = ['+' '-']
let digit = ['0'-'9']
let uppercase = ['A'-'Z' '\xc0'-'\xd6' '\xd8'-'\xde']
let lowercase = ['a'-'z' '\xdf'-'\xf6' '\xf8'-'\xff']
let atomchar = [^ '\r' '\n' '\x00'-'\x1f' '\\' '\'']
let charchar = [^ '\r' '\n' '\x00'-'\x1f' '\\' ' ']
let stringchar = [^ '\r' '\n' '\x00'-'\x1f' '\\' '"']
let namechar = uppercase | lowercase | digit | '@' | '_'
let octaldigit = ['0'-'7']
let octal = octaldigit (octaldigit octaldigit?)?
let ctrlchar = ['\x40'-'\x5f']
let escapechar = ['b' 'd' 'e' 'f' 'n' 'r' 's' 't' 'v' '"' ''' '\\']

let integer = sign? digit+
let float = sign? digit+ '.' digit+ (('E'|'e') sign? digit+)?
let variable = (uppercase | ('_' namechar)) namechar*

let atom = '''
let char = '$'
let string = '"'
let comment = '%' [^ '\r' '\n']*

let newline = '\r'* '\n' | '\r'
let blank = [' ' '\t' '\012']

rule read = parse
    | comment { read lexbuf }
    | newline { next_line lexbuf; read lexbuf }
    | blank + { read lexbuf }
    | float as f { FLOAT (float_of_string(f), tokinfo lexbuf) }
    | integer as integer { INTEGER (int_of_string(integer), tokinfo lexbuf) }
    | atom { read_atom (Buffer.create 1024) lexbuf }
    | string { read_string (Buffer.create 1024) lexbuf }
    | char { read_char lexbuf }
    | variable as variable { VARIABLE (variable, tokinfo lexbuf) }
    | "after" { AFTER (tokinfo lexbuf) }
    | "apply" { APPLY (tokinfo lexbuf) }
    | "attributes" { ATTRIBUTES (tokinfo lexbuf) }
    | "call" { CALL (tokinfo lexbuf) }
    | "case" { CASE (tokinfo lexbuf) }
    | "catch" { CATCH (tokinfo lexbuf) }
    | "do" { DO (tokinfo lexbuf) }
    | "end" { END (tokinfo lexbuf) }
    | "fun" { FUN (tokinfo lexbuf) }
    | "in" { IN (tokinfo lexbuf) }
    | "let" { LET (tokinfo lexbuf) }
    | "letrec" { LETREC (tokinfo lexbuf) }
    | "module" { MODULE (tokinfo lexbuf) }
    | "of" { OF (tokinfo lexbuf) }
    | "primop" { PRIMOP (tokinfo lexbuf) }
    | "receive" { RECEIVE (tokinfo lexbuf) }
    | "try" { TRY (tokinfo lexbuf) }
    | "when" { WHEN (tokinfo lexbuf) }
    | "(" { LEFT_PARENS (tokinfo lexbuf) }
    | ")" { RIGHT_PARENS (tokinfo lexbuf) }
    | "{" { LEFT_BRACE (tokinfo lexbuf) }
    | "}" { RIGHT_BRACE (tokinfo lexbuf) }
    | "[" { LEFT_BRACKET (tokinfo lexbuf) }
    | "]" { RIGHT_BRACKET (tokinfo lexbuf) }
    | "<" { LEFT_ANGLE (tokinfo lexbuf) }
    | ">" { RIGHT_ANGLE (tokinfo lexbuf) }
    | "|" { PIPE (tokinfo lexbuf) }
    | "#" { HASH (tokinfo lexbuf) }
    | "," { COMMA (tokinfo lexbuf) }
    | ":" { COLON (tokinfo lexbuf) }
    | "/" { SLASH (tokinfo lexbuf) }
    | "=" { EQUAL (tokinfo lexbuf) }
    | "->" { ARROW (tokinfo lexbuf) }
    | "-|" { DASH_PIPE (tokinfo lexbuf) }
    | _ { error lexbuf ("Unexpected character: " ^ Lexing.lexeme lexbuf) }
    | eof { EOF (tokinfo lexbuf) }

and read_atom buf = parse
    | '''                       { ATOM (Buffer.contents buf, tokinfo lexbuf) }
    | '\\' (octal as o)         { Buffer.add_char buf (char_of_octal o); read_atom buf lexbuf }
    | '\\' '^' (ctrlchar as c)  { Buffer.add_char buf (char_of_ctrlchar c); read_atom buf lexbuf }
    | '\\' (escapechar as c)    { Buffer.add_char buf (char_of_escapechar c); read_atom buf lexbuf }
    | [^ '\r' '\n' '\x00'-'\x1f' ''' '\\']+ {
        Buffer.add_string buf (Lexing.lexeme lexbuf); read_atom buf lexbuf }
    | _ { error lexbuf ("Illegal atom character: " ^ Lexing.lexeme lexbuf) }
    | eof { error lexbuf "Unterminated atom" }

and read_string buf = parse
    | '"'                       { STRING (Buffer.contents buf, tokinfo lexbuf) }
    | '\\' (octal as o)         { Buffer.add_char buf (char_of_octal o); read_string buf lexbuf }
    | '\\' '^' (ctrlchar as c)  { Buffer.add_char buf (char_of_ctrlchar c); read_string buf lexbuf }
    | '\\' (escapechar as c)    { Buffer.add_char buf (char_of_escapechar c); read_string buf lexbuf }
    | [^ '\r' '\n' '\x00'-'\x1f' '"' '\\']+ {
        Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
    | _ { error lexbuf ("Illegal string character: " ^ Lexing.lexeme lexbuf) }
    | eof { error lexbuf "Unterminated string" }

and read_char = parse
    | '\\' (octal as o)                     { CHAR (char_of_octal o, tokinfo lexbuf) }
    | '\\' '^' (ctrlchar as c)              { CHAR (char_of_ctrlchar c, tokinfo lexbuf) }
    | '\\' (escapechar as c)                { CHAR (char_of_escapechar c, tokinfo lexbuf) }
    | [^ '\r' '\n' '\x00'-'\x1f' ' ' '\\']  { CHAR ((Lexing.lexeme lexbuf).[0], tokinfo lexbuf) }
    | _ { error lexbuf "Unexpected character: $" }

