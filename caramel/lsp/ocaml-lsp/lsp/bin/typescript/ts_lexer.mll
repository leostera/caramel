{
  open! Import
  open Ts_parser
}

let ws = [' ' '\r' '\t' '\n']+

let digit = ['0' - '9']
let ident_first = ['a' - 'z' 'A' - 'Z' '_' ]
let ident_char = ident_first | digit

rule token = parse
  | "//" [^ '\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "/*" { comment lexbuf }
  | "extends" { Extends }
  | "const" { Const }
  | "export const" { Const }
  | "export enum" { Enum }
  | "export class" { Interface }
  | "type" { Type }
  | "export type" { Type }
  | "export interface" { Interface }
  | "interface" { Interface }
  | "export namespace" { Namespace }
  | "readonly" { Readonly }
  | '|' { Alt }
  | ';' { Semicolon }
  | '(' { L_paren }
  | ')' { R_paren }
  | '{' { L_curly }
  | '}' { R_curly }
  | ':' { Colon }
  | '=' { Equal }
  | '<' { L_angle }
  | '>' { R_angle }
  | ',' { Comma }
  | "[]" { Array_type }
  | '[' { L_square }
  | ']' { R_square }
  | '?' { Question }
  | '-'? digit* '.' digit+ { Float (Option.value_exn (Float.of_string (Lexing.lexeme lexbuf))) }
  | '-'? digit+ as i { Int (Int.of_string_exn i) }
  | '"' ([^ '"']* as s) '"' { String s }
  | '\'' ([^ '\'']* as s) '\'' { String s }
  | ident_first ident_char* { Ident (Lexing.lexeme lexbuf) }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']* { token lexbuf }
  | _ { failwith ("token: " ^ (Lexing.lexeme lexbuf)) }
  | eof { Eof }

and comment = parse
  | "*/" { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; comment lexbuf }
  | _ { comment lexbuf }
  | eof { failwith "unterminated comment" }
{
}
