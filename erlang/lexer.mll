(* The lexer definition *)

{
open Parser

type error =
  | Unterminated_string
  | Invalid_literal of string
;;

exception Error of error * Location.t;;
let error lexbuf e = raise (Error(e, Location.curr lexbuf))

let pp_err ppf err =
  match err with
  | Unterminated_string -> Format.fprintf ppf "Unterminated_string";
  | Invalid_literal lit -> Format.fprintf ppf "Invalid_literal: %s" lit

let update_loc lexbuf ?file ~line ~absolute chars =
  let pos = lexbuf.Lexing.lex_curr_p in
  let new_file = match file with
               | None -> pos.pos_fname
               | Some s -> s
  in
  lexbuf.Lexing.lex_curr_p <- {
    pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

(* The table of keywords *)
let keyword_table =
  let h = Hashtbl.create 32 in
  List.iter (fun (s,f) -> Hashtbl.add h s f ) [
    "case", CASE;
    "of", OF;
    "end", END;
    "fun", FUN;
  ];
  h

(* Update the current location with file name and line number. *)

let or_else a b = match b with | Some c -> c | None -> a
}

let newline = ('\013' * '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']

let digit = [ '0'-'9' ]
let number = digit digit*

let atom = lowercase ['A'-'Z' 'a'-'z' '_' '0'-'9' '@']*

rule token = parse
  | newline { update_loc lexbuf ~line:1 ~absolute:false 0; token lexbuf }
  | blank + { token lexbuf }
  | number as number { NUMBER number }
  | "." { DOT }
  | "," { COMMA }
  | ":" { COLON }
  | "::" { COLON_COLON }
  | ";" { SEMICOLON }
  | "-" { DASH }
  | "->" { ARROW }
  | "/" { SLASH }
  | "<<" { BINARY_OPEN }
  | ">>" { BINARY_CLOSE }
  | "(" { LEFT_PARENS }
  | ")" { RIGHT_PARENS }
  | "[" { LEFT_BRACKET }
  | "]" { RIGHT_BRACKET }
  | "{" { LEFT_BRACE }
  | "}" { RIGHT_BRACE }
  | "|" { PIPE }
  | "\"" { read_string (Buffer.create 1024) lexbuf }
  | atom as atom { (Hashtbl.find_opt keyword_table atom) |> or_else (ATOM atom) }
  | eof { EOF }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+ {
    Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _ { error lexbuf (Invalid_literal ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { error lexbuf Unterminated_string }

