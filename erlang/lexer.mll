(* The lexer definition *)

{
open Lexing
open Misc
open Parser

type error =
  | Illegal_character of char
  | Illegal_escape of string * string option
  | Reserved_sequence of string * string option
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Invalid_literal of string
  | Invalid_directive of string * string option
;;

exception Error of error * Location.t;;
let error lexbuf e = raise (Error(e, Location.curr lexbuf))

(* The table of keywords *)

let keyword_table =
  create_hashtable 149 [
  ]

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

let or_else a b = match b with | Some c -> c | None -> a
}

let newline = ('\013' * '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']

let atom = lowercase ['A'-'Z' 'a'-'z' '_' '0'-'9' '@']*

let module_attribute = "-" atom "(" atom ")."

rule token = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        EOL }
  | blank + { token lexbuf }
  | "." { DOT }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | "(" { LEFT_PARENS }
  | ")" { RIGHT_PARENS }
  | "->" { ARROW }
  | "<<" { BINARY_OPEN }
  | ">>" { BINARY_CLOSE }
  | "\"" { read_string (Buffer.create 1024) lexbuf }
  | atom as atom { (Hashtbl.find_opt keyword_table atom) |> or_else (ATOM atom) }
  | _ as c { print_char c; token lexbuf }
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
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { error lexbuf (Invalid_literal ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { error lexbuf Unterminated_string }

