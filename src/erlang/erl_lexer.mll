(**
	* The lexer definition. This is largely inspired by the Js_of_ocaml Lexer.
	*)

{
open Erl_parser

exception Error of (Parse_info.t * string)

let error lexbuf e =
	let info = Parse_info.t_of_lexbuf lexbuf in
  raise (Error (info, e))

(* The table of keywords *)
let keyword_table =
  let h = Hashtbl.create 6 in
  List.iter (fun (s,f) -> Hashtbl.add h s f ) [
    "case", (fun i -> CASE i);
    "receive", (fun i -> RECEIVE i);
    "after", (fun i -> AFTER i);
    "of", (fun i -> OF i);
    "end", (fun i -> END i);
    "fun", (fun i -> FUN i);
  ];
  h

let update_loc lexbuf ?file ~line ~absolute chars =
  let pos = lexbuf.Lexing.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.Lexing.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
                              }

let tokinfo lexbuf = Parse_info.t_of_lexbuf lexbuf

let or_else a b = match b with | Some c -> c | None -> a
}

let newline = ('\013' * '\010' | "\r" | "\n" | "\r\n")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']

let digit = [ '0'-'9' ]
let number = digit digit*

let float = number '.' number

let atom = lowercase ['A'-'Z' 'a'-'z' '_' '0'-'9' '@']*

let variable = ['_' 'A'-'Z'] ['A'-'Z' 'a'-'z' '_' '0'-'9' ]*

rule token = parse
  | newline { update_loc lexbuf ~line:1 ~absolute:false 0; token lexbuf }
  | blank + { token lexbuf }
  | float as float { FLOAT (float, tokinfo lexbuf) }
  | number as number { INTEGER (number, tokinfo lexbuf) }
  | "!" { BANG (tokinfo lexbuf) }
  | "." { DOT (tokinfo lexbuf) }
  | "," { COMMA (tokinfo lexbuf) }
  | ":" { COLON (tokinfo lexbuf) }
  | "=" { EQUAL (tokinfo lexbuf) }
  | "::" { COLON_COLON (tokinfo lexbuf) }
  | ";" { SEMICOLON (tokinfo lexbuf) }
  | "-" { DASH (tokinfo lexbuf) }
  | "|" { PIPE (tokinfo lexbuf) }
  | "/" { SLASH (tokinfo lexbuf) }
  | "(" { LEFT_PARENS (tokinfo lexbuf) }
  | ")" { RIGHT_PARENS (tokinfo lexbuf) }
  | "[" { LEFT_BRACKET (tokinfo lexbuf) }
  | "]" { RIGHT_BRACKET (tokinfo lexbuf) }
  | "{" { LEFT_BRACE (tokinfo lexbuf) }
  | "}" { RIGHT_BRACE (tokinfo lexbuf) }
  | "->" { ARROW (tokinfo lexbuf) }
  | "<<" { BINARY_OPEN (tokinfo lexbuf) }
  | ">>" { BINARY_CLOSE (tokinfo lexbuf) }
  | "\'" { read_atom (Buffer.create 1024) lexbuf }
  | "\"" { read_string (Buffer.create 1024) lexbuf }
  | atom as atom {
			let a = (Hashtbl.find_opt keyword_table atom)
							|> or_else (fun i -> ATOM (atom, i))
			in a (tokinfo lexbuf)
	}
  | variable as variable { VARIABLE (variable, tokinfo lexbuf) }
  | eof { EOF (tokinfo lexbuf) }

(* NOTE: this is naively copied from read_string and should be restricted to
 * valid atom characters *)
and read_atom buf = parse
  | '''       { ATOM (Buffer.contents buf, tokinfo lexbuf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_atom buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_atom buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_atom buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_atom buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_atom buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_atom buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_atom buf lexbuf }
  | [^ ''' '\\']+ {
    Buffer.add_string buf (Lexing.lexeme lexbuf); read_atom buf lexbuf }
  | _ { error lexbuf ("Illegal atom character: " ^ Lexing.lexeme lexbuf) }
  | eof { error lexbuf "Unterminated_string" }


and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf, tokinfo lexbuf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+ {
    Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _ { error lexbuf ("Illegal string character: " ^ Lexing.lexeme lexbuf) }
  | eof { error lexbuf "Unterminated_string" }
