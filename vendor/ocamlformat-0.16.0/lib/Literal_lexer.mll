(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

{
open Caml

(* To buffer string literals *)

let string_buffer = Buffer.create 256
let reset_string_buffer () = Buffer.reset string_buffer
let get_stored_string () = Buffer.contents string_buffer

let store_string_char c = Buffer.add_char string_buffer c
let store_string s = Buffer.add_string string_buffer s

exception Parse_error

}

let newline = ('\013'* '\010')
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']

rule string mode = parse
  | '\"'
      { reset_string_buffer ();
        string_aux mode lexbuf;
        get_stored_string () }
  | _ { raise Parse_error }


and string_aux mode = parse
    '\"'
      { () }
  | '\\' newline ([' ' '\t'] *)
      { string_aux mode lexbuf }
  | '\\' ['\\' '\'' '\"' 't' 'b' 'r']
      { store_string (Lexing.lexeme lexbuf);
        string_aux mode lexbuf }
  | "\\ "
      { begin match mode with
          | `Normalize -> store_string " "
          | `Preserve -> store_string "\\ " end;
        string_aux mode lexbuf }
  | "\t"
      { begin match mode with
          | `Normalize -> store_string "\\t"
          | `Preserve -> store_string "\t" end;
        string_aux mode lexbuf }
  | "\\n"
      { begin match mode with
          | `Normalize -> store_string "\n"
          | `Preserve -> store_string "\\n" end;
        string_aux mode lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string (Lexing.lexeme lexbuf);
        string_aux mode lexbuf }
  | '\\' 'o' ['0'-'3'] ['0'-'7'] ['0'-'7']
      { store_string (Lexing.lexeme lexbuf);
        string_aux mode lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { store_string (Lexing.lexeme lexbuf);
        string_aux mode lexbuf }
  | '\\' 'u' '{' hex_digit+ '}'
      { store_string (Lexing.lexeme lexbuf);
        string_aux mode lexbuf }
  | '\\' _
      { (*  Should be an error, but we are very lax.
            raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
            Location.curr lexbuf))
        *)
        store_string (Lexing.lexeme lexbuf);
        string_aux mode lexbuf
      }
  | newline
      { store_string (Lexing.lexeme lexbuf);
        string_aux mode lexbuf }
  | eof
      { raise Parse_error }
  | _
      { store_string_char (Lexing.lexeme_char lexbuf 0);
        string_aux mode lexbuf }

and char = parse
  | "\'" newline "\'"
      { "\\n" }
  | "\'" ([^ '\\' '\'' '\010' '\013'] as x) "\'"
      { String.make 1 x }
  | "\'" ("\\" ['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] as x) "\'"
      { x }
  | "\'" ("\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] as x) "\'"
      { x }
  | "\'" ("\\" 'o' ['0'-'3'] ['0'-'7'] ['0'-'7'] as x) "\'"
      { x }
  | "\'" ("\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] as x) "\'"
      { x }
  | _
      { raise Parse_error }

{
  let string mode s =
    let lexbuf = Lexing.from_string s in
    match string mode lexbuf with
    | s -> Some s
    | exception Parse_error -> None

  let char s =
    let lexbuf = Lexing.from_string s in
    match char lexbuf with
    | s -> Some s
    | exception Parse_error -> None
}
