(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

(* Implementation notes *********************************************

   * - This module should depend on OCaml's standard library only and
   * should be as 'pure OCaml' (i.e. depend as least as possible on
   * external tools) as possible.

   * - `while' loops are sometimes preferred to recursion because this
   * may be used on systems where tail recursion is not well
   * supported. (I tried to write "while" as often as possible, but it
   * turned out that it was pretty inconvenient, so I do use
   * recursion.  When I have time, I'll do some tests and see if I
   * need to convert recursive loops into iterative loops. Sorry if it
   * makes it harder to read.)

*)

(* class type tag = object method is_me : 'a. 'a -> bool end *)

open Omd_representation

type token = Omd_representation.tok
type t = Omd_representation.tok list

let string_of_token = function
  | Tag (name, o) ->
    if Omd_utils.debug then "TAG("^name^")" ^ o#to_string else o#to_string
  | Ampersand -> "&"
  | Ampersands n -> assert (n >= 0); String.make (2+n) '&'
  | At -> "@"
  | Ats n -> assert (n >= 0); String.make (2+n) '@'
  | Backquote -> "`"
  | Backquotes n -> assert (n >= 0); String.make (2+n) '`'
  | Backslash -> "\\"
  | Backslashs n -> assert (n >= 0); String.make (2+n) '\\'
  | Bar -> "|"
  | Bars n -> assert (n >= 0); String.make (2+n) '|'
  | Caret -> "^"
  | Carets n -> assert (n >= 0); String.make (2+n) '^'
  | Cbrace -> "}"
  | Cbraces n -> assert (n >= 0); String.make (2+n) '}'
  | Colon -> ":"
  | Colons n -> assert (n >= 0); String.make (2+n) ':'
  | Comma -> ","
  | Commas n -> assert (n >= 0); String.make (2+n) ','
  | Cparenthesis -> ")"
  | Cparenthesiss n -> assert (n >= 0); String.make (2+n) ')'
  | Cbracket -> "]"
  | Cbrackets n -> assert (n >= 0); String.make (2+n) ']'
  | Dollar -> "$"
  | Dollars n -> assert (n >= 0); String.make (2+n) '$'
  | Dot -> "."
  | Dots n -> assert (n >= 0); String.make (2+n) '.'
  | Doublequote -> "\""
  | Doublequotes n -> assert (n >= 0); String.make (2+n) '"'
  | Exclamation -> "!"
  | Exclamations n -> assert (n >= 0); String.make (2+n) '!'
  | Equal -> "="
  | Equals n -> assert (n >= 0); String.make (2+n) '='
  | Greaterthan -> ">"
  | Greaterthans n -> assert (n >= 0); String.make (2+n) '>'
  | Hash -> "#"
  | Hashs n -> assert (n >= 0); String.make (2+n) '#'
  | Lessthan -> "<"
  | Lessthans n -> assert (n >= 0); String.make (2+n) '<'
  | Minus -> "-"
  | Minuss n -> assert (n >= 0); String.make (2+n) '-'
  | Newline -> "\n"
  | Newlines n -> assert (n >= 0); String.make (2+n) '\n'
  | Number s -> s
  | Obrace -> "{"
  | Obraces n -> assert (n >= 0); String.make (2+n) '{'
  | Oparenthesis -> "("
  | Oparenthesiss n -> assert (n >= 0); String.make (2+n) '('
  | Obracket -> "["
  | Obrackets n -> assert (n >= 0); String.make (2+n) '['
  | Percent -> "%"
  | Percents n -> assert (n >= 0); String.make (2+n) '%'
  | Plus -> "+"
  | Pluss n -> assert (n >= 0); String.make (2+n) '+'
  | Question -> "?"
  | Questions n -> assert (n >= 0); String.make (2+n) '?'
  | Quote -> "'"
  | Quotes n -> assert (n >= 0); String.make (2+n) '\''
  | Semicolon -> ";"
  | Semicolons n -> assert (n >= 0); String.make (2+n) ';'
  | Slash -> "/"
  | Slashs n -> assert (n >= 0); String.make (2+n) '/'
  | Space -> " "
  | Spaces n -> assert (n >= 0); String.make (2+n) ' '
  | Star -> "*"
  | Stars n -> assert (n >= 0); String.make (2+n) '*'
  | Tab -> "    "
  | Tabs n -> assert (n >= 0); String.make ((2+n)*4) ' '
  | Tilde -> "~"
  | Tildes n -> assert (n >= 0); String.make (2+n) '~'
  | Underscore -> "_"
  | Underscores n -> assert (n >= 0); String.make (2+n) '_'
  | Word s -> s


let size_and_newlines = function
  | Tag _ -> (0, 0)
  | Ampersand | At | Backquote | Backslash | Bar | Caret | Cbrace
  | Colon | Comma | Cparenthesis | Cbracket | Dollar | Dot
  | Doublequote | Exclamation | Equal | Greaterthan | Hash | Lessthan
  | Minus | Obrace | Oparenthesis | Obracket | Percent | Plus
  | Question | Quote | Semicolon | Slash | Space | Star | Tab
  | Tilde | Underscore -> (1, 0)
  | Ampersands x | Ats x | Backquotes x | Backslashs x | Bars x | Carets x
  | Cbraces x | Colons x | Commas x | Cparenthesiss x | Cbrackets x
  | Dollars x | Dots x
  | Doublequotes x | Exclamations x | Equals x | Greaterthans x | Hashs x
  | Lessthans x
  | Minuss x | Obraces x | Oparenthesiss x | Obrackets x | Percents x | Pluss x
  | Questions x | Quotes x | Semicolons x | Slashs x | Spaces x | Stars x
  | Tabs x
  | Tildes x | Underscores x -> (2+x, 0)
  | Newline -> (0, 1)
  | Newlines x -> (0, 2+x)
  | Number s | Word s -> (String.length s, 0)

let length t =
  let c, nl = size_and_newlines t in
  c + nl

let split_first = function
  | Ampersands n -> Ampersand, (if n > 0 then Ampersands(n-1) else Ampersand)
  | Ats n -> At, (if n > 0 then Ats(n-1) else At)
  | Backquotes n -> Backquote, (if n > 0 then Backquotes(n-1) else Backquote)
  | Backslashs n -> Backslash, (if n > 0 then Backslashs(n-1) else Backslash)
  | Bars n -> Bar, (if n > 0 then Bars(n-1) else Bar)
  | Carets n -> Caret, (if n > 0 then Carets(n-1) else Caret)
  | Cbraces n -> Cbrace, (if n > 0 then Cbraces(n-1) else Cbrace)
  | Colons n -> Colon, (if n > 0 then Colons(n-1) else Colon)
  | Commas n -> Comma, (if n > 0 then Commas(n-1) else Comma)
  | Cparenthesiss n -> Cparenthesis, (if n > 0 then Cparenthesiss(n-1)
                                     else Cparenthesis)
  | Cbrackets n -> Cbracket, (if n > 0 then Cbrackets(n-1) else Cbracket)
  | Dollars n -> Dollar, (if n > 0 then Dollars(n-1) else Dollar)
  | Dots n -> Dot, (if n > 0 then Dots(n-1) else Dot)
  | Doublequotes n -> Doublequote, (if n > 0 then Doublequotes(n-1)
                                   else Doublequote)
  | Exclamations n -> Exclamation, (if n > 0 then Exclamations(n-1)
                                   else Exclamation)
  | Equals n -> Equal, (if n > 0 then Equals(n-1) else Equal)
  | Greaterthans n -> Greaterthan, (if n > 0 then Greaterthans(n-1)
                                   else Greaterthan)
  | Hashs n -> Hash, (if n > 0 then Hashs(n-1) else Hash)
  | Lessthans n -> Lessthan, (if n > 0 then Lessthans(n-1) else Lessthan)
  | Minuss n -> Minus, (if n > 0 then Minuss(n-1) else Minus)
  | Newlines n -> Newline, (if n > 0 then Newlines(n-1) else Newline)
  | Obraces n -> Obrace, (if n > 0 then Obraces(n-1) else Obrace)
  | Oparenthesiss n -> Oparenthesis, (if n > 0 then Oparenthesiss(n-1)
                                     else Oparenthesis)
  | Obrackets n -> Obracket, (if n > 0 then Obrackets(n-1) else Obracket)
  | Percents n -> Percent, (if n > 0 then Percents(n-1) else Percent)
  | Pluss n -> Plus, (if n > 0 then Pluss(n-1) else Plus)
  | Questions n -> Question, (if n > 0 then Questions(n-1) else Question)
  | Quotes n -> Quote, (if n > 0 then Quotes(n-1) else Quote)
  | Semicolons n -> Semicolon, (if n > 0 then Semicolons(n-1) else Semicolon)
  | Slashs n -> Slash, (if n > 0 then Slashs(n-1) else Slash)
  | Spaces n -> Space, (if n > 0 then Spaces(n-1) else Space)
  | Stars n -> Star, (if n > 0 then Stars(n-1) else Star)
  | Tabs n -> Tab, (if n > 0 then Tabs(n-1) else Tab)
  | Tildes n -> Tilde, (if n > 0 then Tildes(n-1) else Tilde)
  | Underscores n -> Underscore, (if n > 0 then Underscores(n-1)
                                 else Underscore)
  | Ampersand | At | Backquote | Backslash | Bar | Caret | Cbrace | Colon
  | Comma | Cparenthesis | Cbracket | Dollar | Dot | Doublequote
  | Exclamation | Equal | Greaterthan | Hash | Lessthan | Minus
  | Newline | Number _ | Obrace | Oparenthesis | Obracket | Percent
  | Plus | Question | Quote | Semicolon | Slash | Space | Star | Tab
  | Tilde | Underscore | Tag _ | Word _ ->
     invalid_arg "Omd_lexer.split_first"

module type Input =
sig
  type t
  val length : t -> int
  val get : t -> int -> char
  val sub : t -> pos:int -> len:int -> string
end

module Lex(I : Input) :
sig
  val lex : I.t -> t
end =
struct
  let lex (s : I.t) =
    let result = ref [] in
    let i = ref 0 in
    let l = I.length s in
    let rcount c =
      (* [rcount c] returns the number of immediate consecutive
         occurrences of [c].  By side-effect, it increases the reference
         counter [i]. *)
      let rec loop r =
        if !i = l then r
        else if I.get s !i = c then (incr i; loop (r+1))
        else r
      in
      loop 1
    in
    let word () =
      let start = !i in
      let rec loop () =
        begin
          if !i = l then
            Word(I.sub s ~pos:start ~len:(!i-start))
          else
            match I.get s !i with
            | ' ' | '\t' | '\n' | '\r' | '#' | '*' | '-' | '+' | '`' | '\''
            | '"' | '\\' | '_' | '[' | ']' | '{' | '}' | '(' | ')' | ':'
            | ';' | '>' | '~' | '<' | '@' | '&' | '|' | '^' | '.' | '/'
            | '$' | '%' | '!' | '?' | '=' ->
                Word(I.sub s ~pos:start ~len:(!i-start))
            | c -> incr i; loop()
        end
      in
      loop()
    in
    let maybe_number () =
      let start = !i in
      while
        !i < l &&
        match I.get s !i with
        | '0' .. '9' -> true
        | _ -> false
      do
        incr i
      done;
      if !i = l then
        Number(I.sub s ~pos:start ~len:(!i-start))
      else
        begin match I.get s !i with
          | ' ' | '\t' | '\n' | '\r' | '#' | '*' | '-' | '+' | '`' | '\'' | '"'
          | '\\' | '_' | '[' | ']' | '{' | '}' | '(' | ')' | ':' | ';' | '>'
          | '~' | '<' | '@' | '&' | '|' | '^' | '.' | '/' | '$' | '%' | '!'
          | '?' | '=' ->
              Number(I.sub s ~pos:start ~len:(!i-start))
          | _ ->
              i := start;
              word()
        end
    in

    let n_occ c = incr i; rcount c in

    while !i < l do
      let c = I.get s !i in
      let w = match c with
        | ' '  -> let n = n_occ c in if n = 1 then Space else Spaces (n-2)
        | '\t' -> let n = n_occ c in if n = 1 then Spaces(2) else Spaces(4*n-2)
        | '\n' -> let n = n_occ c in if n = 1 then Newline else Newlines (n-2)
        | '\r' -> (* eliminating \r by converting all styles to unix style *)
          incr i;
          let rec count_rn x =
            if !i < l && I.get s (!i) = '\n' then
              if !i + 1 < l && I.get s (!i+1) = '\r' then
                (i := !i + 2; count_rn (x+1))
              else
                x
            else
              x
          in
          let rn = 1 + count_rn 0 in
          if rn = 1 then
            match n_occ c with
            | 1 -> Newline
            | x -> assert(x>=2); Newlines(x-2)
          else
            (assert(rn>=2);Newlines(rn-2))
        | '#'  -> let n = n_occ c in if n = 1 then Hash else Hashs (n-2)
        | '*'  -> let n = n_occ c in if n = 1 then Star else Stars (n-2)
        | '-'  -> let n = n_occ c in if n = 1 then Minus else Minuss (n-2)
        | '+'  -> let n = n_occ c in if n = 1 then Plus else Pluss (n-2)
        | '`'  -> let n = n_occ c in if n = 1 then Backquote else Backquotes (n-2)
        | '\'' -> let n = n_occ c in if n = 1 then Quote else Quotes (n-2)
        | '"'  -> let n = n_occ c in if n = 1 then Doublequote
                                    else Doublequotes (n-2)
        | '\\' -> let n = n_occ c in if n = 1 then Backslash
                                    else Backslashs (n-2)
        | '_'  -> let n = n_occ c in if n = 1 then Underscore
                                    else Underscores (n-2)
        | '['  -> let n = n_occ c in if n = 1 then Obracket
                                    else Obrackets (n-2)
        | ']'  -> let n = n_occ c in if n = 1 then Cbracket else Cbrackets (n-2)
        | '{'  -> let n = n_occ c in if n = 1 then Obrace else Obraces (n-2)
        | '}'  -> let n = n_occ c in if n = 1 then Cbrace else Cbraces (n-2)
        | '('  -> let n = n_occ c in if n = 1 then Oparenthesis
                                    else Oparenthesiss (n-2)
        | ')'  -> let n = n_occ c in if n = 1 then Cparenthesis
                                    else Cparenthesiss (n-2)
        | ':'  -> let n = n_occ c in if n = 1 then Colon else Colons (n-2)
        | ';'  -> let n = n_occ c in if n = 1 then Semicolon else Semicolons (n-2)
        | '>'  -> let n = n_occ c in if n = 1 then Greaterthan
                                    else Greaterthans (n-2)
        | '~'  -> let n = n_occ c in if n = 1 then Tilde else Tildes (n-2)
        | '<'  -> let n = n_occ c in if n = 1 then Lessthan else Lessthans (n-2)
        | '@'  -> let n = n_occ c in if n = 1 then At else Ats (n-2)
        | '&'  -> let n = n_occ c in if n = 1 then Ampersand else Ampersands (n-2)
        | '|'  -> let n = n_occ c in if n = 1 then Bar else Bars (n-2)
        | '^'  -> let n = n_occ c in if n = 1 then Caret else Carets (n-2)
        | ','  -> let n = n_occ c in if n = 1 then Comma else Commas (n-2)
        | '.'  -> let n = n_occ c in if n = 1 then Dot else Dots (n-2)
        | '/'  -> let n = n_occ c in if n = 1 then Slash else Slashs (n-2)
        | '$'  -> let n = n_occ c in if n = 1 then Dollar else Dollars (n-2)
        | '%'  -> let n = n_occ c in if n = 1 then Percent else Percents (n-2)
        | '='  -> let n = n_occ c in if n = 1 then Equal else Equals (n-2)
        | '!'  -> let n = n_occ c in if n = 1 then Exclamation
                                    else Exclamations (n-2)
        | '?'  -> let n = n_occ c in if n = 1 then Question else Questions (n-2)
        | '0' .. '9' -> maybe_number()
        | c -> word() in
      result := w :: !result
    done;
    List.rev !result
end

module Lex_string = Lex(StringLabels)
let lex = Lex_string.lex

type bigstring = (char,
                  Bigarray.int8_unsigned_elt,
                  Bigarray.c_layout) Bigarray.Array1.t

module Bigarray_input : Input with type t = bigstring =
struct
  module BA = Bigarray

  type t = bigstring
  let get = BA.Array1.get
  let length = BA.Array1.dim
  let sub arr ~pos ~len =
    if len < 0 || pos < 0 || pos + len > BA.Array1.dim arr
    then invalid_arg "Bigarray_input.sub";
    let s = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.unsafe_set s i (BA.Array1.unsafe_get arr (i + pos))
    done;
    Bytes.unsafe_to_string s
end
module Lex_bigarray = Lex(Bigarray_input)
let lex_bigarray = Lex_bigarray.lex

let make_space = function
  | 0 -> invalid_arg "Omd_lexer.make_space"
  | 1 -> Space
  | n -> if n < 0 then invalid_arg "Omd_lexer.make_space" else Spaces (n-2)


(*
(** [string_of_tl l] returns the string representation of l.
    [estring_of_tl l] returns the escaped string representation of l
    (same semantics as [String.escaped (string_of_tl l)]). *)
let string_of_tl, estring_of_tl =
  let g escaped tl =
    let b = Buffer.create 42 in
    let rec loop : 'a t list -> unit = function
      | e::tl ->
          Buffer.add_string b (if escaped then String.escaped (string_of_t e)
                               else string_of_t e);
          loop tl
      | [] ->
          ()
    in
      Buffer.contents (loop tl; b)
  in g false, g true
*)

let string_of_tokens tl =
  let b = Buffer.create 128 in
  List.iter (fun e -> Buffer.add_string b (string_of_token e)) tl;
  Buffer.contents b


let destring_of_tokens ?(limit=max_int) tl =
  let b = Buffer.create 1024 in
  let rec loop (i:int) (tlist:tok list) : unit = match tlist with
    | e::tl ->
        if limit = i then
          loop i []
        else
          begin
            Buffer.add_string b (String.escaped (string_of_token e));
            Buffer.add_string b "::";
            loop (succ i) tl
          end
    | [] ->
        Buffer.add_string b "[]"
  in
    Buffer.contents (loop 0 tl; b)
