open Sexplib.Std

type error = Lexer_offset_is_out_of_bounds | Unknown_character of string
[@@deriving sexp]

exception Lexer_error of error

type t = {
  filename : string;
  contents : string;
  (* The current character under the lexing cursor *)
  mutable curr_char : char;
  (* The offset in characters from the beginning of the file *)
  mutable offset : int;
  max_offset : int;
  (* The current line number *)
  mutable line_num : int;
  (* The current column number *)
  mutable col_num : int;
}
[@@deriving sexp]

module Position = struct
  type t = { filename : string; line_num : int; col_num : int; offset : int }
  [@@deriving sexp]

  let pp ppf t =
    let sexp = sexp_of_t t in
    Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp
end

type span = { start_pos : Position.t; end_pos : Position.t; token : Token.t }
[@@deriving sexp]

type spans = span list [@@deriving sexp]

(*** Ops on t ******************************************************************)

let make ~filename ~contents =
  {
    filename;
    contents;
    curr_char =
      (if String.length contents = 0 then Char.unsafe_chr (-1)
      else String.get contents 0);
    max_offset = String.length contents - 1;
    offset = 0;
    line_num = 1;
    col_num = 1;
  }

let pp ppf t =
  let sexp = sexp_of_t t in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let pp_error ppf error =
  let sexp = sexp_of_error error in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let pp_spans ppf spans =
  let sexp = sexp_of_spans spans in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

(*** Character matchers ********************************************************)

let is_newline c = match c with '\n' -> true | _ -> false

let is_whitespace c =
  match c with ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let is_num c = match c with '0' .. '9' -> true | _ -> false

let is_alpha c = match c with 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false

let is_alphanum c =
  match c with '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false

(*** Lexer actions *************************************************************)

let new_line t =
  t.line_num <- t.line_num + 1;
  t.col_num <- 1

let fill t = t.curr_char <- String.get t.contents t.offset

let next t =
  let offset' = t.offset + 1 in
  if offset' > t.max_offset then
    raise (Lexer_error Lexer_offset_is_out_of_bounds)
  else (
    t.offset <- offset';
    t.col_num <- t.col_num + 1;
    if is_newline t.curr_char then new_line t;
    fill t)

(*** Helpers *******************************************************************)

let position (t : t) =
  Position.
    {
      filename = t.filename;
      line_num = t.line_num;
      col_num = t.col_num;
      offset = t.offset;
    }

let eof t = { start_pos = position t; end_pos = position t; token = Token.EOF }

let get_string ~start_pos ~end_pos t =
  String.sub t.contents
    Position.(start_pos.offset)
    Position.(end_pos.offset - start_pos.offset)

(*** Combinators ***************************************************************)

let rec skip_while c t =
  if c t.curr_char then (
    next t;
    skip_while c t)

let skip_whitespace t = skip_while is_whitespace t

(*** Internals *****************************************************************)

(**
   Scan an identifier.
*)
let identifier t =
  let start_pos = position t in
  skip_while
    (fun c ->
      match c with
      | '_' | '.' | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' -> true
      | _ -> false)
    t;
  let end_pos = position t in
  let name = get_string ~start_pos ~end_pos t in
  match Token.find_keyword name with
  | Some token -> token
  | None -> Token.Id name

(**
   Scan a type variable.
*)
let type_var t =
  let start_pos = position t in
  skip_while
    (fun c ->
      match c with
      | '_' | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' -> true
      | _ -> false)
    t;
  let end_pos = position t in
  let name = get_string ~start_pos ~end_pos t in
  Token.Type_var name

(**
   Scan a string.
*)
let string t =
  (* drop the opening string quotation *)
  next t;

  (* capture the string *)
  let start_pos = position t in
  skip_while
    (fun c ->
      match c with
      | '\\' ->
          next t;
          true
      | '"' -> false
      | _ -> true)
    t;
  let end_pos = position t in
  let str = get_string ~start_pos ~end_pos t in

  (* drop the closing string quote *)
  next t;

  (* return the contents as a token *)
  Token.String str

(**
   Scan a coment.
*)
let comment t =
  (* drop the beginning of the comment *)
  next t;

  (* capture the comment *)
  let start_pos = position t in
  skip_while (fun c -> c <> '\n') t;
  let end_pos = position t in
  let str = get_string ~start_pos ~end_pos t in

  (* return the comment *)
  Token.Comment str

(**
   Scan a number: float or integer.
*)
let number t =
  let start_pos = position t in
  skip_while is_num t;
  let end_pos = position t in
  let str = get_string ~start_pos ~end_pos t in

  (* return the contents as a token *)
  Token.Integer str

(**
   Scan an atom.
*)
let atom t =
  let start_pos = position t in
  skip_while
    (fun c ->
      match c with
      | '_' | '.' | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' -> true
      | _ -> false)
    t;
  let end_pos = position t in
  let name = get_string ~start_pos ~end_pos t in
  Token.Atom name

(**
   Scan the next available token.

   NOTE: this function raises an exception for control flow.
*)
let scan t =
  skip_whitespace t;
  let start_pos = position t in

  let next token =
    next t;
    token
  in

  let token =
    match t.curr_char with
    | '0' .. '9' -> number t
    | '_' | 'A' .. 'Z' | 'a' .. 'z' -> identifier t
    | '"' -> string t
    | '@' -> next Token.At
    | ',' -> next Token.Comma
    | '[' -> next Token.Bracket_left
    | ']' -> next Token.Bracket_right
    | ')' -> next Token.Parens_right
    | '{' -> next Token.Brace_left
    | '}' -> next Token.Brace_right
    | '=' -> next Token.Equal
    | ';' -> next Token.Semicolon
    | '(' -> next Token.Parens_left
    | '*' -> next Token.Star
    | '^' -> next Token.Caret
    | '+' -> (
        next ();
        match t.curr_char with '+' -> next Token.Plus_plus | _ -> Token.Plus)
    | '/' -> (
        next ();
        match t.curr_char with '/' -> comment t | _ -> Token.Slash)
    | '|' -> (
        next ();
        match t.curr_char with
        | '|' -> next Token.Or
        | '>' -> next Token.Fun_pipe
        | _ -> Token.Pipe)
    | '!' -> (
        next ();
        match t.curr_char with '=' -> next Token.Not_equal | _ -> Token.Bang)
    | '<' -> (
        next ();
        match t.curr_char with
        | '=' -> next Token.Lesser_or_equal
        | _ -> Token.Lesser_than)
    | '>' -> (
        next ();
        match t.curr_char with
        | '=' -> next Token.Greater_or_equal
        | _ -> Token.Greater_than)
    | '&' -> (
        next ();
        match t.curr_char with '&' -> next Token.And | _ -> Token.Ampersand)
    | '-' -> (
        next ();
        match t.curr_char with '>' -> next Token.Arrow | _ -> Token.Dash)
    | '\'' -> (
        next ();
        match t.curr_char with 'a' .. 'z' -> type_var t | _ -> Token.Colon)
    | ':' -> (
        next ();
        match t.curr_char with
        | 'A' .. 'Z' | 'a' .. 'z' -> atom t
        | _ -> Token.Colon)
    | '.' -> (
        next ();
        match t.curr_char with
        | '.' -> (
            next ();
            match t.curr_char with
            | '.' -> next Token.Dot_dot_dot
            | _ -> Token.Dot_dot)
        | _ -> Token.Dot)
    | _ -> raise (Lexer_error (Unknown_character (String.make 1 t.curr_char)))
  in

  let end_pos = position t in
  { start_pos; end_pos; token }

(*** API ***********************************************************************)

let scan t =
  match scan t with
  | exception Lexer_error Lexer_offset_is_out_of_bounds -> Ok (eof t)
  | exception Lexer_error err -> Error (`Lexer_error err)
  | res -> Ok res

let peek t =
  (* save current state instaed of making an immutable copy to avoid
     copying the contents of the file on each peek
  *)
  let { curr_char; offset; line_num; col_num; _ } = t in

  let span = scan t in

  (* restore state of the lexer *)
  t.curr_char <- curr_char;
  t.offset <- offset;
  t.line_num <- line_num;
  t.col_num <- col_num;

  span

let tokenize t =
  let ( let* ) = Result.bind in
  let rec tokenize' t acc =
    let* span = scan t in
    if span.token = Token.EOF then Ok (List.rev acc)
    else tokenize' t (span.token :: acc)
  in
  tokenize' t []
