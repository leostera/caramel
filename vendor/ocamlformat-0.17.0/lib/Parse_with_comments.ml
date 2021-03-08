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

type 'a with_comments =
  {ast: 'a; comments: Cmt.t list; prefix: string; source: Source.t}

module W = struct
  type t = int

  let in_lexer = [1; 2; 3; 14; 29]

  let disable x = -abs x

  let enable x = abs x

  let to_string x =
    String.concat ~sep:"" (List.map ~f:(Format.sprintf "%+d") x)
end

exception Warning50 of (Location.t * Warnings.t) list

let tokens lexbuf =
  let rec loop acc =
    match Migrate_ast.Lexer.token_with_comments lexbuf with
    (* The location in lexbuf are invalid for comments *)
    | Token_latest.COMMENT (_, loc) as tok -> loop ((tok, loc) :: acc)
    | Token_latest.DOCSTRING ds as tok ->
        loop ((tok, Docstrings.docstring_loc ds) :: acc)
    | tok -> (
        let loc = Ppxlib.Location.of_lexbuf lexbuf in
        let acc = (tok, loc) :: acc in
        match tok with Token_latest.EOF -> List.rev acc | _ -> loop acc )
  in
  loop []

let fresh_lexbuf source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf !Location.input_name ;
  let hash_bang =
    Lexer.skip_hash_bang lexbuf ;
    let len = lexbuf.lex_last_pos in
    String.sub source ~pos:0 ~len
  in
  (lexbuf, hash_bang)

let parse fragment (conf : Conf.t) ~source =
  let warnings =
    W.enable 50
    :: (if conf.quiet then List.map ~f:W.disable W.in_lexer else [])
  in
  Warnings.parse_options false (W.to_string warnings) ;
  let w50 = ref [] in
  let t =
    let lexbuf, hash_bang = fresh_lexbuf source in
    with_warning_filter
      ~filter:(fun loc warn ->
        if is_unexpected_docstring warn && conf.comment_check then (
          w50 := (loc, warn) :: !w50 ;
          false )
        else not conf.quiet )
      ~f:(fun () ->
        let ast = Migrate_ast.Parse.fragment fragment lexbuf in
        Warnings.check_fatal () ;
        let comments =
          List.map
            ~f:(fun (txt, loc) -> Cmt.create txt loc)
            (Lexer.comments ())
        in
        let tokens =
          let lexbuf, _ = fresh_lexbuf source in
          tokens lexbuf
        in
        let source = Source.create ~text:source ~tokens in
        {ast; comments; prefix= hash_bang; source} )
  in
  match List.rev !w50 with [] -> t | w50 -> raise (Warning50 w50)
