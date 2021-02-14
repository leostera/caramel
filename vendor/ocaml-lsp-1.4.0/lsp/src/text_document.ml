open Types

module Encoding = struct
  let recode ?nln ?encoding out_encoding
      (src : [ `Channel of in_channel | `String of string ])
      (dst : [ `Channel of out_channel | `Buffer of Buffer.t ]) =
    let rec loop d e =
      match Uutf.decode d with
      | `Uchar _ as u ->
        ignore (Uutf.encode e u);
        loop d e
      | `End -> ignore (Uutf.encode e `End)
      | `Malformed _ ->
        ignore (Uutf.encode e (`Uchar Uutf.u_rep));
        loop d e
      | `Await -> assert false
    in
    let d = Uutf.decoder ?nln ?encoding src in
    let e = Uutf.encoder out_encoding dst in
    loop d e

  let nln = `ASCII (Uchar.of_char '\n')

  let reencode_string in_enc out_enc str =
    let buf = Buffer.create (String.length str) in
    let () = recode ~nln ~encoding:in_enc out_enc (`String str) (`Buffer buf) in
    Buffer.contents buf

  let utf8_to_utf16 = reencode_string `UTF_8 `UTF_16LE

  let utf16_to_utf8 = reencode_string `UTF_16LE `UTF_8

  let utf16_line_offsets (text : string) =
    let rec loop d acc =
      let old_line = Uutf.decoder_line d in
      match Uutf.decode d with
      | `Uchar _
      | `Malformed _ ->
        let new_line = Uutf.decoder_line d in
        if new_line > old_line then
          let line_ofs = Uutf.decoder_byte_count d / 2 in
          (* UTF16 encodes on 2 bytes *)
          loop d (line_ofs :: acc)
        else
          loop d acc
      | `End ->
        let end_ofs = Uutf.decoder_byte_count d / 2 in
        Array.of_list (List.rev (end_ofs :: acc))
      | `Await -> assert false
    in
    let encoding = `UTF_16LE in
    let decoder = Uutf.decoder ~nln ~encoding (`String text) in
    loop decoder [ 0 ]
end

(* Text is received as UTF-8. However, the protocol specifies offsets should be
   computed based on UTF-16. Therefore we reencode every file into utf16 for
   analysis. *)

type t = TextDocumentItem.t

let utf16_offsetAt (text : string) ({ line; character } : Position.t) =
  if line < 0 then
    0
  else
    let lofs = Encoding.utf16_line_offsets text in
    let al = Array.length lofs in
    if line >= al - 1 then
      lofs.(al - 1)
    else
      let this_lofs = lofs.(line) in
      let next_line_offset = lofs.(line + 1) in
      max (min (this_lofs + character) next_line_offset) this_lofs

let byte_offsetAt t pos = 2 * utf16_offsetAt t pos

let utf16_range_change (text_utf8 : string) ({ start; end_ } : Range.t)
    change_utf16 =
  let text = Encoding.utf8_to_utf16 text_utf8 in
  let doc_length = String.length text in
  let start_ofs = byte_offsetAt text start in
  let end_ofs = byte_offsetAt text end_ in
  let buf = Buffer.create (String.length change_utf16 + doc_length) in
  Buffer.add_substring buf text 0 start_ofs;
  Buffer.add_string buf change_utf16;
  Buffer.add_substring buf text end_ofs (doc_length - end_ofs);
  Buffer.contents buf

let make (t : DidOpenTextDocumentParams.t) = t.textDocument

let documentUri (t : t) = Uri0.t_of_yojson (`String t.uri)

let version (t : t) = t.version

let languageId (t : t) = t.languageId

let text (t : t) = t.text

let apply_content_change ?version (t : t)
    (change : TextDocumentContentChangeEvent.t) =
  (* Changes can only be applied using utf16 offsets *)
  let version =
    match version with
    | None -> t.version + 1
    | Some version -> version
  in
  let change_text = Encoding.utf8_to_utf16 change.text in
  let utf16_text =
    match change.range with
    | None -> change_text
    | Some range -> utf16_range_change t.text range change_text
  in
  let utf8_text = Encoding.utf16_to_utf8 utf16_text in
  { t with version; text = utf8_text }
