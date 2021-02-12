(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

include Omd_representation
include Omd_backend

let of_input lex ?extensions:e ?default_lang:d s =
  let module E = Omd_parser.Default_env(struct end) in
  let module Parser = Omd_parser.Make(
    struct
      include E
      let extensions = match e with Some x -> x | None -> E.extensions
      let default_lang = match d with Some x -> x | None -> E.default_lang
    end
  ) in
  let md =
    Parser.parse (lex s)
  in
  Parser.make_paragraphs md

let of_string = of_input Omd_lexer.lex
let of_bigarray = of_input Omd_lexer.lex_bigarray

let to_html :
  ?override:(Omd_representation.element -> string option) ->
  ?pindent:bool ->
  ?nl2br:bool ->
  ?cs:code_stylist ->
  t ->
  string
  =
  html_of_md

let to_text : t -> string = text_of_md

let to_markdown : t -> string = markdown_of_md

let html_of_string (html:string) : string =
  html_of_md (Omd_parser.default_parse (Omd_lexer.lex html))


let rec set_default_lang lang = function
  | Code("", code) :: tl -> Code(lang, code) :: set_default_lang lang tl
  | Code_block("", code) :: tl -> Code_block(lang, code)
                                 :: set_default_lang lang tl
  (* Recurse on all elements even though code (blocks) are not allowed
     everywhere. *)
  | H1 t :: tl -> H1(set_default_lang lang t) :: set_default_lang lang tl
  | H2 t :: tl -> H2(set_default_lang lang t) :: set_default_lang lang tl
  | H3 t :: tl -> H3(set_default_lang lang t) :: set_default_lang lang tl
  | H4 t :: tl -> H4(set_default_lang lang t) :: set_default_lang lang tl
  | H5 t :: tl -> H5(set_default_lang lang t) :: set_default_lang lang tl
  | H6 t :: tl -> H6(set_default_lang lang t) :: set_default_lang lang tl
  | Paragraph t :: tl -> Paragraph(set_default_lang lang t)
                        :: set_default_lang lang tl
  | Emph t :: tl -> Emph(set_default_lang lang t) :: set_default_lang lang tl
  | Bold t :: tl -> Bold(set_default_lang lang t) :: set_default_lang lang tl
  | Ul t :: tl -> Ul(List.map (set_default_lang lang) t)
                 :: set_default_lang lang tl
  | Ol t :: tl -> Ol(List.map (set_default_lang lang) t)
                 :: set_default_lang lang tl
  | Ulp t :: tl -> Ulp(List.map (set_default_lang lang) t)
                  :: set_default_lang lang tl
  | Olp t :: tl -> Olp(List.map (set_default_lang lang) t)
                  :: set_default_lang lang tl
  | Url(href, t, title) :: tl -> Url(href, set_default_lang lang t, title)
                                :: set_default_lang lang tl
  | Blockquote t :: tl -> Blockquote(set_default_lang lang t)
                         :: set_default_lang lang tl
  (* Elements that do not contain Markdown. *)
  | (Text _|Code _|Code_block _|Br|Hr|NL|Ref _|Img_ref _|Raw _|Raw_block _
    |Html _|Html_block _|Html_comment _|Img _|X _) as e :: tl ->
     e :: set_default_lang lang tl
  | [] -> []


(* Table of contents
 ***********************************************************************)

(* Given a list of headers — in the order of the document — go to the
   requested subsection.  We first seek for the [number]th header at
   [level].  *)
let rec find_start headers level number subsections =
  match headers with
  | [] -> []
  | (H1 _, _, _) :: tl -> deal_with_header 1 headers tl level number subsections
  | (H2 _, _, _) :: tl -> deal_with_header 2 headers tl level number subsections
  | (H3 _, _, _) :: tl -> deal_with_header 3 headers tl level number subsections
  | (H4 _, _, _) :: tl -> deal_with_header 4 headers tl level number subsections
  | (H5 _, _, _) :: tl -> deal_with_header 5 headers tl level number subsections
  | (H6 _, _, _) :: tl -> deal_with_header 6 headers tl level number subsections
  | _ :: _ -> assert false

and deal_with_header h_level headers tl level number subsections =
  if h_level > level then (* Skip, right [level]-header not yet reached. *)
    if number = 0 then
      (* Assume empty section at [level], do not consume token. *)
      (match subsections with
       | [] -> headers (* no subsection to find *)
       | n :: subsections -> find_start headers (level + 1) n subsections)
    else find_start tl level number subsections
  else if h_level = level then (
    (* At proper [level].  Have we reached the [number] one? *)
    if number <= 1 then (
      match subsections with
      | [] -> tl (* no subsection to find *)
      | n :: subsections -> find_start tl (level + 1) n subsections
    )
    else find_start tl level (number - 1) subsections
  )
  else (* h_level < level *)
    [] (* Sought [level] has not been found in the current section *)

(* Assume we are at the start of the headers we are interested in.
   Return the list of TOC entries for [min_level] and the [headers]
   not used for the TOC entries. *)
let rec make_toc (headers:(element*string*string)list) ~min_level ~max_level =
  if min_level > max_level then [], headers
  else (
    match headers with
    | [] -> [], []
    | (H1 t, id, _) :: tl -> toc_entry headers 1 t id tl ~min_level ~max_level
    | (H2 t, id, _) :: tl -> toc_entry headers 2 t id tl ~min_level ~max_level
    | (H3 t, id, _) :: tl -> toc_entry headers 3 t id tl ~min_level ~max_level
    | (H4 t, id, _) :: tl -> toc_entry headers 4 t id tl ~min_level ~max_level
    | (H5 t, id, _) :: tl -> toc_entry headers 5 t id tl ~min_level ~max_level
    | (H6 t, id, _) :: tl -> toc_entry headers 6 t id tl ~min_level ~max_level
    | _ :: _ -> assert false
  )
and toc_entry headers h_level t id tl ~min_level ~max_level =
  if h_level > max_level then (* too deep, skip *)
    make_toc tl ~min_level ~max_level
  else if h_level < min_level then
    (* section we wanted the TOC for is finished, do not comsume the token *)
    [], headers
  else if h_level = min_level then (
    let sub_toc, tl = make_toc tl ~min_level:(min_level + 1) ~max_level in
    let toc_entry = match sub_toc with
      | [] -> [Url("#" ^ id, t, ""); NL]
      | _ -> [Url("#" ^ id, t, ""); NL; Ul sub_toc; NL] in
    let toc, tl = make_toc tl ~min_level ~max_level in
    toc_entry :: toc, tl
  ) else (* h_level > min_level *)
    let sub_toc, tl = make_toc headers ~min_level:(min_level + 1) ~max_level in
    let toc, tl = make_toc tl ~min_level ~max_level in
    [Ul sub_toc] :: toc, tl

let toc ?(start=[]) ?(depth=2) md =
  if depth < 1 then invalid_arg "Omd.toc: ~depth must be >= 1";
  let headers = Omd_backend.headers_of_md ~remove_header_links:true md in
  let headers = match start with
    | [] -> headers
    | number :: subsections ->
       if number < 0 then invalid_arg("Omd.toc: level 1 start must be >= 0");
       find_start headers 1 number subsections in
  let len = List.length start in
  let toc, _ = make_toc headers
                        ~min_level:(len + 1) ~max_level:(len + depth) in
  match toc with
  | [] -> []
  | _ -> [Ul(toc)]

let add_toc ?start ?depth ?title md =
  let toc = toc ?start ?depth md in
  (* Replace "*Table of contents*" with the actual TOC. *)
  toc
