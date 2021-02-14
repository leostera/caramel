(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

type code_stylist = lang:string -> string -> string

open Printf
open Omd_representation
open Omd_utils

let default_language = ref ""



let text_of_md md =
  let b = Buffer.create 128 in
  let rec loop = function
    | X _ :: tl ->
        loop tl
    | Blockquote q :: tl ->
        loop q;
        loop tl
    | Ref(rc, name, text, fallback) :: tl ->
        Buffer.add_string b (htmlentities ~md:true name);
        loop tl
    | Img_ref(rc, name, alt, fallback) :: tl ->
        Buffer.add_string b (htmlentities ~md:true name);
        loop tl
    | Paragraph md :: tl ->
        loop md;
        Buffer.add_char b '\n';
        Buffer.add_char b '\n';
        loop tl
    | Img(alt, src, title) :: tl ->
        Buffer.add_string b (htmlentities ~md:true alt);
        loop tl
    | Text t :: tl ->
        Buffer.add_string b (htmlentities ~md:true t);
        loop tl
    | Raw t :: tl ->
        Buffer.add_string b t;
        loop tl
    | Raw_block t :: tl ->
        Buffer.add_char b '\n';
        Buffer.add_string b t;
        Buffer.add_char b '\n';
        loop tl
    | Emph md :: tl ->
        loop md;
        loop tl
    | Bold md :: tl ->
        loop md;
        loop tl
    | (Ul l | Ol l) :: tl ->
        List.iter (fun item -> loop item; Buffer.add_char b '\n') l;
        loop tl
    | (Ulp l | Olp l) :: tl ->
        List.iter loop l;
        loop tl
    | Code_block(lang, c) :: tl ->
        Buffer.add_string b (htmlentities ~md:false c);
        loop tl
    | Code(lang, c) :: tl ->
        Buffer.add_string b (htmlentities ~md:false c);
        loop tl
    | Br :: tl ->
        loop tl
    | Hr :: tl ->
        loop tl
    | Html(tagname, attrs, body) :: tl ->
        loop body;
        loop tl
    | Html_block(tagname, attrs, body) :: tl ->
        loop body;
        loop tl
    | Html_comment s :: tl ->
        loop tl
    | Url (href,s,title) :: tl ->
        loop s;
        loop tl
    | H1 md :: tl
    | H2 md :: tl
    | H3 md :: tl
    | H4 md :: tl
    | H5 md :: tl
    | H6 md :: tl ->
        loop md;
        loop tl
    | NL :: tl ->
        Buffer.add_string b "\n";
        loop tl
    | [] -> ()
  in
    loop md;
    Buffer.contents b

let default_code_stylist ~lang code = code

let filter_text_omd_rev l =
  let rec loop b r = function
    | [] -> if b then r else l
    | ("media:type", Some "text/omd")::tl ->
      loop true r tl
    | e::tl ->
      loop b (e::r) tl
  in
  loop false [] l

let remove_links : t -> t =
  Omd_representation.visit
    (fun e ->
     match e with
      | Url(_, t, _) -> Some t
      | _ -> None
    )

let rec html_and_headers_of_md
    ?(remove_header_links=false)
    ?(override=(fun (e:element) -> (None:string option)))
    ?(pindent=false)
    ?(nl2br=false)
    ?cs:(code_style=default_code_stylist)
    md
  =
  let ids = object(this)
    val mutable ids = StringSet.add "" StringSet.empty
    method mangle id =
      let rec m i =
        if StringSet.mem id ids then
          let idx = if i > 0 then id^"_"^string_of_int i else id in
          if StringSet.mem idx ids then
            m (i+1)
          else
            (ids <- StringSet.add idx ids;
             idx)
        else
          (ids <- StringSet.add id ids;
           id)
      in m 0
  end in
  let empty s =
    let rec loop i =
      if i < String.length s then
        match s.[i] with
        | ' ' | '\n' -> loop (i+1)
        | _ -> false
      else
        true
    in
    loop 0
  in
  let remove_trailing_blanks s =
    let rec loop i =
      if i < 0 then ""
      else
        match s.[i] with
        | ' '|'\t'|'\n' ->
          loop (pred i)
        | _ ->
          if i = String.length s - 1 then
            s
          else
            String.sub s 0 (i+1)
    in loop (String.length s - 1)
  in
  let b = Buffer.create 64 in
  let headers = ref [] in
  let rec loop indent = function
    | X x as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          (match x#to_t md with
           | Some t -> loop indent t
           | None ->
             match x#to_html ~indent:indent
               (html_of_md ~override ~pindent ~nl2br ~cs:code_style) md
             with
             | Some s -> Buffer.add_string b s
             | None -> ());
          loop indent tl
      end
    | Blockquote q as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          Buffer.add_string b "<blockquote>";
          loop indent q;
          Buffer.add_string b "</blockquote>";
          loop indent tl
      end
    | Ref(rc, name, text, fallback) as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          begin match rc#get_ref name with
            | Some(href, title) ->
              loop indent
                (Url(htmlentities ~md:true href,
                     [Text(text)],
                     htmlentities ~md:true title)
                 ::tl)
            | None ->
              loop indent (fallback#to_t);
              loop indent tl
          end
      end
    | Img_ref(rc, name, alt, fallback) as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          begin match rc#get_ref name with
            | Some(src, title) ->
              loop indent
                (Img(htmlentities ~md:true alt,
                     htmlentities ~md:true src,
                     htmlentities ~md:true title)::tl)
            | None ->
              loop indent (fallback#to_t);
              loop indent tl
          end
      end
    | Paragraph [] :: tl -> loop indent tl
    | Paragraph md as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          (let s = html_of_md ~override ~pindent ~nl2br ~cs:code_style md in
           if empty s then
             ()
           else
             begin
               Buffer.add_string b "<p>";
               Buffer.add_string b (remove_trailing_blanks s);
               Buffer.add_string b "</p>\n";
             end);
          loop indent tl
      end
    | Img(alt, src, title) as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          Buffer.add_string b "<img src='";
          Buffer.add_string b (htmlentities ~md:true src);
          Buffer.add_string b "' alt='";
          Buffer.add_string b (htmlentities ~md:true alt);
          Buffer.add_string b "' ";
          if title <> "" then
            (Buffer.add_string b " title='";
             Buffer.add_string b (htmlentities ~md:true title);
             Buffer.add_string b "' ");
          Buffer.add_string b "/>";
          loop indent tl
      end
    | Text t as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          (* Buffer.add_string b t; *)
          Buffer.add_string b (htmlentities ~md:true t);
          loop indent tl
      end
    | Emph md as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          Buffer.add_string b "<em>";
          loop indent md;
          Buffer.add_string b "</em>";
          loop indent tl
      end
    | Bold md as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          Buffer.add_string b "<strong>";
          loop indent md;
          Buffer.add_string b "</strong>";
          loop indent tl
      end
    | (Ul l|Ol l|Ulp l|Olp l as e) :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          Buffer.add_string b (match e with
              | Ol _|Olp _ -> "<ol>"
              | _ -> "<ul>");
          List.iter
            (
              fun li ->
                Buffer.add_string b "<li>";
                loop (indent+2) li;
                Buffer.add_string b "</li>"
            )
            l;
          Buffer.add_string b (match e with
              | Ol _|Olp _ -> "</ol>"
              | _ -> "</ul>");
          loop indent tl
      end
    | Code_block(lang, c) as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          if lang = "" && !default_language = "" then
            Buffer.add_string b "<pre><code>"
          else if lang = "" then
            bprintf b "<pre class='%s'><code class='%s'>"
              !default_language !default_language
          else
            bprintf b "<pre class='%s'><code class='%s'>" lang lang;
          let new_c = code_style ~lang:lang c in
          if c = new_c then
            Buffer.add_string b (htmlentities ~md:false c)
          else
            Buffer.add_string b new_c;
          Buffer.add_string b "</code></pre>";
          loop indent tl
      end
    | Code(lang, c) as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          if lang = "" && !default_language = "" then
            Buffer.add_string b "<code>"
          else if lang = "" then
            bprintf b "<code class='%s'>" !default_language
          else
            bprintf b "<code class='%s'>" lang;
          let new_c = code_style ~lang:lang c in
          if c = new_c then
            Buffer.add_string b (htmlentities ~md:false c)
          else
            Buffer.add_string b new_c;
          Buffer.add_string b "</code>";
          loop indent tl
      end
    | Br as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          Buffer.add_string b "<br/>";
          loop indent tl
      end
    | Hr as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          Buffer.add_string b "<hr/>";
          loop indent tl
      end
    | Raw s as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          Buffer.add_string b s;
          loop indent tl
      end
    | Raw_block s as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          Buffer.add_string b s;
          loop indent tl
      end
    | Html(tagname, attrs, []) as e :: tl
      when StringSet.mem tagname html_void_elements ->
      let attrs = filter_text_omd_rev attrs in
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          Printf.bprintf b "<%s" tagname;
          Buffer.add_string b (string_of_attrs attrs);
          Printf.bprintf b " />";
          loop indent tl
      end
    | Html(tagname, attrs, body) as e :: tl ->
      let attrs = filter_text_omd_rev attrs in
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          Printf.bprintf b "<%s" tagname;
          Buffer.add_string b (string_of_attrs attrs);
          Buffer.add_string b ">";
          loop indent body;
          Printf.bprintf b "</%s>" tagname;
          loop indent tl
      end
    | Html_block(tagname, attrs, body) as e :: tl ->
      let attrs = filter_text_omd_rev attrs in
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          if body = [] && StringSet.mem tagname html_void_elements then
            (
              Printf.bprintf b "<%s" tagname;
              Buffer.add_string b (string_of_attrs attrs);
              Buffer.add_string b " />";
              loop indent tl
            )
          else
            (
              Printf.bprintf b "<%s" tagname;
              Buffer.add_string b (string_of_attrs attrs);
              Buffer.add_string b ">";
              loop indent body;
              Printf.bprintf b "</%s>" tagname;
              loop indent tl
            )
      end
    | Html_comment s as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          Buffer.add_string b s;
          loop indent tl
      end
    | Url (href,s,title) as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          let s = html_of_md ~override ~pindent ~nl2br ~cs:code_style s in
          Buffer.add_string b "<a href='";
          Buffer.add_string b (htmlentities ~md:true href);
          Buffer.add_string b "'";
          if title <> "" then
            begin
              Buffer.add_string b " title='";
              Buffer.add_string b (htmlentities ~md:true title);
              Buffer.add_string b "'";
            end;
          Buffer.add_string b ">";
          Buffer.add_string b s;
          Buffer.add_string b "</a>";
          loop indent tl
      end
    | (H1 md as e) :: tl ->
      let e, md =
        if not remove_header_links then
          e, md
        else
          let md = remove_links md in
          H1 md, md in
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          let ih = html_of_md ~override ~pindent ~nl2br ~cs:code_style md in
          let id = id_of_string ids (text_of_md md) in
          headers := (e, id, ih) :: !headers;
          Buffer.add_string b "<h1 id=\"";
          Buffer.add_string b id;
          Buffer.add_string b "\">";
          Buffer.add_string b ih;
          Buffer.add_string b "</h1>";
          loop indent tl
      end
    | (H2 md as e) :: tl ->
      let e, md =
        if not remove_header_links then
          e, md
        else
          let md = remove_links md in
          H2 md, md in
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          let ih = html_of_md ~override ~pindent ~nl2br ~cs:code_style md in
          let id = id_of_string ids (text_of_md md) in
          headers := (e, id, ih) :: !headers;
          Buffer.add_string b "<h2 id=\"";
          Buffer.add_string b id;
          Buffer.add_string b "\">";
          Buffer.add_string b ih;
          Buffer.add_string b "</h2>";
          loop indent tl
      end
    | (H3 md as e) :: tl ->
      let e, md =
        if not remove_header_links then
          e, md
        else
          let md = remove_links md in
          H3 md, md in
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          let ih = html_of_md ~override ~pindent ~nl2br ~cs:code_style md in
          let id = id_of_string ids (text_of_md md) in
          headers := (e, id, ih) :: !headers;
          Buffer.add_string b "<h3 id=\"";
          Buffer.add_string b id;
          Buffer.add_string b "\">";
          Buffer.add_string b ih;
          Buffer.add_string b "</h3>";
          loop indent tl
      end
    | (H4 md as e) :: tl ->
      let e, md =
        if not remove_header_links then
          e, md
        else
          let md = remove_links md in
          H4 md, md in
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          let ih = html_of_md ~override ~pindent ~nl2br ~cs:code_style md in
          let id = id_of_string ids (text_of_md md) in
          headers := (e, id, ih) :: !headers;
          Buffer.add_string b "<h4 id=\"";
          Buffer.add_string b id;
          Buffer.add_string b "\">";
          Buffer.add_string b ih;
          Buffer.add_string b "</h4>";
          loop indent tl
      end
    | (H5 md as e) :: tl ->
      let e, md =
        if not remove_header_links then
          e, md
        else
          let md = remove_links md in
          H5 md, md in
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          let ih = html_of_md ~override ~pindent ~nl2br ~cs:code_style md in
          let id = id_of_string ids (text_of_md md) in
          headers := (e, id, ih) :: !headers;
          Buffer.add_string b "<h5 id=\"";
          Buffer.add_string b id;
          Buffer.add_string b "\">";
          Buffer.add_string b ih;
          Buffer.add_string b "</h5>";
          loop indent tl
      end
    | (H6 md as e) :: tl ->
      let e, md =
        if not remove_header_links then
          e, md
        else
          let md = remove_links md in
          H6 md, md in
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          let ih = html_of_md ~override ~pindent ~nl2br ~cs:code_style md in
          let id = id_of_string ids (text_of_md md) in
          headers := (e, id, ih) :: !headers;
          Buffer.add_string b "<h6 id=\"";
          Buffer.add_string b id;
          Buffer.add_string b "\">";
          Buffer.add_string b ih;
          Buffer.add_string b "</h6>";
          loop indent tl
      end
    | NL as e :: tl ->
      begin match override e with
        | Some s ->
          Buffer.add_string b s;
          loop indent tl
        | None ->
          if nl2br then
            Buffer.add_string b "<br />"
          else
            Buffer.add_string b "\n";
          loop indent tl
      end
    | [] ->
      ()
  in
  loop 0 md;
  Buffer.contents b, List.rev !headers

and string_of_attrs attrs =
  let b = Buffer.create 1024 in
  List.iter
    (function
      | (a, Some v) ->
        if not(String.contains v '\'') then
          Printf.bprintf b " %s='%s'" a v
        else if not(String.contains v '"') then
          Printf.bprintf b " %s=\"%s\"" a v
        else
          Printf.bprintf b " %s=\"%s\"" a v
      | a, None ->
        (* if html4 then *)
        (*   Printf.bprintf b " %s='%s'" a a *)
        (* else *)
        Printf.bprintf b " %s=''" a (* HTML5 *)
    )
    attrs;
  Buffer.contents b

and html_of_md
    ?(override=(fun (e:element) -> (None:string option)))
    ?(pindent=false)
    ?(nl2br=false)
    ?cs
    md
  =
  fst (html_and_headers_of_md ~override ~pindent ~nl2br ?cs md)
and headers_of_md ?remove_header_links md =
  snd (html_and_headers_of_md ?remove_header_links md)


let rec sexpr_of_md md =
  let b = Buffer.create 64 in
  let rec loop = function
    | X x :: tl ->
        (match x#to_t md with
           | Some t ->
             Buffer.add_string b "(X";
             loop t;
             Buffer.add_string b ")"
           | None ->
               match x#to_sexpr sexpr_of_md md with
                 | Some s ->
                   Buffer.add_string b "(X";
                   Buffer.add_string b s;
                   Buffer.add_string b ")"
                 | None ->
                     match x#to_html ~indent:0 html_of_md md with
                       | Some s ->
                         Buffer.add_string b "(X";
                         Buffer.add_string b s;
                         Buffer.add_string b ")"
                       | None -> ());
        loop tl
    | Blockquote q :: tl ->
        Buffer.add_string b "(Blockquote";
        loop q;
        Buffer.add_string b ")";
        loop tl
    | Ref(rc, name, text, _) :: tl ->
        bprintf b "(Ref %S %S)" name text;
        loop tl
    | Img_ref(rc, name, alt, _) :: tl ->
        bprintf b "(Img_ref %S %S)" name alt;
        loop tl
    | Paragraph md :: tl ->
        Buffer.add_string b "(Paragraph";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | Img(alt, src, title) :: tl ->
        bprintf b "(Img %S %S %S)" alt src title;
        loop tl
    | Text t :: tl ->
        bprintf b "(Text %S" t;
        let rec f = function
          | Text t :: tl ->
            bprintf b " %S" t;
            f tl
          | x -> x
        in
        let tl = f tl in
        bprintf b ")";
        loop tl
    | Emph md :: tl ->
        Buffer.add_string b "(Emph";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | Bold md :: tl ->
        Buffer.add_string b "(Bold";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | Ol l :: tl ->
        bprintf b "(Ol";
        List.iter(fun li -> bprintf b "(Li "; loop li; bprintf b ")") l;
        bprintf b ")";
        loop tl
    | Ul l :: tl ->
        bprintf b "(Ul";
        List.iter(fun li -> bprintf b "(Li "; loop li;bprintf b ")") l;
        bprintf b ")";
        loop tl
    | Olp l :: tl ->
        bprintf b "(Olp";
        List.iter(fun li -> bprintf b "(Li "; loop li; bprintf b ")") l;
        bprintf b ")";
        loop tl
    | Ulp l :: tl ->
        bprintf b "(Ulp";
        List.iter(fun li -> bprintf b "(Li "; loop li;bprintf b ")") l;
        bprintf b ")";
        loop tl
    | Code(lang, c) :: tl ->
        bprintf b "(Code %S)" c;
        loop tl
    | Code_block(lang, c) :: tl ->
        bprintf b "(Code_block %s)" c;
        loop tl
    | Br :: tl ->
        Buffer.add_string b "(Br)";
        loop tl
    | Hr :: tl ->
        Buffer.add_string b "(Hr)";
        loop tl
    | Raw s :: tl ->
        bprintf b "(Raw %S)" s;
        loop tl
    | Raw_block s :: tl ->
        bprintf b "(Raw_block %S)" s;
        loop tl
    | Html(tagname, attrs, body) :: tl ->
        bprintf b "(Html %s %s " tagname (string_of_attrs attrs);
        loop body;
        bprintf b ")";
        loop tl
    | Html_block(tagname, attrs, body) :: tl ->
        bprintf b "(Html_block %s %s " tagname (string_of_attrs attrs);
        loop body;
        bprintf b ")";
        loop tl
    | Html_comment s :: tl ->
        bprintf b "(Html_comment %S)" s;
        loop tl
    | Url (href,s,title) :: tl ->
        bprintf b "(Url %S %S %S)" href (html_of_md s) title;
        loop tl
    | H1 md :: tl ->
        Buffer.add_string b "(H1";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | H2 md :: tl ->
        Buffer.add_string b "(H2";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | H3 md :: tl ->
        Buffer.add_string b "(H3";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | H4 md :: tl ->
        Buffer.add_string b "(H4";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | H5 md :: tl ->
        Buffer.add_string b "(H5";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | H6 md :: tl ->
        Buffer.add_string b "(H6";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | NL :: tl ->
        Buffer.add_string b "(NL)";
        loop tl
    | [] -> ()
  in
    loop md;
    Buffer.contents b


let escape_markdown_characters s =
  let b = Buffer.create (String.length s * 2) in
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '.' as c ->
        if i > 0 &&
           match s.[i-1] with
           | '0' .. '9' -> i+1 < String.length s && s.[i+1] = ' '
           | _ -> false
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
      | '-' as c ->
        if (i = 0 || match s.[i-1] with ' '| '\n' -> true | _ -> false)
          && (i+1 < String.length s && (s.[i+1] = ' '||s.[i+1] = '-'))
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
      | '+' as c ->
        if (i = 0 || match s.[i-1] with ' '| '\n' -> true | _ -> false)
          && (i+1 < String.length s && s.[i+1] = ' ')
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
      | '!' as c ->
        if i+1 < String.length s && s.[i+1] = '[' then
          Buffer.add_char b '\\';
        Buffer.add_char b c
      | '<' as c ->
        if i <> String.length s - 1 &&
             (match s.[i+1] with 'a' .. 'z' | 'A' .. 'Z' -> false | _ -> true)
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
      | '>' as c ->
        if i = 0 ||
             (match s.[i-1] with ' ' | '\n' -> false | _ -> true)
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
      | '#' as c ->
         if i = 0 || s.[i-1] = '\n' then
           Buffer.add_char b '\\';
         Buffer.add_char b c
      | '\\' | '[' | ']' | '(' | ')' | '`' | '*' as c ->
        Buffer.add_char b '\\';
        Buffer.add_char b c
      | c ->
        Buffer.add_char b c
    done;
    Buffer.contents b

let rec markdown_of_md md =
  if debug then eprintf "(OMD) markdown_of_md(%S)\n%!" (sexpr_of_md md);
  let quote ?(indent=0) s =
    let b = Buffer.create (String.length s) in
    let l = String.length s in
    let rec loop nl i =
      if i < l then
        begin
          if nl && i < l - 1 then
            (for i = 1 to indent do
               Buffer.add_char b ' '
             done;
             Buffer.add_string b "> ");
          match s.[i] with
          | '\n' ->
            Buffer.add_char b '\n';
            loop true (succ i)
          | c ->
            Buffer.add_char b c;
            loop false (succ i)
        end
      else
        Buffer.contents b
    in loop true 0
  in
  let b = Buffer.create 64 in
  let add_spaces n = for i = 1 to n do Buffer.add_char b ' ' done in
  let references = ref None in
  let rec loop ?(fst_p_in_li=true) ?(is_in_list=false) list_indent l =
    (* [list_indent: int] is the indentation level in number of spaces. *)
    (* [is_in_list: bool] is necessary to know if we are inside a paragraph
       which is inside a list item because those need to be indented! *)
    let loop ?(fst_p_in_li=fst_p_in_li) ?(is_in_list=is_in_list) list_indent l =
        loop ~fst_p_in_li:fst_p_in_li ~is_in_list:is_in_list list_indent l
    in
    match l with
    | X x :: tl ->
        (match x#to_t md with
           | Some t -> loop list_indent t
           | None ->
             match x#to_html ~indent:0 html_of_md md with
             | Some s -> Buffer.add_string b s
             | None -> ());
        loop list_indent tl
    | Blockquote q :: tl ->
      Buffer.add_string b (quote ~indent:list_indent (markdown_of_md q));
      if tl <> [] then Buffer.add_string b "\n";
      loop list_indent tl
    | Ref(rc, name, text, fallback) :: tl ->
        if !references = None then references := Some rc;
        loop list_indent (Raw(fallback#to_string)::tl)
    | Img_ref(rc, name, alt, fallback) :: tl ->
        if !references = None then references := Some rc;
        loop list_indent (Raw(fallback#to_string)::tl)
    | Paragraph [] :: tl -> loop list_indent tl
    | Paragraph md :: tl ->
      if is_in_list then
        if fst_p_in_li then
          add_spaces (list_indent-2)
        else
          add_spaces list_indent;
      loop ~fst_p_in_li:false list_indent md;
      Printf.bprintf b "\n\n";
      loop ~fst_p_in_li:false list_indent tl
    | Img(alt, src, title) :: tl ->
      Printf.bprintf b "![%s](%s \"%s\")" alt src title;
      loop list_indent tl
    | Text t :: tl ->
      Printf.bprintf b "%s" (escape_markdown_characters t);
      loop list_indent tl
    | Emph md :: tl ->
      Buffer.add_string b "*";
      loop list_indent md;
      Buffer.add_string b "*";
      loop list_indent tl
    | Bold md :: tl ->
      Buffer.add_string b "**";
      loop list_indent md;
      Buffer.add_string b "**";
      loop list_indent tl
    | Ol l :: tl ->
      if Buffer.length b > 0 && Buffer.nth b (Buffer.length b - 1) <> '\n' then
           Buffer.add_char b '\n';
      let c = ref 0 in (* don't use List.iteri because it's not in 3.12 *)
      List.iter(fun li ->
                    incr c;
                    add_spaces list_indent;
                    Printf.bprintf b "%d. " !c;
                    loop ~is_in_list:true (list_indent+4) li;
                    Buffer.add_char b '\n';
               ) l;
      if list_indent = 0 then Buffer.add_char b '\n';
      loop list_indent tl
    | Ul l :: tl ->
      if Buffer.length b > 0 && Buffer.nth b (Buffer.length b - 1) <> '\n' then
           Buffer.add_char b '\n';
      List.iter(fun li ->
                    add_spaces list_indent;
                    Printf.bprintf b "- ";
                    loop ~is_in_list:true (list_indent+4) li;
                    Buffer.add_char b '\n';
               ) l;
      if list_indent = 0 then Buffer.add_char b '\n';
      loop list_indent tl
    | Olp l :: tl ->
      let c = ref 0 in (* don't use List.iteri because it's not in 3.12 *)
      List.iter(fun li ->
        if Buffer.length b > 0 && Buffer.nth b (Buffer.length b - 1) <> '\n'
        then Buffer.add_char b '\n';
        add_spaces list_indent;
        incr c;
        bprintf b "%d. " !c;
        loop ~is_in_list:true (list_indent+4) li;
               (* Paragraphs => No need of '\n' *)
      ) l;
      loop list_indent tl
    | Ulp l :: tl ->
      List.iter(fun li ->
        if Buffer.length b > 0 && Buffer.nth b (Buffer.length b - 1) <> '\n'
        then Buffer.add_char b '\n';
        add_spaces list_indent;
        bprintf b "+ ";
        loop ~is_in_list:true (list_indent+4) li;
               (* Paragraphs => No need of '\n' *)
               ) l;
      begin match tl with
      | (H1 _ | H2 _ | H3 _ | H4 _ | H5 _ | H6 _)::_
      | NL::(H1 _ | H2 _ | H3 _ | H4 _ | H5 _ | H6 _)::_ ->
        Buffer.add_char b '\n'
      | _ -> ()
      end;
      loop list_indent tl
    | Code(_lang, c) :: tl -> (* FIXME *)
      let n = (* compute how many backquotes we need to use *)
        let filter (n:int) (s:int list) =
          if n > 0 && n < 10 then
            List.filter (fun e -> e <> n) s
          else
            s
        in
        let l = String.length c in
        let rec loop s x b i =
          if i = l then
            match filter b s with
            | hd::_ -> hd
            | [] -> x+1
          else
            match c.[i] with
            | '`' -> loop s x (succ b) (succ i)
            | _ -> loop (filter b s) (max b x) 0 (succ i)
        in
          loop [1;2;3;4;5;6;7;8;9;10] 0 0 0
      in
        begin
          Printf.bprintf b "%s" (String.make n '`');
          if c.[0] = '`' then Buffer.add_char b ' ';
          Printf.bprintf b "%s" c;
          if c.[String.length c - 1] = '`' then Buffer.add_char b ' ';
          Printf.bprintf b "%s" (String.make n '`');
        end;
        loop list_indent tl
    | Code_block(lang, c) :: tl ->
      let n = (* compute how many backquotes we need to use *)
        let filter n s =
          if n > 0 && n < 10 then
            List.filter (fun e -> e <> n) s
          else
            s
        in
        let l = String.length c in
        let rec loop s b i =
          if i = l then
            match filter b s with
              | hd::_ -> hd
              | [] -> 0
          else
            match c.[i] with
            | '`' -> loop s (succ b) (succ i)
            | _ -> loop (filter b s) 0 (succ i)
        in
          loop [3;4;5;6;7;8;9;10] 0 0
      in
      let output_indented_block n s =
        let rec loop p i =
          if i = String.length s then
            ()
          else
            match p with
            | '\n' ->
                Printf.bprintf b "%s" (String.make n ' ');
                Buffer.add_char b s.[i];
                loop s.[i] (succ i)
            | _ ->
                Buffer.add_char b s.[i];
                loop s.[i] (succ i)
        in loop '\n' 0
      in
        if n = 0 then  (* FIXME *)
          begin
            (* case where we can't use backquotes *)
            Buffer.add_char b '\n';
            output_indented_block (4+list_indent) c;
            if tl <> [] then Buffer.add_string b "\n\n"
          end
        else
          begin
            Buffer.add_string b (String.make (list_indent) ' ');
            Printf.bprintf b "%s%s\n" (String.make n '`')
              (if lang = "" then !default_language else lang);
            output_indented_block (list_indent) c;
            if Buffer.nth b (Buffer.length b - 1) <> '\n' then
              Buffer.add_char b '\n';
            Buffer.add_string b (String.make (list_indent) ' ');
            Printf.bprintf b "%s\n" (String.make n '`');
          end;
        loop list_indent tl
    | Br :: tl ->
      Buffer.add_string b "<br />";
      loop list_indent tl
    | Hr :: tl ->
      Buffer.add_string b "* * *\n";
      loop list_indent tl
    | Raw s :: tl ->
      Buffer.add_string b s;
      loop list_indent tl
    | Raw_block s :: tl ->
      Buffer.add_char b '\n';
      Buffer.add_string b s;
      Buffer.add_char b '\n';
      loop list_indent tl
    | Html(tagname, attrs, []) :: tl
      when StringSet.mem tagname html_void_elements ->
      Printf.bprintf b "<%s" tagname;
      Buffer.add_string b (string_of_attrs attrs);
      Buffer.add_string b " />";
      loop list_indent tl
    | Html(tagname, attrs, body) :: tl ->
      let a = filter_text_omd_rev attrs in
      Printf.bprintf b "<%s" tagname;
      Buffer.add_string b (string_of_attrs a);
      Buffer.add_string b ">";
      if a == attrs then
        loop list_indent body
      else
        Buffer.add_string b (html_of_md body);
      Printf.bprintf b "</%s>" tagname;
      loop list_indent tl
    | (Html_block(tagname, attrs, body))::tl ->
      let needs_newlines =
        match tl with
        | NL :: Paragraph p :: _
        | Paragraph p :: _ -> p <> []
        | (H1 _ | H2 _ | H3 _ | H4 _ | H5 _ | H6 _
          | Ul _ | Ol _ | Ulp _ | Olp _ | Code (_, _) | Code_block (_, _)
          | Text _ | Emph _ | Bold _ | Br |Hr | Url (_, _, _)
          | Ref (_, _, _, _) | Img_ref (_, _, _, _)
          | Html (_, _, _)
          | Blockquote _ | Img (_, _, _)) :: _ -> true
        | ( Html_block (_, _, _) | Html_comment _
          | Raw _|Raw_block _) :: _-> false
        | X _ :: _ -> false
        | NL :: _ -> false
        | [] -> false
      in
      if body = [] && StringSet.mem tagname html_void_elements then
        (
          Printf.bprintf b "<%s" tagname;
          Buffer.add_string b (string_of_attrs attrs);
          Buffer.add_string b " />";
          if needs_newlines then Buffer.add_string b "\n\n";
          loop list_indent tl
        )
      else
        (
          let a = filter_text_omd_rev attrs in
          Printf.bprintf b "<%s" tagname;
          Buffer.add_string b (string_of_attrs a);
          Buffer.add_string b ">";
          if a == attrs then
            loop list_indent body
          else
            Buffer.add_string b (html_of_md body);
          Printf.bprintf b "</%s>" tagname;
          if needs_newlines then Buffer.add_string b "\n\n";
          loop list_indent tl
        )
    | Html_comment s :: tl ->
      Buffer.add_string b s;
      loop list_indent tl
    | Url (href,s,title) :: tl ->
      if title = "" then
        bprintf b "[%s](%s)" (markdown_of_md s) href
      else
        bprintf b "[%s](%s \"%s\")" (markdown_of_md s) href title;
      loop list_indent tl
    | H1 md :: tl ->
      Buffer.add_string b "# ";
      loop list_indent md;
      Buffer.add_string b "\n";
      loop list_indent tl
    | H2 md :: tl ->
      Buffer.add_string b "## ";
      loop list_indent md;
      Buffer.add_string b "\n";
      loop list_indent tl
    | H3 md :: tl ->
      Buffer.add_string b "### ";
      loop list_indent md;
      Buffer.add_string b "\n";
      loop list_indent tl
    | H4 md :: tl ->
      Buffer.add_string b "#### ";
      loop list_indent md;
      Buffer.add_string b "\n";
      loop list_indent tl
    | H5 md :: tl ->
      Buffer.add_string b "##### ";
      loop list_indent md;
      Buffer.add_string b "\n";
      loop list_indent tl
    | H6 md :: tl ->
      Buffer.add_string b "###### ";
      loop list_indent md;
      Buffer.add_string b "\n";
      loop list_indent tl
    | NL :: tl ->
      if Buffer.length b = 1
      || (Buffer.length b > 1 &&
          not(Buffer.nth b (Buffer.length b - 1) = '\n'
              && Buffer.nth b (Buffer.length b - 2) = '\n'))
      then
      Buffer.add_string b "\n";
      loop list_indent tl
    | [] -> ()
  in
    loop 0 md;
    begin match !references with
      | None -> ()
      | Some r ->
          Buffer.add_char b '\n';
          List.iter
            (fun (name, (url, title)) ->
               if title = "" then
                 bprintf b "[%s]: %s \n" name url
               else
                 bprintf b "[%s]: %s \"%s\"\n" name url title
            )
            r#get_all
    end;
    let res = Buffer.contents b in
    if debug then
      eprintf "(OMD) markdown_of_md(%S) => %S\n%!"
        (sexpr_of_md md) res;
    res
