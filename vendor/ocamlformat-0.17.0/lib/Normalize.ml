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

(** Normalize abstract syntax trees *)

open Migrate_ast
open Asttypes
open Parsetree
open Ast_helper

type conf =
  { conf: Conf.t
  ; normalize_code:
      Migrate_ast.Parsetree.structure -> Migrate_ast.Parsetree.structure }

(** Remove comments that duplicate docstrings (or other comments). *)
let dedup_cmts fragment ast comments =
  let of_ast ast =
    let iter =
      object
        inherit [Set.M(Cmt).t] Ppxlib.Ast_traverse.fold as super

        method! attribute atr docs =
          match atr with
          | { attr_name= {txt= "ocaml.doc" | "ocaml.text"; _}
            ; attr_payload=
                PStr
                  [ { pstr_desc=
                        Pstr_eval
                          ( { pexp_desc=
                                Pexp_constant (Pconst_string (doc, _, None))
                            ; pexp_loc
                            ; _ }
                          , [] )
                    ; _ } ]
            ; _ } ->
              Set.add docs (Cmt.create ("*" ^ doc) pexp_loc)
          | _ -> super#attribute atr docs
      end
    in
    Traverse.fold fragment iter ast (Set.empty (module Cmt))
  in
  Set.(to_list (diff (of_list (module Cmt) comments) (of_ast ast)))

let comment s =
  (* normalize consecutive whitespace chars to a single space *)
  String.concat ~sep:" "
    (List.filter ~f:(Fn.non String.is_empty)
       (String.split_on_chars s ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' ']) )

let list f fmt l =
  let pp_sep fmt () = Format.fprintf fmt "" in
  Format.pp_print_list ~pp_sep f fmt l

let str fmt s = Format.fprintf fmt "%s" (comment s)

let ign_loc f fmt with_loc = f fmt with_loc.Odoc_model.Location_.value

let fpf = Format.fprintf

open Odoc_parser.Ast

let odoc_reference = ign_loc str

let odoc_style fmt = function
  | `Bold -> fpf fmt "Bold"
  | `Italic -> fpf fmt "Italic"
  | `Emphasis -> fpf fmt "Emphasis"
  | `Superscript -> fpf fmt "Superscript"
  | `Subscript -> fpf fmt "Subscript"

let rec odoc_inline_element fmt = function
  | `Space _ -> ()
  | `Word txt ->
      (* Ignore backspace changes *)
      let txt =
        String.filter txt ~f:(function '\\' -> false | _ -> true)
      in
      fpf fmt "Word,%a" str txt
  | `Code_span txt -> fpf fmt "Code_span,%a" str txt
  | `Raw_markup (Some lang, txt) -> fpf fmt "Raw_html:%s,%a" lang str txt
  | `Raw_markup (None, txt) -> fpf fmt "Raw_html,%a" str txt
  | `Styled (style, elems) ->
      fpf fmt "Styled,%a,%a" odoc_style style odoc_inline_elements elems
  | `Reference (_kind, ref, content) ->
      fpf fmt "Reference,%a,%a" odoc_reference ref odoc_inline_elements
        content
  | `Link (txt, content) ->
      fpf fmt "Link,%a,%a" str txt odoc_inline_elements content

and odoc_inline_elements fmt elems =
  list (ign_loc odoc_inline_element) fmt elems

let rec odoc_nestable_block_element c fmt = function
  | `Paragraph elms -> fpf fmt "Paragraph,%a" odoc_inline_elements elms
  | `Code_block txt ->
      let txt =
        try
          let ({ast; comments; _} : _ Parse_with_comments.with_comments) =
            Parse_with_comments.parse Structure c.conf ~source:txt
          in
          let comments = dedup_cmts Traverse.Structure ast comments in
          let print_comments fmt (l : Cmt.t list) =
            List.sort l ~compare:(fun {Cmt.loc= a; _} {Cmt.loc= b; _} ->
                Location.compare a b )
            |> List.iter ~f:(fun {Cmt.txt; _} ->
                   Caml.Format.fprintf fmt "%s," txt )
          in
          let ast = c.normalize_code ast in
          Caml.Format.asprintf "AST,%a,COMMENTS,[%a]" Printast.implementation
            ast print_comments comments
        with _ -> txt
      in
      fpf fmt "Code_block,%a" str txt
  | `Verbatim txt -> fpf fmt "Verbatim,%a" str txt
  | `Modules mods -> fpf fmt "Modules,%a" (list odoc_reference) mods
  | `List (ord, _syntax, items) ->
      let ord = match ord with `Unordered -> "U" | `Ordered -> "O" in
      let list_item fmt elems =
        fpf fmt "Item(%a)" (odoc_nestable_block_elements c) elems
      in
      fpf fmt "List,%s,%a" ord (list list_item) items

and odoc_nestable_block_elements c fmt elems =
  list (ign_loc (odoc_nestable_block_element c)) fmt elems

let odoc_tag c fmt = function
  | `Author txt -> fpf fmt "Author,%a" str txt
  | `Deprecated elems ->
      fpf fmt "Deprecated,%a" (odoc_nestable_block_elements c) elems
  | `Param (p, elems) ->
      fpf fmt "Param,%a,%a" str p (odoc_nestable_block_elements c) elems
  | `Raise (p, elems) ->
      fpf fmt "Raise,%a,%a" str p (odoc_nestable_block_elements c) elems
  | `Return elems ->
      fpf fmt "Return,%a" (odoc_nestable_block_elements c) elems
  | `See (kind, txt, elems) ->
      let kind =
        match kind with `Url -> "U" | `File -> "F" | `Document -> "D"
      in
      fpf fmt "See,%s,%a,%a" kind str txt
        (odoc_nestable_block_elements c)
        elems
  | `Since txt -> fpf fmt "Since,%a" str txt
  | `Before (p, elems) ->
      fpf fmt "Before,%a,%a" str p (odoc_nestable_block_elements c) elems
  | `Version txt -> fpf fmt "Version,%a" str txt
  | `Canonical ref -> fpf fmt "Canonical,%a" odoc_reference ref
  | `Inline -> fpf fmt "Inline"
  | `Open -> fpf fmt "Open"
  | `Closed -> fpf fmt "Closed"

let odoc_block_element c fmt = function
  | `Heading (lvl, lbl, content) ->
      let lvl = Int.to_string lvl in
      let lbl = match lbl with Some lbl -> lbl | None -> "" in
      fpf fmt "Heading,%s,%a,%a" lvl str lbl odoc_inline_elements content
  | `Tag tag -> fpf fmt "Tag,%a" (odoc_tag c) tag
  | #nestable_block_element as elm -> odoc_nestable_block_element c fmt elm

let odoc_docs c fmt elems = list (ign_loc (odoc_block_element c)) fmt elems

let docstring c text =
  if not c.conf.parse_docstrings then comment text
  else
    let location = Lexing.dummy_pos in
    let parsed = Odoc_parser.parse_comment_raw ~location ~text in
    Format.asprintf "Docstring(%a)%!" (odoc_docs c)
      parsed.Odoc_model.Error.value

let sort_attributes : attributes -> attributes =
  List.sort ~compare:Poly.compare

let make_mapper conf ~ignore_doc_comments =
  let doc_attribute = function
    | {attr_name= {txt= "ocaml.doc" | "ocaml.text"; _}; _} -> true
    | _ -> false
  in
  object (self)
    inherit Ppxlib.Ast_traverse.map as super

    (** Remove locations *)
    method! location _ = Location.none

    method! attribute attr =
      match (attr.attr_name, attr.attr_payload) with
      | ( {txt= ("ocaml.doc" | "ocaml.text") as txt; loc}
        , PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( { pexp_desc=
                          Pexp_constant (Pconst_string (doc, str_loc, None))
                      ; pexp_loc
                      ; pexp_attributes
                      ; _ }
                    , [] )
              ; pstr_loc } ] ) ->
          let doc' =
            if ignore_doc_comments then "IGNORED"
            else
              let c = {conf; normalize_code= self#structure} in
              docstring c doc
          in
          { attr_name= {txt; loc= self#location loc}
          ; attr_payload=
              self#payload
                (PStr
                   [ { pstr_desc=
                         Pstr_eval
                           ( { pexp_desc=
                                 Pexp_constant
                                   (Pconst_string
                                      (doc', self#location str_loc, None) )
                             ; pexp_loc= self#location pexp_loc
                             ; pexp_attributes=
                                 self#attributes pexp_attributes
                             ; pexp_loc_stack= [] }
                           , [] )
                     ; pstr_loc= self#location pstr_loc } ] )
          ; attr_loc= self#location attr.attr_loc }
      | _ -> super#attribute attr

    (** Sort attributes *)
    method! attributes atrs =
      let atrs =
        if ignore_doc_comments then
          List.filter atrs ~f:(fun a -> not (doc_attribute a))
        else atrs
      in
      super#attributes (sort_attributes atrs)

    method! expression exp =
      let exp = {exp with pexp_loc_stack= []} in
      let {pexp_desc; pexp_attributes; _} = exp in
      match pexp_desc with
      (* convert [(c1; c2); c3] to [c1; (c2; c3)] *)
      | Pexp_sequence
          ({pexp_desc= Pexp_sequence (e1, e2); pexp_attributes= []; _}, e3)
        ->
          self#expression
            (Exp.sequence e1 (Exp.sequence ~attrs:pexp_attributes e2 e3))
      | Pexp_poly ({pexp_desc= Pexp_constraint (e, t); _}, None) ->
          self#expression {exp with pexp_desc= Pexp_poly (e, Some t)}
      | Pexp_constraint (e, {ptyp_desc= Ptyp_poly ([], _t); _}) ->
          self#expression e
      | _ -> super#expression exp

    method! location_stack _ = []

    method! pattern pat =
      let {ppat_desc; ppat_loc= loc1; ppat_attributes= attrs1; _} = pat in
      (* normalize nested or patterns *)
      match ppat_desc with
      | Ppat_or
          ( pat1
          , { ppat_desc= Ppat_or (pat2, pat3)
            ; ppat_loc= loc2
            ; ppat_attributes= attrs2
            ; _ } ) ->
          self#pattern
            (Pat.or_ ~loc:loc1 ~attrs:attrs1
               (Pat.or_ ~loc:loc2 ~attrs:attrs2 pat1 pat2)
               pat3 )
      | _ -> super#pattern pat

    method! value_binding vb =
      let { pvb_pat= {ppat_desc; ppat_loc; ppat_attributes; _}
          ; pvb_expr
          ; pvb_loc
          ; pvb_attributes } =
        vb
      in
      match (ppat_desc, pvb_expr.pexp_desc) with
      (* recognize and undo the pattern of code introduced by
         ocaml/ocaml@fd0dc6a0fbf73323c37a73ea7e8ffc150059d6ff to fix
         https://caml.inria.fr/mantis/view.php?id=7344 *)
      | ( Ppat_constraint
            ( ({ppat_desc= Ppat_var _; _} as p0)
            , {ptyp_desc= Ptyp_poly ([], t0); _} )
        , Pexp_constraint (e0, t1) )
        when equal_core_type t0 t1 ->
          self#value_binding
            (Vb.mk ~loc:pvb_loc ~attrs:pvb_attributes p0
               (Exp.constraint_ ~loc:ppat_loc ~attrs:ppat_attributes e0 t0) )
      (* convert [let (x : t) = e] to [let x = (e : t)] *)
      | Ppat_constraint (p0, t0), _ ->
          self#value_binding
            (Vb.mk ~loc:pvb_loc ~attrs:pvb_attributes p0
               (Exp.constraint_ ~loc:ppat_loc ~attrs:ppat_attributes pvb_expr
                  t0 ) )
      | _ -> super#value_binding vb

    method! structure_item si =
      match si.pstr_desc with
      | Pstr_eval ({pexp_desc= Pexp_extension e; _}, []) ->
          let e = self#extension e in
          let pstr_loc = self#location si.pstr_loc in
          {pstr_desc= Pstr_extension (e, []); pstr_loc}
      | _ -> super#structure_item si

    method! structure si =
      let si =
        if ignore_doc_comments then
          List.filter si ~f:(fun si ->
              match si.pstr_desc with
              | Pstr_attribute a -> not (doc_attribute a)
              | _ -> true )
        else si
      in
      super#structure si

    method! signature si =
      let si =
        if ignore_doc_comments then
          List.filter si ~f:(fun si ->
              match si.psig_desc with
              | Psig_attribute a -> not (doc_attribute a)
              | _ -> true )
        else si
      in
      super#signature si

    method! class_signature si =
      let si =
        if ignore_doc_comments then
          let pcsig_fields =
            List.filter si.pcsig_fields ~f:(fun si ->
                match si.pctf_desc with
                | Pctf_attribute a -> not (doc_attribute a)
                | _ -> true )
          in
          {si with pcsig_fields}
        else si
      in
      super#class_signature si

    method! class_structure si =
      let si =
        if ignore_doc_comments then
          let pcstr_fields =
            List.filter si.pcstr_fields ~f:(fun si ->
                match si.pcf_desc with
                | Pcf_attribute a -> not (doc_attribute a)
                | _ -> true )
          in
          {si with pcstr_fields}
        else si
      in
      super#class_structure si
  end

let normalize fragment c =
  Traverse.map fragment (make_mapper c ~ignore_doc_comments:false)

let equal fragment ~ignore_doc_comments c ast1 ast2 =
  let map = Traverse.map fragment (make_mapper c ~ignore_doc_comments) in
  Traverse.equal fragment (map ast1) (map ast2)

let fold_docstrings =
  let doc_attribute = function
    | {attr_name= {txt= "ocaml.doc" | "ocaml.text"; _}; _} -> true
    | _ -> false
  in
  object
    inherit [(Location.t * string) list] Ppxlib.Ast_traverse.fold as super

    method! attribute attr docstrings =
      match (attr.attr_name, attr.attr_payload) with
      | ( {txt= "ocaml.doc" | "ocaml.text"; loc}
        , PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( { pexp_desc=
                          Pexp_constant (Pconst_string (doc, _, None))
                      ; _ }
                    , [] )
              ; _ } ] ) ->
          (loc, doc) :: docstrings
      | _ -> super#attribute attr docstrings

    method! attributes atrs =
      let atrs = List.filter atrs ~f:doc_attribute in
      super#attributes (sort_attributes atrs)
  end

let docstrings (type a) (fragment : a Traverse.fragment) s =
  Traverse.fold fragment fold_docstrings s []

type docstring_error =
  | Moved of Location.t * Location.t * string
  | Unstable of Location.t * string

let moved_docstrings fragment c s1 s2 =
  let c = {conf= c; normalize_code= normalize Structure c} in
  let d1 = docstrings fragment s1 in
  let d2 = docstrings fragment s2 in
  let equal (_, x) (_, y) =
    let b = String.equal (docstring c x) (docstring c y) in
    Caml.Printf.printf "Docstring equal? %b,\n%s\n%s\n" b (docstring c x)
      (docstring c y) ;
    b
  in
  let unstable (x, y) = Unstable (x, y) in
  match List.zip_exn d1 d2 with
  | exception _ ->
      (* We only return the ones that are not in both lists. *)
      (* [l1] contains the ones that disappeared. *)
      let l1 = List.filter d1 ~f:(fun x -> not (List.mem ~equal d2 x)) in
      let l1 = List.map ~f:unstable l1 in
      (* [l2] contains the ones that appeared. *)
      let l2 = List.filter d2 ~f:(fun x -> not (List.mem ~equal d1 x)) in
      let l2 = List.map ~f:unstable l2 in
      List.rev_append l1 l2
  | l ->
      let l = List.filter l ~f:(fun (x, y) -> not (equal x y)) in
      let l1, l2 = List.unzip l in
      let both, l1 =
        List.partition_map l1 ~f:(fun x ->
            match List.find l2 ~f:(equal x) with
            | Some (l, s) -> First (Moved (fst x, l, s))
            | None -> Second x )
      in
      let l2 = List.filter l2 ~f:(fun x -> not (List.mem ~equal l1 x)) in
      let l1 = List.map ~f:unstable l1 in
      let l2 = List.map ~f:unstable l2 in
      List.rev_append both (List.rev_append l1 l2)

let docstring conf =
  let normalize_code = normalize Structure conf in
  docstring {conf; normalize_code}
