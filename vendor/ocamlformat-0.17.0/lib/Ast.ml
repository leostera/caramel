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

(** Abstract syntax tree term *)

open Migrate_ast
open Parsetree

let init, register_reset, leading_nested_match_parens, parens_ite =
  let l = ref [] in
  let leading_nested_match_parens = ref false in
  let parens_ite = ref false in
  let register f = l := f :: !l in
  let init (conf : Conf.t) =
    leading_nested_match_parens := conf.leading_nested_match_parens ;
    parens_ite := conf.parens_ite ;
    List.iter !l ~f:(fun f -> f ())
  in
  (init, register, leading_nested_match_parens, parens_ite)

(** [fit_margin c x] returns [true] if and only if [x] does not exceed 1/3 of
    the margin. *)
let fit_margin (c : Conf.t) x = x * 3 < c.margin

(** 'Classes' of expressions which are parenthesized differently. *)
type cls = Let_match | Match | Non_apply | Sequence | Then | ThenElse

(** Predicates recognizing special symbol identifiers. *)

module Char_id = struct
  let is_kwdop = function
    | '$' | '&' | '*' | '+' | '-' | '/' | '<' | '=' | '>' | '@' | '^' | '|'
     |'!' | '%' | ':' | '?' ->
        true
    | _ -> false

  let is_infixop = function
    | '$' | '%' | '*' | '+' | '-' | '/' | '<' | '=' | '>' | '|' | '&' | '@'
     |'^' | '#' ->
        true
    | _ -> false
end

module Indexing_op = struct
  type brackets = Round | Square | Curly

  type custom_operator =
    {path: string list; opchars: string; brackets: brackets}

  type indexing_op =
    | Defined of expression * custom_operator
    | Extended of expression list * custom_operator
        (** Take a [Pexp_array] of at least 2 elements *)
    | Special of expression list * brackets
        (** Desugared to the application of the corresponding [get] function
            by the parser. (eg. [Array.get], [String.get]) *)

  type t =
    { lhs: expression
    ; op: indexing_op
    ; rhs: expression option
    ; loc: Location.t }

  type raw =
    { opchars: string
    ; brackets: brackets
    ; extended: bool  (** eg. [.*{;..}] *)
    ; has_rhs: bool  (** eg. [.*{}<-] *) }

  let parse ident =
    match String.chop_prefix ~prefix:"." ident with
    | None -> None
    | Some ident ->
        let ident, has_rhs =
          match String.chop_suffix ident ~suffix:"<-" with
          | Some ident -> (ident, true)
          | None -> (ident, false)
        in
        let find_suffix (suffix, brackets, extended) =
          match String.chop_suffix ident ~suffix with
          | None -> None
          | Some opchars -> Some {opchars; brackets; extended; has_rhs}
        in
        List.find_map ~f:find_suffix
          [ ("{}", Curly, false)
          ; ("[]", Square, false)
          ; ("()", Round, false)
          ; ("{;..}", Curly, true)
          ; ("[;..]", Square, true)
          ; ("(;..)", Round, true) ]

  let special ~id_tl ~args_tl brackets args =
    let op = Special (args, brackets) in
    match (id_tl, args_tl) with
    | "get", [] -> Some (op, None)
    | "set", [rhs] -> Some (op, Some rhs)
    | _ -> None

  let custom ~extended ~rhs op arg1 =
    match (extended, arg1) with
    | true, {pexp_desc= Pexp_array (_ :: _ :: _ as args); _} ->
        Some (Extended (args, op), rhs)
    | true, _ -> None
    | false, arg1 -> Some (Defined (arg1, op), rhs)

  let get_sugar_ident ident args =
    match (Longident.flatten ident, args) with
    | ["String"; id_tl], arg1 :: args_tl ->
        special ~id_tl ~args_tl Square [arg1]
    | ["Array"; id_tl], arg1 :: args_tl ->
        special ~id_tl ~args_tl Round [arg1]
    | ["Bigarray"; "Array1"; id_tl], arg1 :: args_tl ->
        special ~id_tl ~args_tl Curly [arg1]
    | ["Bigarray"; "Array2"; id_tl], arg1 :: arg2 :: args_tl ->
        special ~id_tl ~args_tl Curly [arg1; arg2]
    | ["Bigarray"; "Array3"; id_tl], arg1 :: arg2 :: arg3 :: args_tl ->
        special ~id_tl ~args_tl Curly [arg1; arg2; arg3]
    | ( ["Bigarray"; "Genarray"; id_tl]
      , {pexp_desc= Pexp_array args; _} :: args_tl )
      when List.length args > 3 ->
        special ~id_tl ~args_tl Curly args
    | ident, args -> (
      match List.rev ident with
      | [] -> None
      | ident :: path_rev -> (
          let path = List.rev path_rev in
          match parse ident with
          | None -> None
          | Some {opchars; brackets; extended; has_rhs} -> (
              let op = {path; opchars; brackets} in
              match (has_rhs, args) with
              | true, [arg1; rhs] -> custom ~extended ~rhs:(Some rhs) op arg1
              | false, [arg1] -> custom ~extended ~rhs:None op arg1
              | _, _ -> None ) ) )

  let rec all_args_unlabeled acc = function
    | [] -> Some (List.rev acc)
    | (Asttypes.Nolabel, e) :: tl -> all_args_unlabeled (e :: acc) tl
    | _ :: _ -> None

  let get_sugar ident args =
    match all_args_unlabeled [] args with
    | None | Some [] -> None
    | Some (lhs :: args) -> (
      match ident with
      | {pexp_desc= Pexp_ident {txt= ident; loc}; pexp_attributes= []; _}
      (* We only use the sugared form if it was already used in the source. *)
        when loc.loc_ghost -> (
        match get_sugar_ident ident args with
        | None -> None
        | Some (op, rhs) -> Some {lhs; op; rhs; loc} )
      | _ -> None )
end

module String_id = struct
  let is_prefix i =
    match i with
    | "!=" -> false
    | _ -> ( match i.[0] with '!' | '?' | '~' -> true | _ -> false )

  let is_monadic_binding s =
    String.length s > 3
    && (String.is_prefix s ~prefix:"let" || String.is_prefix s ~prefix:"and")
    && Option.is_none
         (String.lfindi s ~pos:3 ~f:(fun _ c -> not (Char_id.is_kwdop c)))

  let is_infix i =
    if Char_id.is_infixop i.[0] then true
    else
      match i with
      | "!=" | "land" | "lor" | "lxor" | "mod" | "::" | ":=" | "asr"
       |"lsl" | "lsr" | "or" | "||" ->
          true
      | _ -> is_monadic_binding i

  let is_hash_getter i =
    let is_infix_char c = Char.equal c '.' || Char_id.is_infixop c in
    match (i.[0], i.[String.length i - 1]) with
    | '#', ('#' | '.') when String.for_all i ~f:is_infix_char -> true
    | _ -> false

  let is_index_op ident = Option.is_some (Indexing_op.parse ident)

  let is_symbol i = is_prefix i || is_infix i || is_index_op i
end

module Longident = struct
  include Longident

  let test ~f = function Longident.Lident i -> f i | _ -> false

  let is_prefix = test ~f:String_id.is_prefix

  let is_monadic_binding = test ~f:String_id.is_monadic_binding

  let is_infix = test ~f:String_id.is_infix

  let is_hash_getter = test ~f:String_id.is_hash_getter

  let is_index_op i = Longident.last i |> String_id.is_index_op

  let is_symbol i = is_prefix i || is_infix i || is_index_op i

  (** [fit_margin c x] returns [true] if and only if [x] does not exceed 2/3
      of the margin. *)
  let fit_margin (c : Conf.t) x = x * 3 < c.margin * 2

  let is_simple c x =
    let rec length (x : Longident.t) =
      match x with
      | Lident x -> String.length x
      | Ldot (x, y) -> length x + 1 + String.length y
      | Lapply (x, y) -> length x + length y + 3
    in
    fit_margin c (length x)
end

module Attr = struct
  let is_doc = function
    | {attr_name= {Location.txt= "ocaml.doc" | "ocaml.text"; _}; _} -> true
    | _ -> false
end

module Exp = struct
  let test_id ~f = function
    | {pexp_desc= Pexp_ident {txt= i; _}; _} -> f i
    | _ -> false

  let is_prefix = test_id ~f:Longident.is_prefix

  let is_infix = test_id ~f:Longident.is_infix

  let is_index_op = test_id ~f:Longident.is_index_op

  let is_monadic_binding = test_id ~f:Longident.is_monadic_binding

  let is_symbol = test_id ~f:Longident.is_symbol

  let is_sequence exp =
    match exp.pexp_desc with
    | Pexp_sequence _ -> true
    | Pexp_extension
        ( ext
        , PStr
            [ { pstr_desc=
                  Pstr_eval (({pexp_desc= Pexp_sequence _; _} as e), [])
              ; _ } ] )
      when Source.extension_using_sugar ~name:ext ~payload:e.pexp_loc ->
        true
    | _ -> false

  let rec is_sugared_list' acc exp =
    match exp.pexp_desc with
    | Pexp_construct ({txt= Lident "[]"; _}, None) -> Ok (exp :: acc)
    | Pexp_construct
        ( {txt= Lident "::"; _}
        , Some
            { pexp_desc= Pexp_tuple [_; ({pexp_attributes= []; _} as tl)]
            ; pexp_attributes= []
            ; _ } ) ->
        is_sugared_list' (exp :: acc) tl
    | _ -> Error acc

  let is_sugared_list =
    let memo = Hashtbl.Poly.create () in
    register_reset (fun () -> Hashtbl.clear memo) ;
    fun exp ->
      match Hashtbl.find memo exp with
      | Some b -> b
      | None -> (
        match is_sugared_list' [] exp with
        | Error l ->
            List.iter ~f:(fun e -> Hashtbl.set memo ~key:e ~data:false) l ;
            false
        | Ok l ->
            List.iter ~f:(fun e -> Hashtbl.set memo ~key:e ~data:true) l ;
            true )

  let has_trailing_attributes {pexp_desc; pexp_attributes; _} =
    match pexp_desc with
    | Pexp_fun _ | Pexp_function _ | Pexp_ifthenelse _ | Pexp_match _
     |Pexp_newtype _ | Pexp_try _ ->
        false
    | _ -> List.exists pexp_attributes ~f:(Fn.non Attr.is_doc)

  let rec is_trivial c exp =
    match exp.pexp_desc with
    | Pexp_constant (Pconst_string (_, _, None)) -> true
    | Pexp_constant _ | Pexp_field _ | Pexp_ident _ | Pexp_send _ -> true
    | Pexp_construct (_, exp) -> Option.for_all exp ~f:(is_trivial c)
    | Pexp_apply (e0, [(_, e1)]) when is_prefix e0 -> is_trivial c e1
    | Pexp_apply
        ({pexp_desc= Pexp_ident {txt= Lident "not"; _}; _}, [(_, e1)]) ->
        is_trivial c e1
    | _ -> false

  let rec exposed_left e =
    match e.pexp_desc with
    | Pexp_apply (op, _) -> is_prefix op || exposed_left op
    | Pexp_field (e, _) -> exposed_left e
    | _ -> false

  (** [mem_cls cls exp] holds if [exp] is in the named class of expressions
      [cls]. *)
  let mem_cls cls ast =
    match (ast, cls) with
    | {pexp_desc= Pexp_ifthenelse (_, _, None); _}, (Non_apply | ThenElse)
     |{pexp_desc= Pexp_ifthenelse _; _}, Non_apply
     |( {pexp_desc= Pexp_sequence _; _}
      , (Non_apply | Sequence | Then | ThenElse) )
     |( {pexp_desc= Pexp_function _ | Pexp_match _ | Pexp_try _; _}
      , (Match | Let_match | Non_apply) )
     |( { pexp_desc=
            ( Pexp_fun _ | Pexp_let _ | Pexp_letop _ | Pexp_letexception _
            | Pexp_letmodule _ | Pexp_newtype _ | Pexp_open _ )
        ; _ }
      , (Let_match | Non_apply) ) ->
        true
    | _ -> false
end

module Pat = struct
  let is_simple {ppat_desc; _} =
    match ppat_desc with
    | Ppat_any | Ppat_constant _ | Ppat_var _
     |Ppat_variant (_, (None | Some {ppat_desc= Ppat_any; _}))
     |Ppat_construct (_, (None | Some {ppat_desc= Ppat_any; _})) ->
        true
    | _ -> false

  let has_trailing_attributes {ppat_desc; ppat_attributes; _} =
    match ppat_desc with
    | Ppat_construct (_, None)
     |Ppat_constant _ | Ppat_any | Ppat_var _
     |Ppat_variant (_, None)
     |Ppat_record _ | Ppat_array _ | Ppat_type _ | Ppat_unpack _
     |Ppat_extension _ | Ppat_open _ | Ppat_interval _ ->
        false
    | _ -> List.exists ppat_attributes ~f:(Fn.non Attr.is_doc)
end

let doc_atrs ?(acc = []) atrs =
  let docs, rev_atrs =
    List.fold atrs ~init:(acc, []) ~f:(fun (docs, rev_atrs) atr ->
        let open Asttypes in
        match atr with
        | { attr_name=
              { txt= ("ocaml.doc" | "ocaml.text") as txt
              ; loc= {loc_ghost= true; _} }
          ; attr_payload=
              PStr
                [ { pstr_desc=
                      Pstr_eval
                        ( { pexp_desc=
                              Pexp_constant (Pconst_string (doc, _, None))
                          ; pexp_loc= loc
                          ; pexp_attributes= []
                          ; _ }
                        , [] )
                  ; _ } ]
          ; _ } -> (
          match (txt, docs) with
          | "ocaml.doc", (_, false) :: _ ->
              (* cannot put two doc comment next to each other *)
              (docs, atr :: rev_atrs)
          | _ ->
              ( ({txt= doc; loc}, String.equal "ocaml.text" txt) :: docs
              , rev_atrs ) )
        | _ -> (docs, atr :: rev_atrs) )
  in
  let docs = match docs with [] -> None | l -> Some (List.rev l) in
  (docs, List.rev rev_atrs)

let rec mty_is_simple x =
  match x.pmty_desc with
  | Pmty_ident _ | Pmty_alias _ | Pmty_signature [] -> true
  | Pmty_signature (_ :: _)
   |Pmty_with (_, _ :: _ :: _)
   |Pmty_extension _
   |Pmty_functor (_, _) ->
      false
  | Pmty_typeof e -> mod_is_simple e
  | Pmty_with (t, ([] | [_])) -> mty_is_simple t

and mod_is_simple x =
  match x.pmod_desc with
  | Pmod_ident _ | Pmod_unpack _ | Pmod_structure [] -> true
  | Pmod_structure (_ :: _) | Pmod_extension _ | Pmod_functor (_, _) -> false
  | Pmod_constraint (e, t) -> mod_is_simple e && mty_is_simple t
  | Pmod_apply (a, b) -> mod_is_simple a && mod_is_simple b

module Mty = struct
  let is_simple = mty_is_simple

  let has_trailing_attributes {pmty_attributes; _} =
    List.exists pmty_attributes ~f:(Fn.non Attr.is_doc)
end

module Mod = struct
  let is_simple = mod_is_simple

  let has_trailing_attributes {pmod_attributes; _} =
    List.exists pmod_attributes ~f:(Fn.non Attr.is_doc)
end

module Cty = struct
  let rec is_simple x =
    match x.pcty_desc with
    | Pcty_constr _ | Pcty_signature {pcsig_fields= []; _} -> true
    | Pcty_signature {pcsig_fields= _ :: _; _}
     |Pcty_open _ | Pcty_extension _ ->
        false
    | Pcty_arrow (_, _, t) -> is_simple t
end

module Cl = struct
  let rec is_simple x =
    match x.pcl_desc with
    | Pcl_constr _ | Pcl_structure {pcstr_fields= []; _} -> true
    | Pcl_structure {pcstr_fields= _ :: _; _}
     |Pcl_let _ | Pcl_open _ | Pcl_extension _ ->
        false
    | Pcl_apply (e, _) | Pcl_fun (_, _, _, e) -> is_simple e
    | Pcl_constraint (e, t) -> is_simple e && Cty.is_simple t

  (** [mem_cls cls cl] holds if [cl] is in the named class of expressions
      [cls]. *)
  let mem_cls cls ast =
    match (ast, cls) with
    | {pcl_desc= Pcl_fun _; _}, Non_apply -> true
    | _ -> false
end

module Tyd = struct
  let is_simple x =
    match x.ptype_kind with
    | Ptype_abstract | Ptype_open -> true
    | Ptype_variant _ | Ptype_record _ -> false
end

module Structure_item = struct
  let has_doc itm =
    match itm.pstr_desc with
    | Pstr_attribute atr -> Option.is_some (fst (doc_atrs [atr]))
    | Pstr_eval (_, atrs)
     |Pstr_value (_, {pvb_attributes= atrs; _} :: _)
     |Pstr_primitive {pval_attributes= atrs; _}
     |Pstr_type (_, {ptype_attributes= atrs; _} :: _)
     |Pstr_typext {ptyext_attributes= atrs; _}
     |Pstr_recmodule ({pmb_expr= {pmod_attributes= atrs; _}; _} :: _)
     |Pstr_modtype {pmtd_attributes= atrs; _}
     |Pstr_open {popen_attributes= atrs; _}
     |Pstr_extension (_, atrs)
     |Pstr_class_type ({pci_attributes= atrs; _} :: _)
     |Pstr_class ({pci_attributes= atrs; _} :: _) ->
        Option.is_some (fst (doc_atrs atrs))
    | Pstr_include
        {pincl_mod= {pmod_attributes= atrs1; _}; pincl_attributes= atrs2; _}
     |Pstr_exception
        { ptyexn_attributes= atrs1
        ; ptyexn_constructor= {pext_attributes= atrs2; _}
        ; _ }
     |Pstr_module
        {pmb_attributes= atrs1; pmb_expr= {pmod_attributes= atrs2; _}; _} ->
        Option.is_some (fst (doc_atrs (List.append atrs1 atrs2)))
    | Pstr_value (_, [])
     |Pstr_type (_, [])
     |Pstr_recmodule []
     |Pstr_class_type []
     |Pstr_class [] ->
        false

  let is_simple (itm, c) =
    match c.Conf.module_item_spacing with
    | `Compact | `Preserve ->
        Location.is_single_line itm.pstr_loc c.Conf.margin
    | `Sparse -> (
      match itm.pstr_desc with
      | Pstr_include {pincl_mod= me; _} | Pstr_module {pmb_expr= me; _} ->
          let rec is_simple_mod me =
            match me.pmod_desc with
            | Pmod_apply (me1, me2) -> is_simple_mod me1 && is_simple_mod me2
            | Pmod_functor (_, me) -> is_simple_mod me
            | Pmod_ident i -> Longident.is_simple c i.txt
            | _ -> false
          in
          is_simple_mod me
      | Pstr_open {popen_expr= {pmod_desc= Pmod_ident i; _}; _} ->
          Longident.is_simple c i.txt
      | _ -> false )

  let allow_adjacent (itmI, cI) (itmJ, cJ) =
    match Conf.(cI.module_item_spacing, cJ.module_item_spacing) with
    | `Compact, `Compact -> (
      match (itmI.pstr_desc, itmJ.pstr_desc) with
      | Pstr_eval _, Pstr_eval _
       |Pstr_value _, Pstr_value _
       |Pstr_primitive _, Pstr_primitive _
       |(Pstr_type _ | Pstr_typext _), (Pstr_type _ | Pstr_typext _)
       |Pstr_exception _, Pstr_exception _
       |( (Pstr_module _ | Pstr_recmodule _ | Pstr_open _ | Pstr_include _)
        , (Pstr_module _ | Pstr_recmodule _ | Pstr_open _ | Pstr_include _) )
       |Pstr_modtype _, Pstr_modtype _
       |Pstr_class _, Pstr_class _
       |Pstr_class_type _, Pstr_class_type _
       |Pstr_attribute _, Pstr_attribute _
       |Pstr_extension _, Pstr_extension _ ->
          true
      | _ -> false )
    | _ -> true

  let break_between s ~cmts ~has_cmts_before ~has_cmts_after (i1, c1) (i2, c2)
      =
    has_cmts_after cmts i1.pstr_loc
    || has_cmts_before cmts i2.pstr_loc
    || has_doc i1 || has_doc i2
    ||
    match Conf.(c1.module_item_spacing, c2.module_item_spacing) with
    | `Preserve, `Preserve ->
        Source.empty_line_between s i1.pstr_loc.loc_end i2.pstr_loc.loc_start
    | _ ->
        (not (is_simple (i1, c1)))
        || (not (is_simple (i2, c2)))
        || not (allow_adjacent (i1, c1) (i2, c2))
end

module Signature_item = struct
  let has_doc itm =
    match itm.psig_desc with
    | Psig_attribute atr -> Option.is_some (fst (doc_atrs [atr]))
    | Psig_value {pval_attributes= atrs; _}
     |Psig_type (_, {ptype_attributes= atrs; _} :: _)
     |Psig_typesubst ({ptype_attributes= atrs; _} :: _)
     |Psig_typext {ptyext_attributes= atrs; _}
     |Psig_modtype {pmtd_attributes= atrs; _}
     |Psig_modsubst {pms_attributes= atrs; _}
     |Psig_open {popen_attributes= atrs; _}
     |Psig_extension (_, atrs)
     |Psig_class_type ({pci_attributes= atrs; _} :: _)
     |Psig_class ({pci_attributes= atrs; _} :: _) ->
        Option.is_some (fst (doc_atrs atrs))
    | Psig_recmodule
        ({pmd_type= {pmty_attributes= atrs1; _}; pmd_attributes= atrs2; _}
        :: _ )
     |Psig_include
        {pincl_mod= {pmty_attributes= atrs1; _}; pincl_attributes= atrs2; _}
     |Psig_exception
        { ptyexn_attributes= atrs1
        ; ptyexn_constructor= {pext_attributes= atrs2; _}
        ; _ }
     |Psig_module
        {pmd_attributes= atrs1; pmd_type= {pmty_attributes= atrs2; _}; _} ->
        Option.is_some (fst (doc_atrs (List.append atrs1 atrs2)))
    | Psig_type (_, [])
     |Psig_typesubst []
     |Psig_recmodule []
     |Psig_class_type []
     |Psig_class [] ->
        false

  let is_simple (itm, c) =
    match c.Conf.module_item_spacing with
    | `Compact | `Preserve ->
        Location.is_single_line itm.psig_loc c.Conf.margin
    | `Sparse -> (
      match itm.psig_desc with
      | Psig_open {popen_expr= i; _}
       |Psig_module {pmd_type= {pmty_desc= Pmty_alias i; _}; _}
       |Psig_modsubst {pms_manifest= i; _} ->
          Longident.is_simple c i.txt
      | _ -> false )

  let allow_adjacent (itmI, cI) (itmJ, cJ) =
    match Conf.(cI.module_item_spacing, cJ.module_item_spacing) with
    | `Compact, `Compact -> (
      match (itmI.psig_desc, itmJ.psig_desc) with
      | Psig_value _, Psig_value _
       |( (Psig_type _ | Psig_typesubst _ | Psig_typext _)
        , (Psig_type _ | Psig_typesubst _ | Psig_typext _) )
       |Psig_exception _, Psig_exception _
       |( ( Psig_module _ | Psig_modsubst _ | Psig_recmodule _ | Psig_open _
          | Psig_include _ )
        , ( Psig_module _ | Psig_modsubst _ | Psig_recmodule _ | Psig_open _
          | Psig_include _ ) )
       |Psig_modtype _, Psig_modtype _
       |Psig_class _, Psig_class _
       |Psig_class_type _, Psig_class_type _
       |Psig_attribute _, Psig_attribute _
       |Psig_extension _, Psig_extension _ ->
          true
      | _ -> false )
    | _ -> true

  let break_between s ~cmts ~has_cmts_before ~has_cmts_after (i1, c1) (i2, c2)
      =
    has_cmts_after cmts i1.psig_loc
    || has_cmts_before cmts i2.psig_loc
    || has_doc i1 || has_doc i2
    ||
    match Conf.(c1.module_item_spacing, c2.module_item_spacing) with
    | `Preserve, `Preserve ->
        Source.empty_line_between s i1.psig_loc.loc_end i2.psig_loc.loc_start
    | _ ->
        (not (is_simple (i1, c1)))
        || (not (is_simple (i2, c2)))
        || not (allow_adjacent (i1, c1) (i2, c2))
end

module Vb = struct
  let is_simple (i, c) =
    Poly.(c.Conf.module_item_spacing = `Compact)
    && Location.is_single_line i.pvb_loc c.Conf.margin

  let break_between ~cmts ~has_cmts_before ~has_cmts_after (i1, c1) (i2, c2)
      =
    has_cmts_after cmts i1.pvb_loc
    || has_cmts_before cmts i2.pvb_loc
    || (not (is_simple (i1, c1)))
    || not (is_simple (i2, c2))
end

type toplevel_item =
  [`Item of structure_item | `Directive of toplevel_directive]

(** Ast terms of various forms. *)
module T = struct
  type t =
    | Pld of payload
    | Typ of core_type
    | Cty of class_type
    | Pat of pattern
    | Exp of expression
    | Vb of value_binding
    | Cl of class_expr
    | Mty of module_type
    | Mod of module_expr
    | Sig of signature_item
    | Str of structure_item
    | Tli of toplevel_item
    | Top

  let dump fs = function
    | Pld l -> Format.fprintf fs "Pld:@\n%a" Printast.payload l
    | Typ t -> Format.fprintf fs "Typ:@\n%a" Pprintast.core_type t
    | Pat p -> Format.fprintf fs "Pat:@\n%a" Pprintast.pattern p
    | Exp e ->
        Format.fprintf fs "Exp:@\n%a@\n@\n%a" Pprintast.expression e
          Printast.expression e
    | Vb b ->
        let str =
          let open Ast_helper in
          Str.value Nonrecursive [b]
        in
        Format.fprintf fs "Vb:@\n%a@\n@\n%a" Pprintast.structure [str]
          Printast.implementation [str]
    | Cl cl ->
        let str =
          let open Ast_helper in
          Str.class_ [Ci.mk {txt= ""; loc= Location.none} cl]
        in
        Format.fprintf fs "Cl:@\n%a@\n%a" Pprintast.structure [str]
          Printast.implementation [str]
    | Mty mt ->
        let si =
          let open Ast_helper in
          Sig.modtype (Mtd.mk {txt= ""; loc= Location.none} ~typ:mt)
        in
        Format.fprintf fs "Mty:@\n%a@\n%a" Pprintast.signature [si]
          Printast.interface [si]
    | Cty cty ->
        let si =
          let open Ast_helper in
          Sig.class_type [Ci.mk {txt= ""; loc= Location.none} cty]
        in
        Format.fprintf fs "Cty:@\n%a@\n%a" Pprintast.signature [si]
          Printast.interface [si]
    | Mod m ->
        let m =
          let open Ast_helper in
          Str.module_ (Mb.mk {txt= None; loc= Location.none} m)
        in
        Format.fprintf fs "Mod:@\n%a@\n%a" Pprintast.structure [m]
          Printast.implementation [m]
    | Sig s ->
        Format.fprintf fs "Sig:@\n%a@\n%a" Pprintast.signature [s]
          Printast.interface [s]
    | Str s | Tli (`Item s) ->
        Format.fprintf fs "Str:@\n%a@\n%a" Pprintast.structure [s]
          Printast.implementation [s]
    | Tli (`Directive d) ->
        Format.fprintf fs "Dir:@\n%a" Pprintast.toplevel_phrase (Ptop_dir d)
    | Top -> Format.pp_print_string fs "Top"
end

include T

let is_top = function Top -> true | _ -> false

let attributes = function
  | Pld _ -> []
  | Typ x -> x.ptyp_attributes
  | Cty x -> x.pcty_attributes
  | Pat x -> x.ppat_attributes
  | Exp x -> x.pexp_attributes
  | Vb x -> x.pvb_attributes
  | Cl x -> x.pcl_attributes
  | Mty x -> x.pmty_attributes
  | Mod x -> x.pmod_attributes
  | Sig _ -> []
  | Str _ -> []
  | Top -> []
  | Tli _ -> []

let location = function
  | Pld _ -> Location.none
  | Typ x -> x.ptyp_loc
  | Cty x -> x.pcty_loc
  | Pat x -> x.ppat_loc
  | Exp x -> x.pexp_loc
  | Vb x -> x.pvb_loc
  | Cl x -> x.pcl_loc
  | Mty x -> x.pmty_loc
  | Mod x -> x.pmod_loc
  | Sig x -> x.psig_loc
  | Str x -> x.pstr_loc
  | Tli (`Item x) -> x.pstr_loc
  | Tli (`Directive x) -> x.pdir_loc
  | Top -> Location.none

let break_between_modules ~cmts ~has_cmts_before ~has_cmts_after (i1, c1)
    (i2, c2) =
  let has_doc itm = Option.is_some (fst (doc_atrs (attributes itm))) in
  let is_simple (itm, c) =
    Location.is_single_line (location itm) c.Conf.margin
  in
  has_cmts_after cmts (location i1)
  || has_cmts_before cmts (location i2)
  || has_doc i1 || has_doc i2
  || (not (is_simple (i1, c1)))
  || not (is_simple (i2, c2))

let break_between s ~cmts ~has_cmts_before ~has_cmts_after (i1, c1) (i2, c2)
    =
  match (i1, i2) with
  | Str i1, Str i2 ->
      Structure_item.break_between s ~cmts ~has_cmts_before ~has_cmts_after
        (i1, c1) (i2, c2)
  | Sig i1, Sig i2 ->
      Signature_item.break_between s ~cmts ~has_cmts_before ~has_cmts_after
        (i1, c1) (i2, c2)
  | Vb i1, Vb i2 ->
      Vb.break_between ~cmts ~has_cmts_before ~has_cmts_after (i1, c1)
        (i2, c2)
  | Mty _, Mty _ ->
      break_between_modules ~cmts ~has_cmts_before ~has_cmts_after (i1, c1)
        (i2, c2)
  | Mod _, Mod _ ->
      break_between_modules ~cmts ~has_cmts_before ~has_cmts_after (i1, c1)
        (i2, c2)
  | Tli (`Item i1), Tli (`Item i2) ->
      Structure_item.break_between s ~cmts ~has_cmts_before ~has_cmts_after
        (i1, c1) (i2, c2)
  | Tli (`Directive _), Tli (`Directive _) | Tli _, Tli _ ->
      true (* always break between an item and a directive *)
  | _ -> assert false

(** Term-in-context, [{ctx; ast}] records that [ast] is (considered to be) an
    immediate sub-term of [ctx] as assumed by the operations in
    [Requires_sub_terms]. *)
module rec In_ctx : sig
  type 'a xt = private {ctx: T.t; ast: 'a}

  val sub_ast : ctx:T.t -> T.t -> T.t xt

  val sub_typ : ctx:T.t -> core_type -> core_type xt

  val sub_cty : ctx:T.t -> class_type -> class_type xt

  val sub_pat : ctx:T.t -> pattern -> pattern xt

  val sub_exp : ctx:T.t -> expression -> expression xt

  val sub_cl : ctx:T.t -> class_expr -> class_expr xt

  val sub_mty : ctx:T.t -> module_type -> module_type xt

  val sub_mod : ctx:T.t -> module_expr -> module_expr xt

  val sub_sig : ctx:T.t -> signature_item -> signature_item xt

  val sub_str : ctx:T.t -> structure_item -> structure_item xt
end = struct
  open Requires_sub_terms

  type 'a xt = {ctx: T.t; ast: 'a}

  let sub_ast ~ctx ast = {ctx; ast}

  let sub_typ ~ctx typ = check parenze_typ {ctx; ast= typ}

  let sub_cty ~ctx cty = {ctx; ast= cty}

  let sub_pat ~ctx pat = check parenze_pat {ctx; ast= pat}

  let sub_exp ~ctx exp = check parenze_exp {ctx; ast= exp}

  let sub_cl ~ctx cl = {ctx; ast= cl}

  let sub_mty ~ctx mty = {ctx; ast= mty}

  let sub_mod ~ctx mod_ = {ctx; ast= mod_}

  let sub_sig ~ctx sig_ = {ctx; ast= sig_}

  let sub_str ~ctx str = {ctx; ast= str}
end

(** Operations determining precedence and necessary parenthesization of terms
    based on their super-terms. *)
and Requires_sub_terms : sig
  val is_simple :
    Conf.t -> (expression In_ctx.xt -> int) -> expression In_ctx.xt -> bool

  val exposed_right_exp : cls -> expression -> bool

  val prec_ast : T.t -> Prec.t option

  val parenze_typ : core_type In_ctx.xt -> bool

  val parenze_mty : module_type In_ctx.xt -> bool

  val parenze_mod : module_expr In_ctx.xt -> bool

  val parenze_cty : class_type In_ctx.xt -> bool

  val parenze_cl : class_expr In_ctx.xt -> bool

  val parenze_pat : pattern In_ctx.xt -> bool

  val parenze_exp : expression In_ctx.xt -> bool

  val parenze_nested_exp : expression In_ctx.xt -> bool

  val is_displaced_infix_op : expression In_ctx.xt -> bool
end = struct
  open In_ctx

  (* This module uses physical equality extensively to detect sub-terms. *)

  let ( == ) = Base.phys_equal

  let dump ctx ast fs =
    Format.fprintf fs "ast: %a@\nctx: %a@\n" T.dump ast T.dump ctx

  let assert_no_raise ~f ~dump x =
    assert (
      try
        ignore (f x) ;
        true
      with exc ->
        let bt = Caml.Printexc.get_backtrace () in
        dump x Format.err_formatter ;
        Format.eprintf "%s%!" bt ;
        raise exc )

  (** Predicates to check the claimed sub-term relation. *)

  let check_typ {ctx; ast= typ} =
    let f tI = typ == tI in
    let fst_f (tI, _) = typ == tI in
    let snd_f (_, tI) = typ == tI in
    let check_cstr = function
      | Pcstr_tuple t1N -> List.exists t1N ~f
      | Pcstr_record ld1N ->
          List.exists ld1N ~f:(fun {pld_type; _} -> typ == pld_type)
    in
    let check_ext {pext_kind; _} =
      match pext_kind with
      | Pext_decl (cstr, t0) -> check_cstr cstr || Option.exists t0 ~f
      | _ -> false
    in
    let check_typext {ptyext_params; ptyext_constructors; _} =
      List.exists ptyext_params ~f:fst_f
      || List.exists ptyext_constructors ~f:check_ext
    in
    let check_typexn {ptyexn_constructor; _} =
      check_ext ptyexn_constructor
    in
    let check_type {ptype_params; ptype_cstrs; ptype_kind; ptype_manifest; _}
        =
      List.exists ptype_params ~f:fst_f
      || List.exists ptype_cstrs ~f:(fun (t1, t2, _) ->
             typ == t1 || typ == t2 )
      || ( match ptype_kind with
         | Ptype_variant cd1N ->
             List.exists cd1N ~f:(fun {pcd_args; pcd_res; _} ->
                 check_cstr pcd_args || Option.exists pcd_res ~f )
         | Ptype_record ld1N ->
             List.exists ld1N ~f:(fun {pld_type; _} -> typ == pld_type)
         | _ -> false )
      || Option.exists ptype_manifest ~f
    in
    let check_pcstr_fields pcstr_fields =
      List.exists pcstr_fields ~f:(fun f ->
          match f.pcf_desc with
          | Pcf_inherit (_, _, _) -> false
          | Pcf_val (_, _, Cfk_virtual t) -> typ == t
          | Pcf_val
              (_, _, Cfk_concrete (_, {pexp_desc= Pexp_constraint (_, t); _}))
            ->
              typ == t
          | Pcf_val (_, _, Cfk_concrete _) -> false
          | Pcf_method (_, _, Cfk_virtual t) -> typ == t
          | Pcf_method
              (_, _, Cfk_concrete (_, {pexp_desc= Pexp_constraint (_, t); _}))
            ->
              typ == t
          | Pcf_method
              (_, _, Cfk_concrete (_, {pexp_desc= Pexp_poly (e, topt); _}))
            ->
              let rec loop = function
                | {pexp_desc= Pexp_newtype (_, e); _} -> loop e
                | {pexp_desc= Pexp_constraint (_, t); _} -> t == typ
                | {pexp_desc= Pexp_fun (_, _, _, e); _} -> loop e
                | _ -> false
              in
              (match topt with None -> false | Some t -> typ == t)
              || loop e
          | Pcf_method (_, _, Cfk_concrete _) -> false
          | Pcf_constraint (t1, t2) -> t1 == typ || t2 == typ
          | Pcf_initializer _ | Pcf_attribute _ | Pcf_extension _ -> false )
    in
    let check_class_type l =
      List.exists l ~f:(fun {pci_expr= {pcty_desc; _}; pci_params; _} ->
          List.exists pci_params ~f:(fun (t, _) -> t == typ)
          ||
          match pcty_desc with
          | Pcty_constr (_, l) -> List.exists l ~f:(fun x -> x == typ)
          | Pcty_arrow (_, t, _) -> t == typ
          | _ -> false )
    in
    match ctx with
    | Pld (PTyp t1) -> assert (typ == t1)
    | Pld _ -> assert false
    | Typ ctx -> (
      match ctx.ptyp_desc with
      | Ptyp_extension _ -> ()
      | Ptyp_any | Ptyp_var _ -> assert false
      | Ptyp_alias (t1, _) | Ptyp_poly (_, t1) -> assert (typ == t1)
      | Ptyp_arrow (_, t1, t2) -> assert (typ == t1 || typ == t2)
      | Ptyp_tuple t1N | Ptyp_constr (_, t1N) -> assert (List.exists t1N ~f)
      | Ptyp_variant (r1N, _, _) ->
          assert (
            List.exists r1N ~f:(function
              | {prf_desc= Rtag (_, _, t1N); _} -> List.exists t1N ~f
              | {prf_desc= Rinherit t1; _} -> typ == t1 ) )
      | Ptyp_package (_, it1N) -> assert (List.exists it1N ~f:snd_f)
      | Ptyp_object (fields, _) ->
          assert (
            List.exists fields ~f:(function
              | {pof_desc= Otag (_, t1); _} -> typ == t1
              | {pof_desc= Oinherit t1; _} -> typ == t1 ) )
      | Ptyp_class (_, l) -> assert (List.exists l ~f) )
    | Cty {pcty_desc; _} ->
        assert (
          match pcty_desc with
          | Pcty_constr (_, l) -> List.exists l ~f
          | Pcty_arrow (_, t, _) -> t == typ
          | Pcty_open _ -> false
          | Pcty_extension _ -> false
          | Pcty_signature {pcsig_self; pcsig_fields; _} ->
              pcsig_self == typ
              || List.exists pcsig_fields ~f:(fun {pctf_desc; _} ->
                     match pctf_desc with
                     | Pctf_constraint (t1, t2) -> t1 == typ || t2 == typ
                     | Pctf_val (_, _, _, t) -> t == typ
                     | Pctf_method (_, _, _, t) -> t == typ
                     | Pctf_inherit _ -> false
                     | Pctf_attribute _ -> false
                     | Pctf_extension _ -> false ) )
    | Pat ctx -> (
      match ctx.ppat_desc with
      | Ppat_constraint (_, t1) -> assert (typ == t1)
      | Ppat_extension (_, PTyp t) -> assert (typ == t)
      | _ -> assert false )
    | Exp ctx -> (
      match ctx.pexp_desc with
      | Pexp_constraint (_, ({ptyp_desc= Ptyp_package (_, it1N); _} as ty))
        ->
          assert (typ == ty || List.exists it1N ~f:snd_f)
      | Pexp_constraint (_, t1)
       |Pexp_coerce (_, None, t1)
       |Pexp_poly (_, Some t1)
       |Pexp_extension (_, PTyp t1) ->
          assert (typ == t1)
      | Pexp_coerce (_, Some t1, t2) -> assert (typ == t1 || typ == t2)
      | Pexp_letexception (ext, _) -> assert (check_ext ext)
      | Pexp_object {pcstr_fields; _} ->
          assert (check_pcstr_fields pcstr_fields)
      | Pexp_record (en1, _) ->
          assert (
            List.exists en1 ~f:(fun (_, e) ->
                match e with
                | {pexp_desc= Pexp_constraint (_, t); pexp_attributes= []; _}
                  ->
                    t == typ
                | _ -> false ) )
      | _ -> assert false )
    | Vb _ -> assert false
    | Cl {pcl_desc; _} ->
        assert (
          match pcl_desc with
          | Pcl_constr (_, l) -> List.exists l ~f
          | Pcl_constraint _ -> false
          | Pcl_let _ -> false
          | Pcl_apply _ -> false
          | Pcl_fun _ -> false
          | Pcl_open _ -> false
          | Pcl_extension _ -> false
          | Pcl_structure {pcstr_fields; _} ->
              check_pcstr_fields pcstr_fields )
    | Mty ctx ->
        let rec loop m =
          match m with
          | Pmty_with (m, c1N) ->
              List.exists c1N ~f:(function
                | Pwith_type (_, d1) | Pwith_typesubst (_, d1) ->
                    check_type d1
                | _ -> false )
              || loop m.pmty_desc
          | _ -> false
        in
        assert (loop ctx.pmty_desc)
    | Mod ctx -> (
      match ctx.pmod_desc with
      | Pmod_unpack e1 -> (
        match e1.pexp_desc with
        | Pexp_constraint (_, ({ptyp_desc= Ptyp_package (_, it1N); _} as ty))
          ->
            assert (typ == ty || List.exists it1N ~f:snd_f)
        | Pexp_constraint (_, t1)
         |Pexp_coerce (_, None, t1)
         |Pexp_poly (_, Some t1)
         |Pexp_extension (_, PTyp t1) ->
            assert (typ == t1)
        | Pexp_coerce (_, Some t1, t2) -> assert (typ == t1 || typ == t2)
        | Pexp_letexception (ext, _) -> assert (check_ext ext)
        | Pexp_object {pcstr_fields; _} ->
            assert (check_pcstr_fields pcstr_fields)
        | _ -> assert false )
      | _ -> assert false )
    | Sig ctx -> (
      match ctx.psig_desc with
      | Psig_value {pval_type= t1; _} -> assert (typ == t1)
      | Psig_type (_, d1N) -> assert (List.exists d1N ~f:check_type)
      | Psig_typesubst d1N -> assert (List.exists d1N ~f:check_type)
      | Psig_typext typext -> assert (check_typext typext)
      | Psig_exception ext -> assert (check_typexn ext)
      | Psig_class_type l -> assert (check_class_type l)
      | Psig_class l -> assert (check_class_type l)
      | _ -> assert false )
    | Str ctx -> (
      match ctx.pstr_desc with
      | Pstr_primitive {pval_type= t1; _} -> assert (typ == t1)
      | Pstr_type (_, d1N) -> assert (List.exists d1N ~f:check_type)
      | Pstr_typext typext -> assert (check_typext typext)
      | Pstr_exception ext -> assert (check_typexn ext)
      | Pstr_class l ->
          assert (
            List.exists l ~f:(fun {pci_expr= {pcl_desc; _}; pci_params; _} ->
                List.exists pci_params ~f:(fun (t, _) -> t == typ)
                ||
                match pcl_desc with
                | Pcl_constr (_, l) -> List.exists l ~f:(fun x -> x == typ)
                | _ -> false ) )
      | Pstr_class_type l -> assert (check_class_type l)
      | Pstr_extension ((_, PTyp t), _) -> assert (t == typ)
      | Pstr_extension (_, _) -> assert false
      | _ -> assert false )
    | Top | Tli _ -> assert false

  let assert_check_typ xtyp =
    let dump {ctx; ast= typ} = dump ctx (Typ typ) in
    assert_no_raise ~f:check_typ ~dump xtyp

  let check_cty {ctx; ast= cty} =
    let check_class_type l =
      List.exists l ~f:(fun {pci_expr; _} ->
          let rec loop x =
            x == cty
            ||
            match x.pcty_desc with
            | Pcty_arrow (_, _, x) -> loop x
            | _ -> false
          in
          loop pci_expr )
    in
    match (ctx : t) with
    | Exp _ -> assert false
    | Vb _ -> assert false
    | Pld _ -> assert false
    | Str ctx -> (
      match ctx.pstr_desc with
      | Pstr_class_type l -> assert (check_class_type l)
      | Pstr_class l ->
          assert (
            List.exists l ~f:(fun {pci_expr; _} ->
                let rec loop x =
                  match x.pcl_desc with
                  | Pcl_fun (_, _, _, x) -> loop x
                  | Pcl_constraint (_, x) -> x == cty
                  | _ -> false
                in
                loop pci_expr ) )
      | _ -> assert false )
    | Sig ctx -> (
      match ctx.psig_desc with
      | Psig_class_type l -> assert (check_class_type l)
      | Psig_class l -> assert (check_class_type l)
      | _ -> assert false )
    | Cty {pcty_desc; _} -> (
      match pcty_desc with
      | Pcty_arrow (_, _, t) -> assert (t == cty)
      | Pcty_signature {pcsig_fields; _} ->
          assert (
            List.exists pcsig_fields ~f:(fun {pctf_desc; _} ->
                match pctf_desc with
                | Pctf_inherit t -> t == cty
                | Pctf_val _ -> false
                | Pctf_method _ -> false
                | Pctf_constraint _ -> false
                | Pctf_attribute _ -> false
                | Pctf_extension _ -> false ) )
      | Pcty_open (_, t) -> assert (t == cty)
      | Pcty_constr _ -> assert false
      | Pcty_extension _ -> assert false )
    | Top -> assert false
    | Tli _ -> assert false
    | Typ _ -> assert false
    | Pat _ -> assert false
    | Cl ctx ->
        assert (
          match ctx.pcl_desc with
          | Pcl_fun (_, _, _, _) -> false
          | Pcl_constr _ -> false
          | Pcl_structure _ -> false
          | Pcl_apply _ -> false
          | Pcl_let (_, _, _) -> false
          | Pcl_constraint (_, x) -> x == cty
          | Pcl_extension _ -> false
          | Pcl_open _ -> false )
    | Mty _ -> assert false
    | Mod _ -> assert false

  let assert_check_cty xcty =
    let dump {ctx; ast= cty} = dump ctx (Cty cty) in
    assert_no_raise ~f:check_cty ~dump xcty

  let check_cl {ctx; ast= cl} =
    let check_pcstr_fields pcstr_fields =
      List.exists pcstr_fields ~f:(fun f ->
          match f.pcf_desc with
          | Pcf_inherit (_, x, _) -> x == cl
          | _ -> false )
    in
    match (ctx : t) with
    | Exp e -> (
      match e.pexp_desc with
      | Pexp_object {pcstr_fields; _} ->
          assert (check_pcstr_fields pcstr_fields)
      | _ -> assert false )
    | Vb _ -> assert false
    | Pld _ -> assert false
    | Str ctx -> (
      match ctx.pstr_desc with
      | Pstr_class l ->
          assert (
            List.exists l ~f:(fun {pci_expr; _} ->
                let rec loop x =
                  cl == x
                  ||
                  match x.pcl_desc with
                  | Pcl_fun (_, _, _, x) -> loop x
                  | Pcl_constraint (x, _) -> loop x
                  | _ -> false
                in
                loop pci_expr ) )
      | _ -> assert false )
    | Sig _ -> assert false
    | Cty _ -> assert false
    | Top -> assert false
    | Tli _ -> assert false
    | Typ _ -> assert false
    | Pat _ -> assert false
    | Cl {pcl_desc; _} ->
        assert (
          match pcl_desc with
          | Pcl_structure {pcstr_fields; _} ->
              check_pcstr_fields pcstr_fields
          | Pcl_fun (_, _, _, x) -> x == cl
          | Pcl_apply (x, _) -> x == cl
          | Pcl_let (_, _, x) -> x == cl
          | Pcl_constraint (x, _) -> x == cl
          | Pcl_open (_, x) -> x == cl
          | Pcl_constr _ -> false
          | Pcl_extension _ -> false )
    | Mty _ -> assert false
    | Mod _ -> assert false

  let assert_check_cl xcl =
    let dump {ctx; ast= cl} = dump ctx (Cl cl) in
    assert_no_raise ~f:check_cl ~dump xcl

  let check_pat {ctx; ast= pat} =
    let check_pcstr_fields pcstr_fields =
      List.exists pcstr_fields ~f:(fun {pcf_desc; _} ->
          match pcf_desc with
          | Pcf_initializer _ -> false
          | Pcf_val (_, _, _) -> false
          | Pcf_method (_, _, _) -> false
          | Pcf_extension (_, PPat (p, _)) -> p == pat
          | Pcf_extension (_, _) -> false
          | Pcf_inherit _ -> false
          | Pcf_constraint _ -> false
          | Pcf_attribute _ -> false )
    in
    let check_extensions = function PPat (p, _) -> p == pat | _ -> false in
    let check_subpat ppat =
      ppat == pat
      ||
      match ppat.ppat_desc with
      | Ppat_constraint (p, _) -> p == pat
      | _ -> false
    in
    let check_bindings l =
      List.exists l ~f:(fun {pvb_pat; _} -> check_subpat pvb_pat)
    in
    match ctx with
    | Pld (PPat (p1, _)) -> assert (p1 == pat)
    | Pld _ -> assert false
    | Typ ctx -> (
      match ctx.ptyp_desc with
      | Ptyp_extension (_, ext) -> assert (check_extensions ext)
      | _ -> assert false )
    | Pat ctx -> (
        let f pI = pI == pat in
        let snd_f (_, pI) = pI == pat in
        match ctx.ppat_desc with
        | Ppat_array p1N | Ppat_tuple p1N -> assert (List.exists p1N ~f)
        | Ppat_record (p1N, _) -> assert (List.exists p1N ~f:snd_f)
        | Ppat_construct
            ({txt= Lident "::"; _}, Some {ppat_desc= Ppat_tuple [p1; p2]; _})
         |Ppat_or (p1, p2) ->
            assert (p1 == pat || p2 == pat)
        | Ppat_alias (p1, _)
         |Ppat_constraint (p1, _)
         |Ppat_construct (_, Some p1)
         |Ppat_exception p1
         |Ppat_lazy p1
         |Ppat_open (_, p1)
         |Ppat_variant (_, Some p1) ->
            assert (p1 == pat)
        | Ppat_extension (_, ext) -> assert (check_extensions ext)
        | Ppat_any | Ppat_constant _
         |Ppat_construct (_, None)
         |Ppat_interval _ | Ppat_type _ | Ppat_unpack _ | Ppat_var _
         |Ppat_variant (_, None) ->
            assert false )
    | Exp ctx -> (
      match ctx.pexp_desc with
      | Pexp_apply _ | Pexp_array _ | Pexp_assert _ | Pexp_coerce _
       |Pexp_constant _ | Pexp_constraint _ | Pexp_construct _
       |Pexp_field _ | Pexp_ident _ | Pexp_ifthenelse _ | Pexp_lazy _
       |Pexp_letexception _ | Pexp_letmodule _ | Pexp_new _
       |Pexp_newtype _ | Pexp_open _ | Pexp_override _ | Pexp_pack _
       |Pexp_poly _ | Pexp_record _ | Pexp_send _ | Pexp_sequence _
       |Pexp_setfield _ | Pexp_setinstvar _ | Pexp_tuple _
       |Pexp_unreachable | Pexp_variant _ | Pexp_while _ ->
          assert false
      | Pexp_extension (_, ext) -> assert (check_extensions ext)
      | Pexp_object {pcstr_self; pcstr_fields} ->
          assert (pcstr_self == pat || check_pcstr_fields pcstr_fields)
      | Pexp_let (_, bindings, _) -> assert (check_bindings bindings)
      | Pexp_letop {let_; ands; _} ->
          let f {pbop_pat; _} = check_subpat pbop_pat in
          assert (f let_ || List.exists ~f ands)
      | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases) ->
          assert (
            List.exists cases ~f:(function
              | {pc_lhs; _} when pc_lhs == pat -> true
              | _ -> false ) )
      | Pexp_for (p, _, _, _, _) | Pexp_fun (_, _, p, _) -> assert (p == pat)
      )
    | Vb ctx -> assert (ctx.pvb_pat == pat)
    | Cl ctx ->
        assert (
          match ctx.pcl_desc with
          | Pcl_fun (_, _, p, _) -> p == pat
          | Pcl_constr _ -> false
          | Pcl_structure {pcstr_self; pcstr_fields} ->
              pcstr_self == pat || check_pcstr_fields pcstr_fields
          | Pcl_apply _ -> false
          | Pcl_let (_, l, _) -> check_bindings l
          | Pcl_constraint _ -> false
          | Pcl_extension (_, ext) -> check_extensions ext
          | Pcl_open _ -> false )
    | Cty _ -> assert false
    | Mty _ | Mod _ | Sig _ -> assert false
    | Str str -> (
      match str.pstr_desc with
      | Pstr_value (_, bindings) -> assert (check_bindings bindings)
      | Pstr_extension ((_, ext), _) -> assert (check_extensions ext)
      | _ -> assert false )
    | Top | Tli _ -> assert false

  let assert_check_pat xpat =
    let dump {ctx; ast= pat} = dump ctx (Pat pat) in
    assert_no_raise ~f:check_pat ~dump xpat

  let check_exp {ctx; ast= exp} =
    let check_extensions = function
      | PPat (_, Some e) -> e == exp
      | PStr [{pstr_desc= Pstr_eval (e, _); _}] -> e == exp
      | _ -> false
    in
    let check_pcstr_fields pcstr_fields =
      List.exists pcstr_fields ~f:(fun {pcf_desc; _} ->
          match pcf_desc with
          | Pcf_initializer e -> e == exp
          | Pcf_val (_, _, Cfk_concrete (_, e)) ->
              let rec loop x =
                x == exp
                ||
                match x with
                | {pexp_desc= Pexp_constraint (e, _); _} -> loop e
                | _ -> false
              in
              loop e
          | Pcf_val (_, _, Cfk_virtual _) -> false
          | Pcf_method
              (_, _, Cfk_concrete (_, {pexp_desc= Pexp_poly (e, _); _}))
           |Pcf_method (_, _, Cfk_concrete (_, e)) ->
              let rec loop x =
                x == exp
                ||
                match x with
                | {pexp_desc= Pexp_newtype (_, e); _} -> loop e
                | {pexp_desc= Pexp_constraint (e, _); _} -> loop e
                | {pexp_desc= Pexp_fun (_, _, _, e); _} -> loop e
                | _ -> false
              in
              loop e
          | Pcf_method (_, _, Cfk_virtual _) -> false
          | Pcf_extension (_, ext) -> check_extensions ext
          | Pcf_inherit _ -> false
          | Pcf_constraint _ -> false
          | Pcf_attribute _ -> false )
    in
    match ctx with
    | Pld (PPat (_, Some e1)) -> assert (e1 == exp)
    | Pld _ -> assert false
    | Exp ctx -> (
        let f eI = eI == exp in
        let snd_f (_, eI) = eI == exp in
        match ctx.pexp_desc with
        | Pexp_construct
            ({txt= Lident "::"; _}, Some {pexp_desc= Pexp_tuple [e1; e2]; _})
          ->
            assert (e1 == exp || e2 == exp)
        | Pexp_extension (_, ext) -> assert (check_extensions ext)
        | Pexp_constant _ | Pexp_ident _ | Pexp_new _ | Pexp_pack _
         |Pexp_unreachable ->
            assert false
        | Pexp_object {pcstr_fields; _} ->
            assert (check_pcstr_fields pcstr_fields)
        | Pexp_let (_, bindings, e) ->
            assert (
              List.exists bindings ~f:(fun {pvb_expr; _} -> pvb_expr == exp)
              || e == exp )
        | Pexp_letop {let_; ands; body} ->
            let f {pbop_exp; _} = pbop_exp == exp in
            assert (f let_ || List.exists ~f ands || body == exp)
        | (Pexp_match (e, _) | Pexp_try (e, _)) when e == exp -> ()
        | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases)
          ->
            assert (
              List.exists cases ~f:(function
                | {pc_guard= Some g; _} when g == exp -> true
                | {pc_rhs; _} when pc_rhs == exp -> true
                | _ -> false ) )
        | Pexp_fun (_, default, _, body) ->
            assert (Option.value_map default ~default:false ~f || body == exp)
        | Pexp_apply (e0, args)
          when Option.is_some (Indexing_op.get_sugar e0 args) ->
            let op = Option.value_exn (Indexing_op.get_sugar e0 args) in
            let in_args =
              match op.op with
              | Defined (e2, _) -> e2 == exp
              | Extended (args, _) | Special (args, _) -> List.exists args ~f
            in
            let in_rhs = Option.value_map ~default:false ~f op.rhs in
            assert (e0 == exp || op.lhs == exp || in_args || in_rhs)
        | Pexp_apply (e0, e1N) ->
            assert (e0 == exp || List.exists e1N ~f:snd_f)
        | Pexp_tuple e1N | Pexp_array e1N -> assert (List.exists e1N ~f)
        | Pexp_construct (_, e) | Pexp_variant (_, e) ->
            assert (Option.exists e ~f)
        | Pexp_record (e1N, e0) ->
            assert (
              Option.exists e0 ~f
              || List.exists e1N ~f:(fun (_, e) ->
                     match e with
                     | { pexp_desc= Pexp_constraint (e, _)
                       ; pexp_attributes= []
                       ; _ }
                       when e == exp ->
                         true
                     | _ -> e == e ) )
        | Pexp_assert e
         |Pexp_constraint (e, _)
         |Pexp_coerce (e, _, _)
         |Pexp_field (e, _)
         |Pexp_lazy e
         |Pexp_letexception (_, e)
         |Pexp_letmodule (_, _, e)
         |Pexp_newtype (_, e)
         |Pexp_open (_, e)
         |Pexp_poly (e, _)
         |Pexp_send (e, _)
         |Pexp_setinstvar (_, e) ->
            assert (e == exp)
        | Pexp_sequence (e1, e2) -> assert (e1 == exp || e2 == exp)
        | Pexp_setfield (e1, _, e2) | Pexp_while (e1, e2) ->
            assert (e1 == exp || e2 == exp)
        | Pexp_ifthenelse (e1, e2, e3) ->
            assert (e1 == exp || e2 == exp || Option.exists e3 ~f)
        | Pexp_for (_, e1, e2, _, e3) ->
            assert (e1 == exp || e2 == exp || e3 == exp)
        | Pexp_override e1N -> assert (List.exists e1N ~f:snd_f) )
    | Vb vb -> assert (vb.pvb_expr == exp)
    | Str str -> (
      match str.pstr_desc with
      | Pstr_eval (e0, _) -> assert (e0 == exp)
      | Pstr_value (_, bindings) ->
          assert (
            List.exists bindings ~f:(fun {pvb_expr; _} -> pvb_expr == exp) )
      | Pstr_extension ((_, ext), _) -> assert (check_extensions ext)
      | Pstr_primitive _ | Pstr_type _ | Pstr_typext _ | Pstr_exception _
       |Pstr_module _ | Pstr_recmodule _ | Pstr_modtype _ | Pstr_open _
       |Pstr_class _ | Pstr_class_type _ | Pstr_include _ | Pstr_attribute _
        ->
          assert false )
    | Mod {pmod_desc= Pmod_unpack e1; _} -> (
      match e1 with
      | { pexp_desc=
            Pexp_constraint
              (e, {ptyp_desc= Ptyp_package _; ptyp_attributes= []; _})
        ; pexp_attributes= []
        ; _ } ->
          assert (e == exp)
      | e -> assert (e == exp) )
    | Cl ctx ->
        let rec loop ctx =
          match ctx.pcl_desc with
          | Pcl_fun (_, eopt, _, e) ->
              Option.exists eopt ~f:(fun e -> e == exp) || loop e
          | Pcl_constr _ -> false
          | Pcl_structure {pcstr_fields; _} ->
              check_pcstr_fields pcstr_fields
          | Pcl_apply (_, l) -> List.exists l ~f:(fun (_, e) -> e == exp)
          | Pcl_let (_, l, _) ->
              List.exists l ~f:(fun {pvb_expr; _} -> pvb_expr == exp)
          | Pcl_constraint _ -> false
          | Pcl_extension _ -> false
          | Pcl_open _ -> false
        in
        assert (loop ctx)
    | Cty _ -> assert false
    | Mod _ | Top | Tli _ | Typ _ | Pat _ | Mty _ | Sig _ -> assert false

  let assert_check_exp xexp =
    let dump {ctx; ast= exp} = dump ctx (Exp exp) in
    assert_no_raise ~f:check_exp ~dump xexp

  let rec is_simple (c : Conf.t) width ({ast= exp; _} as xexp) =
    let ctx = Exp exp in
    match exp.pexp_desc with
    | Pexp_constant _ -> Exp.is_trivial c exp
    | Pexp_field _ | Pexp_ident _ | Pexp_send _
     |Pexp_construct (_, None)
     |Pexp_variant (_, None) ->
        true
    | Pexp_construct
        ({txt= Lident "::"; _}, Some {pexp_desc= Pexp_tuple [e1; e2]; _}) ->
        is_simple c width (sub_exp ~ctx e1)
        && is_simple c width (sub_exp ~ctx e2)
        && fit_margin c (width xexp)
    | Pexp_construct (_, Some e0) | Pexp_variant (_, Some e0) ->
        Exp.is_trivial c e0
    | Pexp_array e1N | Pexp_tuple e1N ->
        List.for_all e1N ~f:(Exp.is_trivial c) && fit_margin c (width xexp)
    | Pexp_record (e1N, e0) ->
        Option.for_all e0 ~f:(Exp.is_trivial c)
        && List.for_all e1N ~f:(snd >> Exp.is_trivial c)
        && fit_margin c (width xexp)
    | Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident ":="; _}; _}, _) ->
        false
    | Pexp_apply (e0, e1N) ->
        Exp.is_trivial c e0
        && List.for_all e1N ~f:(snd >> Exp.is_trivial c)
        && fit_margin c (width xexp)
    | Pexp_extension (_, PStr [{pstr_desc= Pstr_eval (e0, []); _}]) ->
        is_simple c width (sub_exp ~ctx e0)
    | Pexp_extension (_, (PStr [] | PTyp _)) -> true
    | _ -> false

  (** [prec_ctx {ctx; ast}] is the precedence of the context of [ast] within
      [ctx], where [ast] is an immediate sub-term (modulo syntactic sugar) of
      [ctx]. Also returns whether [ast] is the left, right, or neither child
      of [ctx]. Meaningful for binary operators, otherwise returns [None]. *)
  let prec_ctx ctx =
    let open Prec in
    let open Assoc in
    let is_tuple_lvl1_in_constructor ty = function
      | {ptype_kind= Ptype_variant cd1N; _} ->
          List.exists cd1N ~f:(function
            | {pcd_args= Pcstr_tuple t1N; _} ->
                List.exists t1N ~f:(phys_equal ty)
            | _ -> false )
      | _ -> false
    in
    let is_tuple_lvl1_in_ext_constructor ty = function
      | {pext_kind= Pext_decl (Pcstr_tuple t1N, _); _} ->
          List.exists t1N ~f:(phys_equal ty)
      | _ -> false
    in
    let constructor_cxt_prec_of_inner = function
      | {ptyp_desc= Ptyp_arrow _; _} -> Some (Apply, Non)
      | {ptyp_desc= Ptyp_tuple _; _} -> Some (InfixOp3, Non)
      | _ -> None
    in
    match ctx with
    | { ctx=
          ( Str {pstr_desc= Pstr_type (_, t1N); _}
          | Sig {psig_desc= Psig_type (_, t1N); _}
          | Sig {psig_desc= Psig_typesubst t1N; _} )
      ; ast= Typ ({ptyp_desc= Ptyp_arrow _ | Ptyp_tuple _; _} as typ) }
      when List.exists t1N ~f:(is_tuple_lvl1_in_constructor typ) ->
        constructor_cxt_prec_of_inner typ
    | { ctx=
          ( Str {pstr_desc= Pstr_typext {ptyext_constructors= l; _}; _}
          | Sig {psig_desc= Psig_typext {ptyext_constructors= l; _}; _} )
      ; ast= Typ ({ptyp_desc= Ptyp_arrow _ | Ptyp_tuple _; _} as typ)
      ; _ }
      when List.exists l ~f:(is_tuple_lvl1_in_ext_constructor typ) ->
        constructor_cxt_prec_of_inner typ
    | { ctx=
          ( Str {pstr_desc= Pstr_exception {ptyexn_constructor= constr; _}; _}
          | Sig {psig_desc= Psig_exception {ptyexn_constructor= constr; _}; _}
          | Exp {pexp_desc= Pexp_letexception (constr, _); _} )
      ; ast= Typ ({ptyp_desc= Ptyp_tuple _ | Ptyp_arrow _; _} as typ) }
      when is_tuple_lvl1_in_ext_constructor typ constr ->
        constructor_cxt_prec_of_inner typ
    | {ctx= Str _; ast= Typ _; _} -> None
    | {ctx= Typ {ptyp_desc; _}; ast= Typ typ; _} -> (
      match ptyp_desc with
      | Ptyp_arrow (_, t1, _) ->
          Some (MinusGreater, if t1 == typ then Left else Right)
      | Ptyp_tuple _ -> Some (InfixOp3, Non)
      | Ptyp_alias _ -> Some (As, Non)
      | Ptyp_constr (_, _ :: _ :: _) -> Some (Comma, Non)
      | Ptyp_constr _ -> Some (Apply, Non)
      | Ptyp_any | Ptyp_var _ | Ptyp_object _ | Ptyp_class _
       |Ptyp_variant _ | Ptyp_poly _ | Ptyp_package _ | Ptyp_extension _ ->
          None )
    | {ctx= Cty {pcty_desc; _}; ast= Typ typ; _} -> (
      match pcty_desc with
      | Pcty_constr (_, _ :: _ :: _) -> Some (Comma, Non)
      | Pcty_arrow (_, t1, _) ->
          Some (MinusGreater, if t1 == typ then Left else Right)
      | _ -> None )
    | {ctx= Cty {pcty_desc; _}; ast= Cty typ; _} -> (
      match pcty_desc with
      | Pcty_arrow (_, _, t2) ->
          Some (MinusGreater, if t2 == typ then Right else Left)
      | _ -> None )
    | {ast= Cty _; _} -> None
    | {ast= Typ _; _} -> None
    | {ctx= Exp {pexp_desc; _}; ast= Exp exp} -> (
      match pexp_desc with
      | Pexp_tuple (e0 :: _) ->
          Some (Comma, if exp == e0 then Left else Right)
      | Pexp_construct
          ({txt= Lident "::"; _}, Some {pexp_desc= Pexp_tuple [_; e2]; _}) ->
          if Exp.is_sugared_list e2 then Some (Semi, Non)
          else Some (ColonColon, if exp == e2 then Right else Left)
      | Pexp_array _ -> Some (Semi, Non)
      | Pexp_construct (_, Some _)
       |Pexp_assert _ | Pexp_lazy _
       |Pexp_variant (_, Some _) ->
          Some (Apply, Non)
      | Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident i; loc}; _}, [_])
        -> (
        match i with
        | "~-" | "~-." | "~+" | "~+." ->
            if
              loc.loc_end.pos_cnum - loc.loc_start.pos_cnum
              = String.length i - 1
            then Some (UMinus, Non)
            else Some (High, Non)
        | _ -> (
          match i.[0] with
          | '!' | '?' | '~' -> Some (High, Non)
          | _ -> Some (Apply, Non) ) )
      | Pexp_apply (e0, args)
        when Option.is_some (Indexing_op.get_sugar e0 args) -> (
          let op = Option.value_exn (Indexing_op.get_sugar e0 args) in
          if op.lhs == exp then Some (Dot, Left)
          else
            match op.rhs with
            | Some e when e == exp -> Some (LessMinus, Right)
            | _ -> Some (Low, Left) )
      | Pexp_apply
          ({pexp_desc= Pexp_ident {txt= Lident i; _}; _}, [(_, e1); _]) -> (
          let child = if e1 == exp then Left else Right in
          match (i.[0], i) with
          | _, ":=" -> Some (ColonEqual, child)
          | _, ("or" | "||") -> Some (BarBar, child)
          | _, ("&" | "&&") -> Some (AmperAmper, child)
          | ('=' | '<' | '>' | '|' | '&' | '$'), _ | _, "!=" ->
              Some (InfixOp0, child)
          | ('@' | '^'), _ -> Some (InfixOp1, child)
          | ('+' | '-'), _ -> Some (InfixOp2, child)
          | '*', _ when String.(i <> "*") && Char.(i.[1] = '*') ->
              Some (InfixOp4, child)
          | ('*' | '/' | '%'), _ | _, ("lor" | "lxor" | "mod" | "land") ->
              Some (InfixOp3, child)
          | _, ("lsl" | "lsr" | "asr") -> Some (InfixOp4, child)
          | '#', _ -> Some (HashOp, child)
          | _ -> Some (Apply, if String_id.is_infix i then child else Non) )
      | Pexp_apply _ -> Some (Apply, Non)
      | Pexp_setfield (e0, _, _) when e0 == exp -> Some (Dot, Left)
      | Pexp_setfield (_, _, e0) when e0 == exp -> Some (LessMinus, Non)
      | Pexp_setinstvar _ -> Some (LessMinus, Non)
      | Pexp_field _ -> Some (Dot, Left)
      (* We use [Dot] so [x#y] has the same precedence as [x.y], it is
         different to what is done in the parser, but it is intended. *)
      | Pexp_send _ -> Some (Dot, Left)
      | _ -> None )
    | {ctx= Cl {pcl_desc; _}; ast= Cl _ | Exp _} -> (
      match pcl_desc with Pcl_apply _ -> Some (Apply, Non) | _ -> None )
    | { ctx= Exp _
      ; ast=
          Pld _ | Top | Tli _ | Pat _ | Cl _ | Mty _ | Mod _ | Sig _ | Str _
      }
     |{ctx= Vb _; ast= _}
     |{ctx= _; ast= Vb _}
     |{ ctx= Cl _
      ; ast= Pld _ | Top | Tli _ | Pat _ | Mty _ | Mod _ | Sig _ | Str _ }
     |{ ctx=
          ( Pld _ | Top | Tli _ | Typ _ | Cty _ | Pat _ | Mty _ | Mod _
          | Sig _ | Str _ )
      ; ast=
          ( Pld _ | Top | Tli _ | Pat _ | Exp _ | Cl _ | Mty _ | Mod _
          | Sig _ | Str _ ) } ->
        None

  (** [prec_ast ast] is the precedence of [ast]. Meaningful for binary
      operators, otherwise returns [None]. *)
  let rec prec_ast =
    let open Prec in
    function
    | Pld _ -> None
    | Typ {ptyp_desc; _} -> (
      match ptyp_desc with
      | Ptyp_package _ -> Some Low
      | Ptyp_arrow _ -> Some MinusGreater
      | Ptyp_tuple _ -> Some InfixOp3
      | Ptyp_alias _ -> Some As
      | Ptyp_any | Ptyp_var _ | Ptyp_constr _ | Ptyp_object _
       |Ptyp_class _ | Ptyp_variant _ | Ptyp_poly _ | Ptyp_extension _ ->
          None )
    | Cty {pcty_desc; _} -> (
      match pcty_desc with Pcty_arrow _ -> Some MinusGreater | _ -> None )
    | Exp {pexp_desc; _} -> (
      match pexp_desc with
      | Pexp_tuple _ -> Some Comma
      | Pexp_construct
          ({txt= Lident "::"; _}, Some {pexp_desc= Pexp_tuple _; _}) ->
          Some ColonColon
      | Pexp_construct (_, Some _) -> Some Apply
      | Pexp_constant (Pconst_integer (i, _) | Pconst_float (i, _)) -> (
        match i.[0] with '-' | '+' -> Some UMinus | _ -> Some Atomic )
      | Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident i; loc; _}; _}, [_])
        -> (
        match i with
        | "~-" | "~-." | "~+." | "~+" ->
            if
              loc.loc_end.pos_cnum - loc.loc_start.pos_cnum
              = String.length i - 1
            then Some UMinus
            else Some High
        | "!=" -> Some Apply
        | _ -> (
          match i.[0] with '!' | '?' | '~' -> Some High | _ -> Some Apply ) )
      | Pexp_apply (e0, args)
        when Option.is_some (Indexing_op.get_sugar e0 args) -> (
          let op = Option.value_exn (Indexing_op.get_sugar e0 args) in
          match op.rhs with Some _ -> Some LessMinus | _ -> Some Dot )
      | Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident i; _}; _}, [_; _])
        -> (
        match (i.[0], i) with
        | _, ":=" -> Some ColonEqual
        | _, ("or" | "||") -> Some BarBar
        | _, ("&" | "&&") -> Some AmperAmper
        | ('=' | '<' | '>' | '|' | '&' | '$'), _ | _, "!=" -> Some InfixOp0
        | ('@' | '^'), _ -> Some InfixOp1
        | ('+' | '-'), _ -> Some InfixOp2
        | '*', _ when String.(i <> "*") && Char.(i.[1] = '*') ->
            Some InfixOp4
        | ('*' | '/' | '%'), _ | _, ("lor" | "lxor" | "mod" | "land") ->
            Some InfixOp3
        | _, ("lsl" | "lsr" | "asr") -> Some InfixOp4
        | '#', _ -> Some HashOp
        | _ -> Some Apply )
      | Pexp_apply _ -> Some Apply
      | Pexp_assert _ | Pexp_lazy _ | Pexp_for _
       |Pexp_variant (_, Some _)
       |Pexp_while _ | Pexp_new _ | Pexp_object _ ->
          Some Apply
      | Pexp_extension (ext, PStr [{pstr_desc= Pstr_eval (e, _); _}])
        when Source.extension_using_sugar ~name:ext ~payload:e.pexp_loc ->
          prec_ast (Exp e)
      | Pexp_setfield _ -> Some LessMinus
      | Pexp_setinstvar _ -> Some LessMinus
      | Pexp_field _ -> Some Dot
      | Pexp_send _ -> Some Dot
      | _ -> None )
    | Vb _ -> None
    | Cl c -> (
      match c.pcl_desc with
      | Pcl_apply _ -> Some Apply
      | Pcl_structure _ -> Some Apply
      | _ -> None )
    | Top | Pat _ | Mty _ | Mod _ | Sig _ | Str _ | Tli _ -> None

  (** [ambig_prec {ctx; ast}] holds when [ast] is ambiguous in its context
      [ctx], indicating that [ast] should be parenthesized. Meaningful for
      binary operators, otherwise returns [None] if [ctx] has no precedence
      or [Some None] if [ctx] does but [ast] does not. *)
  let ambig_prec ({ast; _} as xast) =
    match prec_ctx xast with
    | Some (prec_ctx, which_child) -> (
      match prec_ast ast with
      | Some prec_ast ->
          let ambiguous =
            match Prec.compare prec_ctx prec_ast with
            | 0 ->
                (* which child and associativity match: no parens *)
                (* which child and assoc conflict: add parens *)
                Assoc.equal which_child Non
                || not (Assoc.equal (Assoc.of_prec prec_ast) which_child)
            (* add parens only when the context has a higher prec than ast *)
            | cmp -> cmp >= 0
          in
          if ambiguous then `Ambiguous else `Non_ambiguous
      | None -> `No_prec_ast )
    | None -> `No_prec_ctx

  (** [parenze_typ {ctx; ast}] holds when type [ast] should be parenthesized
      in context [ctx]. *)
  let parenze_typ ({ctx; ast= typ} as xtyp) =
    assert_check_typ xtyp ;
    match xtyp with
    | {ast= {ptyp_desc= Ptyp_package _; _}; _} -> true
    | {ast= {ptyp_desc= Ptyp_alias _; _}; ctx= Typ _} -> true
    | { ast= {ptyp_desc= Ptyp_alias _; _}
      ; ctx=
          ( Str {pstr_desc= Pstr_typext _; _}
          | Sig {psig_desc= Psig_typext _; _} ) } ->
        true
    | { ast= {ptyp_desc= Ptyp_alias _; _}
      ; ctx=
          ( Str {pstr_desc= Pstr_type (_, t); _}
          | Sig {psig_desc= Psig_type (_, t); _}
          | Sig {psig_desc= Psig_typesubst t; _} ) }
      when List.exists t ~f:(fun t ->
               match t.ptype_kind with
               | Ptype_variant l ->
                   List.exists l ~f:(fun c ->
                       match c.pcd_args with
                       | Pcstr_tuple l -> List.exists l ~f:(phys_equal typ)
                       | _ -> false )
               | _ -> false ) ->
        true
    | { ast= {ptyp_desc= Ptyp_alias _; _}
      ; ctx=
          ( Str
              { pstr_desc=
                  Pstr_exception
                    { ptyexn_constructor=
                        {pext_kind= Pext_decl (Pcstr_tuple t, _); _}
                    ; _ }
              ; _ }
          | Sig
              { psig_desc=
                  Psig_exception
                    { ptyexn_constructor=
                        {pext_kind= Pext_decl (Pcstr_tuple t, _); _}
                    ; _ }
              ; _ } ) }
      when List.exists t ~f:(phys_equal typ) ->
        true
    | _ -> (
      match ambig_prec (sub_ast ~ctx (Typ typ)) with
      | `Ambiguous -> true
      | _ -> false )

  (** [parenze_cty {ctx; ast}] holds when class type [ast] should be
      parenthesized in context [ctx]. *)
  let parenze_cty ({ctx; ast= cty} as xcty) =
    assert_check_cty xcty ;
    match ambig_prec (sub_ast ~ctx (Cty cty)) with
    | `Ambiguous -> true
    | _ -> false

  (** [parenze_mty {ctx; ast}] holds when module type [ast] should be
      parenthesized in context [ctx]. *)
  let parenze_mty {ctx; ast= mty} =
    Mty.has_trailing_attributes mty
    ||
    match (ctx, mty.pmty_desc) with
    | Str {pstr_desc= Pstr_recmodule _; _}, Pmty_with _ -> true
    | Sig {psig_desc= Psig_recmodule _; _}, Pmty_with _ -> true
    | _ -> false

  (** [parenze_mod {ctx; ast}] holds when module expr [ast] should be
      parenthesized in context [ctx]. *)
  let parenze_mod {ctx; ast= m} =
    Mod.has_trailing_attributes m
    ||
    match (ctx, m.pmod_desc) with
    | Mod {pmod_desc= Pmod_apply _; _}, Pmod_functor _ -> true
    | _ -> false

  (** [parenze_pat {ctx; ast}] holds when pattern [ast] should be
      parenthesized in context [ctx]. *)
  let parenze_pat ({ctx; ast= pat} as xpat) =
    assert_check_pat xpat ;
    Pat.has_trailing_attributes pat
    ||
    match (ctx, pat.ppat_desc) with
    | ( Pat
          { ppat_desc=
              Ppat_construct
                ( {txt= Lident "::"; _}
                , Some {ppat_desc= Ppat_tuple [_; tl]; _} )
          ; _ }
      , Ppat_construct ({txt= Lident "::"; _}, _) )
      when tl == pat ->
        false
    | ( Pat
          { ppat_desc=
              Ppat_construct
                ( {txt= Lident "::"; _}
                , Some {ppat_desc= Ppat_tuple [_; _]; _} )
          ; _ }
      , inner ) -> (
      match inner with
      | Ppat_construct ({txt= Lident "::"; _}, _) -> true
      | Ppat_construct _ | Ppat_record _ | Ppat_variant _ -> false
      | _ -> true )
    | ( Pat {ppat_desc= Ppat_construct _; _}
      , Ppat_construct ({txt= Lident "::"; _}, _) ) ->
        true
    | ( ( Exp {pexp_desc= Pexp_let _ | Pexp_letop _; _}
        | Str {pstr_desc= Pstr_value _; _} )
      , ( Ppat_construct (_, Some _)
        | Ppat_variant (_, Some _)
        | Ppat_or _ | Ppat_alias _ ) ) ->
        true
    | ( ( Exp {pexp_desc= Pexp_let _ | Pexp_letop _; _}
        | Str {pstr_desc= Pstr_value _; _} )
      , Ppat_constraint (_, {ptyp_desc= Ptyp_poly _; _}) ) ->
        false
    | ( ( Exp {pexp_desc= Pexp_let _ | Pexp_letop _; _}
        | Str {pstr_desc= Pstr_value _; _} )
      , Ppat_constraint ({ppat_desc= Ppat_any; _}, _) ) ->
        true
    | ( ( Exp {pexp_desc= Pexp_let _ | Pexp_letop _; _}
        | Str {pstr_desc= Pstr_value _; _} )
      , Ppat_constraint ({ppat_desc= Ppat_tuple _; _}, _) ) ->
        false
    | ( ( Exp {pexp_desc= Pexp_let _ | Pexp_letop _; _}
        | Str {pstr_desc= Pstr_value _; _} )
      , Ppat_constraint _ ) ->
        true
    | Pat _, Ppat_constraint _
     |_, Ppat_unpack _
     |_, Ppat_constraint ({ppat_desc= Ppat_unpack _; _}, _)
     |( Pat
          { ppat_desc=
              ( Ppat_alias _ | Ppat_array _ | Ppat_constraint _
              | Ppat_construct _ | Ppat_variant _ )
          ; _ }
      , Ppat_tuple _ )
     |( ( Pat
            { ppat_desc=
                ( Ppat_construct _ | Ppat_exception _ | Ppat_or _
                | Ppat_lazy _ | Ppat_tuple _ | Ppat_variant _ )
            ; _ }
        | Exp {pexp_desc= Pexp_fun _; _} )
      , Ppat_alias _ )
     |( Pat {ppat_desc= Ppat_lazy _; _}
      , (Ppat_construct _ | Ppat_variant (_, Some _) | Ppat_or _) )
     |( Pat
          { ppat_desc=
              ( Ppat_construct _ | Ppat_exception _ | Ppat_tuple _
              | Ppat_variant _ )
          ; _ }
      , Ppat_or _ )
     |Pat {ppat_desc= Ppat_lazy _; _}, Ppat_tuple _
     |Pat {ppat_desc= Ppat_tuple _; _}, Ppat_tuple _
     |Pat _, Ppat_lazy _
     |Pat _, Ppat_exception _
     |Exp {pexp_desc= Pexp_fun _; _}, Ppat_or _
     |Cl {pcl_desc= Pcl_fun _; _}, Ppat_constraint _
     |Cl {pcl_desc= Pcl_fun _; _}, Ppat_tuple _
     |Cl {pcl_desc= Pcl_fun _; _}, Ppat_construct _
     |Cl {pcl_desc= Pcl_fun _; _}, Ppat_alias _
     |Cl {pcl_desc= Pcl_fun _; _}, Ppat_lazy _
     |( Exp
          { pexp_desc=
              Pexp_fun _ | Pexp_function _ | Pexp_match _ | Pexp_try _
          ; _ }
      , Ppat_constraint _ )
     |Exp {pexp_desc= Pexp_let _ | Pexp_letop _; _}, Ppat_exception _
     |( Exp {pexp_desc= Pexp_fun _; _}
      , (Ppat_construct _ | Ppat_lazy _ | Ppat_tuple _ | Ppat_variant _) ) ->
        true
    | (Str _ | Exp _), Ppat_lazy _ -> true
    | ( Pat {ppat_desc= Ppat_construct _ | Ppat_variant _; _}
      , (Ppat_construct (_, Some _) | Ppat_variant (_, Some _)) ) ->
        true
    | ( ( Exp {pexp_desc= Pexp_let (_, bindings, _); _}
        | Str {pstr_desc= Pstr_value (_, bindings); _} )
      , _ ) ->
        List.exists bindings ~f:(function
          | {pvb_pat; pvb_expr= {pexp_desc= Pexp_constraint _; _}; _} ->
              pvb_pat == pat
          | _ -> false )
    | Pld _, Ppat_constraint _ -> true
    | _ -> false

  (** Check if an exp is a prefix op that is not fully applied *)
  let is_displaced_prefix_op {ctx; ast= exp} =
    match (ctx, exp.pexp_desc) with
    | ( Exp {pexp_desc= Pexp_apply (e0, [(Nolabel, _)]); _}
      , Pexp_ident {txt= i; _} )
      when e0 == exp && Longident.is_prefix i ->
        false
    | _, Pexp_ident {txt= i; _} when Longident.is_prefix i -> true
    | _ -> false

  (** Check if an exp is an infix op that is not fully applied *)
  let is_displaced_infix_op {ctx; ast= exp} =
    match (ctx, exp.pexp_desc) with
    | ( Exp {pexp_desc= Pexp_apply (e0, [(Nolabel, _); (Nolabel, _)]); _}
      , Pexp_ident {txt= i; _} )
      when e0 == exp && Longident.is_infix i
           && List.is_empty exp.pexp_attributes ->
        false
    | _, Pexp_ident {txt= i; _} when Longident.is_infix i ->
        List.is_empty exp.pexp_attributes
    | _ -> false

  let marked_parenzed_inner_nested_match =
    let memo = Hashtbl.Poly.create () in
    register_reset (fun () -> Hashtbl.clear memo) ;
    memo

  (** [exposed cls exp] holds if there is a right-most subexpression of [exp]
      which satisfies [Exp.mem_cls cls] and is not parenthesized. *)
  let rec exposed_right_exp =
    (* exponential without memoization *)
    let memo = Hashtbl.Poly.create () in
    register_reset (fun () -> Hashtbl.clear memo) ;
    fun cls exp ->
      let exposed_ () =
        let continue subexp =
          (not (parenze_exp (sub_exp ~ctx:(Exp exp) subexp)))
          && exposed_right_exp cls subexp
        in
        match exp.pexp_desc with
        | Pexp_assert e
         |Pexp_construct
            ({txt= Lident "::"; _}, Some {pexp_desc= Pexp_tuple [_; e]; _})
         |Pexp_construct (_, Some e)
         |Pexp_fun (_, _, _, e)
         |Pexp_ifthenelse (_, e, None)
         |Pexp_ifthenelse (_, _, Some e)
         |Pexp_lazy e
         |Pexp_newtype (_, e)
         |Pexp_open (_, e)
         |Pexp_sequence (_, e)
         |Pexp_setfield (_, _, e)
         |Pexp_setinstvar (_, e)
         |Pexp_variant (_, Some e) ->
            continue e
        | Pexp_extension
            ( ext
            , PStr
                [ { pstr_desc= Pstr_eval (({pexp_attributes= []; _} as e), _)
                  ; _ } ] )
          when Source.extension_using_sugar ~name:ext ~payload:e.pexp_loc ->
            continue e
        | Pexp_let (_, _, e)
         |Pexp_letop {body= e; _}
         |Pexp_letexception (_, e)
         |Pexp_letmodule (_, _, e) -> (
          match cls with Match | Then | ThenElse -> continue e | _ -> false )
        | Pexp_match _ when match cls with Then -> true | _ -> false ->
            false
        | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases)
          ->
            continue (List.last_exn cases).pc_rhs
        | Pexp_apply (e0, args)
          when Option.is_some (Indexing_op.get_sugar e0 args) ->
            false
        | Pexp_apply (_, args) -> continue (snd (List.last_exn args))
        | Pexp_tuple es -> continue (List.last_exn es)
        | Pexp_array _ | Pexp_coerce _ | Pexp_constant _ | Pexp_constraint _
         |Pexp_construct (_, None)
         |Pexp_extension _ | Pexp_field _ | Pexp_for _ | Pexp_ident _
         |Pexp_new _ | Pexp_object _ | Pexp_override _ | Pexp_pack _
         |Pexp_poly _ | Pexp_record _ | Pexp_send _ | Pexp_unreachable
         |Pexp_variant (_, None)
         |Pexp_while _ ->
            false
      in
      Exp.mem_cls cls exp
      || Hashtbl.find_or_add memo (cls, exp) ~default:exposed_

  and exposed_right_cl =
    let memo = Hashtbl.Poly.create () in
    register_reset (fun () -> Hashtbl.clear memo) ;
    fun cls cl ->
      let exposed_ () =
        match cl.pcl_desc with
        | Pcl_apply (_, args) ->
            let exp = snd (List.last_exn args) in
            (not (parenze_exp (sub_exp ~ctx:(Cl cl) exp)))
            && exposed_right_exp cls exp
        | Pcl_fun (_, _, _, e) ->
            (not (parenze_cl (sub_cl ~ctx:(Cl cl) e)))
            && exposed_right_cl cls e
        | _ -> false
      in
      Cl.mem_cls cls cl
      || Hashtbl.find_or_add memo (cls, cl) ~default:exposed_

  and mark_parenzed_inner_nested_match exp =
    let exposed_ () =
      let continue subexp =
        if not (parenze_exp (sub_exp ~ctx:(Exp exp) subexp)) then
          mark_parenzed_inner_nested_match subexp ;
        false
      in
      match exp.pexp_desc with
      | Pexp_assert e
       |Pexp_construct
          ({txt= Lident "::"; _}, Some {pexp_desc= Pexp_tuple [_; e]; _})
       |Pexp_construct (_, Some e)
       |Pexp_ifthenelse (_, e, None)
       |Pexp_ifthenelse (_, _, Some e)
       |Pexp_lazy e
       |Pexp_newtype (_, e)
       |Pexp_open (_, e)
       |Pexp_fun (_, _, _, e)
       |Pexp_sequence (_, e)
       |Pexp_setfield (_, _, e)
       |Pexp_setinstvar (_, e)
       |Pexp_variant (_, Some e) ->
          continue e
      | Pexp_let (_, _, e)
       |Pexp_letop {body= e; _}
       |Pexp_letexception (_, e)
       |Pexp_letmodule (_, _, e) ->
          continue e
      | Pexp_extension (ext, PStr [{pstr_desc= Pstr_eval (e, _); _}])
        when Source.extension_using_sugar ~name:ext ~payload:e.pexp_loc -> (
        match e.pexp_desc with
        | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases)
          ->
            List.iter cases ~f:(fun case ->
                mark_parenzed_inner_nested_match case.pc_rhs ) ;
            true
        | _ -> continue e )
      | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases) ->
          List.iter cases ~f:(fun case ->
              mark_parenzed_inner_nested_match case.pc_rhs ) ;
          true
      | Pexp_apply (e0, args)
        when Option.is_some (Indexing_op.get_sugar e0 args) -> (
        match Option.value_exn (Indexing_op.get_sugar e0 args) with
        | {rhs= Some e; _} -> continue e
        | {rhs= None; _} -> false )
      | Pexp_apply (_, args) -> continue (snd (List.last_exn args))
      | Pexp_tuple es -> continue (List.last_exn es)
      | Pexp_array _ | Pexp_coerce _ | Pexp_constant _ | Pexp_constraint _
       |Pexp_construct (_, None)
       |Pexp_extension _ | Pexp_field _ | Pexp_for _ | Pexp_ident _
       |Pexp_new _ | Pexp_object _ | Pexp_override _ | Pexp_pack _
       |Pexp_poly _ | Pexp_record _ | Pexp_send _ | Pexp_unreachable
       |Pexp_variant (_, None)
       |Pexp_while _ ->
          false
    in
    Hashtbl.find_or_add marked_parenzed_inner_nested_match exp
      ~default:exposed_
    |> (ignore : bool -> _)

  (** [parenze_exp {ctx; ast}] holds when expression [ast] should be
      parenthesized in context [ctx]. *)
  and parenze_exp ({ctx; ast= exp} as xexp) =
    let parenze () =
      let is_right_infix_arg ctx_desc exp =
        match ctx_desc with
        | Pexp_apply
            ({pexp_desc= Pexp_ident {txt= i; _}; _}, _ :: (_, e2) :: _)
          when e2 == exp && Longident.is_infix i
               && Option.value_map ~default:false (prec_ast ctx) ~f:(fun p ->
                      Prec.compare p Apply < 0 ) ->
            true
        | Pexp_apply (e0, (_ :: (_, e2) :: _ as args))
          when e2 == exp && Option.is_some (Indexing_op.get_sugar e0 args) ->
            true
        | Pexp_tuple e1N -> List.last_exn e1N == xexp.ast
        | _ -> false
      in
      match ambig_prec (sub_ast ~ctx (Exp exp)) with
      | `No_prec_ctx -> false (* ctx not apply *)
      | `Ambiguous -> true (* exp is apply and ambig *)
      | _ -> (
        match ctx with
        | Exp {pexp_desc; _} ->
            if is_right_infix_arg pexp_desc exp then Exp.is_sequence exp
            else exposed_right_exp Non_apply exp
        | _ -> exposed_right_exp Non_apply exp )
    in
    let rec ifthenelse pexp_desc =
      match pexp_desc with
      | Pexp_extension (ext, PStr [{pstr_desc= Pstr_eval (e, _); _}])
        when Source.extension_using_sugar ~name:ext ~payload:e.pexp_loc ->
          ifthenelse e.pexp_desc
      | Pexp_let _ | Pexp_match _ | Pexp_try _ -> true
      | _ -> false
    in
    let exp_in_sequence lhs rhs exp =
      match (lhs.pexp_desc, exp.pexp_attributes) with
      | (Pexp_match _ | Pexp_try _), _ :: _ when lhs == exp -> true
      | _, _ :: _ -> false
      | ( Pexp_extension
            ( _
            , PStr
                [ { pstr_desc= Pstr_eval ({pexp_desc= Pexp_sequence _; _}, [])
                  ; _ } ] )
        , _ )
        when lhs == exp ->
          true
      | _ when lhs == exp -> exposed_right_exp Let_match exp
      | _ when rhs == exp -> false
      | _ -> failwith "exp must be lhs or rhs from the parent expression"
    in
    assert_check_exp xexp ;
    is_displaced_prefix_op xexp
    || is_displaced_infix_op xexp
    || Hashtbl.find marked_parenzed_inner_nested_match exp
       |> Option.value ~default:false
    ||
    match (ctx, exp) with
    | Str {pstr_desc= Pstr_eval _; _}, _ -> false
    | ( _
      , { pexp_desc=
            Pexp_apply ({pexp_desc= Pexp_ident {txt= id; _}; _}, _ :: _)
        ; pexp_attributes= _ :: _
        ; _ } )
      when Longident.is_infix id ->
        true
    | ( Str
          { pstr_desc=
              Pstr_value
                (Nonrecursive, [{pvb_pat= {ppat_desc= Ppat_any; _}; _}])
          ; _ }
      , _ ) ->
        false
    (* Object fields do not require parens, even with trailing attributes *)
    | Exp {pexp_desc= Pexp_object _; _}, _ -> false
    | ( Exp {pexp_desc= Pexp_construct ({txt= id; _}, _); _}
      , {pexp_attributes= _ :: _; _} )
      when Longident.is_infix id ->
        true
    | Exp {pexp_desc= Pexp_extension _; _}, {pexp_desc= Pexp_tuple _; _} ->
        false
    | Pld _, {pexp_desc= Pexp_tuple _; _} -> false
    | Cl {pcl_desc= Pcl_apply _; _}, _ -> parenze ()
    | Exp {pexp_desc= Pexp_ifthenelse (_, e, _); _}, {pexp_desc; _}
      when !parens_ite && e == exp && ifthenelse pexp_desc ->
        true
    | Exp {pexp_desc= Pexp_ifthenelse (_, _, Some e); _}, {pexp_desc; _}
      when !parens_ite && e == exp && ifthenelse pexp_desc ->
        true
    | ( Exp
          {pexp_desc= Pexp_apply (op, (Nolabel, _) :: (Nolabel, e1) :: _); _}
      , { pexp_desc=
            Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident "not"; _}; _}, _)
        ; _ } )
      when Exp.is_infix op && not (e1 == exp) ->
        true
    | ( Exp {pexp_desc= Pexp_apply (e, _); _}
      , {pexp_desc= Pexp_construct _ | Pexp_variant _; _} )
      when e == exp ->
        true
    (* Integers without suffixes must be parenthesised on the lhs of an
       indexing operator *)
    | ( Exp {pexp_desc= Pexp_apply (op, (Nolabel, left) :: _); _}
      , {pexp_desc= Pexp_constant (Pconst_integer (_, None)); _} )
      when exp == left && Exp.is_index_op op ->
        true
    | Exp {pexp_desc= Pexp_field (e, _); _}, {pexp_desc= Pexp_construct _; _}
      when e == exp ->
        true
    | Exp {pexp_desc; _}, _ -> (
      match pexp_desc with
      | Pexp_extension
          ( _
          , PStr
              [ { pstr_desc=
                    Pstr_eval
                      ( { pexp_desc=
                            ( Pexp_function cases
                            | Pexp_match (_, cases)
                            | Pexp_try (_, cases) )
                        ; _ }
                      , _ )
                ; _ } ] )
       |Pexp_function cases
       |Pexp_match (_, cases)
       |Pexp_try (_, cases) ->
          if !leading_nested_match_parens then
            List.iter cases ~f:(fun {pc_rhs; _} ->
                mark_parenzed_inner_nested_match pc_rhs ) ;
          List.exists cases ~f:(fun {pc_rhs; _} -> pc_rhs == exp)
          && exposed_right_exp Match exp
      | Pexp_ifthenelse (cnd, _, _) when cnd == exp -> false
      | Pexp_ifthenelse (_, thn, None) when thn == exp ->
          exposed_right_exp Then exp
      | Pexp_ifthenelse (_, thn, Some _) when thn == exp ->
          exposed_right_exp ThenElse exp
      | Pexp_ifthenelse (_, _, Some els) when els == exp ->
          Exp.is_sequence exp
      | Pexp_apply (({pexp_desc= Pexp_new _; _} as exp2), _) when exp2 == exp
        ->
          false
      | Pexp_apply
          ( ( { pexp_desc=
                  Pexp_extension
                    ( _
                    , PStr
                        [ { pstr_desc=
                              Pstr_eval ({pexp_desc= Pexp_new _; _}, [])
                          ; _ } ] )
              ; _ } as exp2 )
          , _ )
        when exp2 == exp ->
          false
      | Pexp_record (flds, _)
        when List.exists flds ~f:(fun (_, e0) ->
                 match e0 with
                 | {pexp_desc= Pexp_constraint (e, _); pexp_attributes= []; _}
                   when e == exp ->
                     true
                 | _ -> e0 == exp ) ->
          exposed_right_exp Non_apply exp
          (* Non_apply is perhaps pessimistic *)
      | Pexp_record (_, Some ({pexp_desc= Pexp_apply (ident, [_]); _} as e0))
        when e0 == exp && Exp.is_prefix ident ->
          (* don't put parens around [!e] in [{ !e with a; b }] *)
          false
      | Pexp_record
          ( _
          , Some
              ( { pexp_desc=
                    ( Pexp_ident _ | Pexp_constant _ | Pexp_record _
                    | Pexp_field _ )
                ; _ } as e0 ) )
        when e0 == exp ->
          false
      | Pexp_record (_, Some e0) when e0 == exp -> true
      | Pexp_sequence (lhs, rhs) -> exp_in_sequence lhs rhs exp
      | _ -> Exp.has_trailing_attributes exp || parenze () )
    | _, exp when Exp.has_trailing_attributes exp -> true
    | _ -> false

  (** [parenze_cl {ctx; ast}] holds when class expr [ast] should be
      parenthesized in context [ctx]. *)
  and parenze_cl ({ctx; ast= cl} as xcl) =
    assert_check_cl xcl ;
    match ambig_prec (sub_ast ~ctx (Cl cl)) with
    | `No_prec_ctx -> false
    | `Ambiguous -> true
    | _ -> exposed_right_cl Non_apply cl

  let parenze_nested_exp {ctx; ast= exp} =
    let infix_prec ast =
      match ast with
      | Exp {pexp_desc= Pexp_apply (e, _); _} when Exp.is_infix e ->
          prec_ast ast
      | Exp
          ( { pexp_desc=
                Pexp_construct
                  ( {txt= Lident "::"; loc= _}
                  , Some {pexp_desc= Pexp_tuple [_; _]; _} )
            ; _ } as exp )
        when not (Exp.is_sugared_list exp) ->
          prec_ast ast
      | _ -> None
    in
    (* Make the precedence explicit for infix operators *)
    match (infix_prec ctx, infix_prec (Exp exp)) with
    | Some (InfixOp0 | ColonEqual), _ | _, Some (InfixOp0 | ColonEqual) ->
        (* special case for refs update and all InfixOp0 to reduce parens
           noise *)
        false
    | None, _ | _, None -> false
    | Some p1, Some p2 -> not (Prec.equal p1 p2)
end

include In_ctx
include Requires_sub_terms
