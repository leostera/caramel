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

open Migrate_ast
open Asttypes
open Parsetree
open Ast

let rec arrow_typ cmts i ({ast= typ; _} as xtyp) =
  let ctx = Typ typ in
  let {ptyp_desc; ptyp_loc; _} = typ in
  match ptyp_desc with
  | Ptyp_arrow (l, t1, t2) ->
      let before = if i > 0 then ptyp_loc else t1.ptyp_loc in
      Cmts.relocate cmts ~src:ptyp_loc ~before ~after:t2.ptyp_loc ;
      let rest =
        match t2.ptyp_attributes with
        | [] -> arrow_typ cmts (i + 1) (sub_typ ~ctx t2)
        | _ -> [(ptyp_loc, Nolabel, sub_typ ~ctx t2)]
      in
      (ptyp_loc, l, sub_typ ~ctx t1) :: rest
  | _ -> [(ptyp_loc, Nolabel, xtyp)]

let arrow_typ cmts t = arrow_typ cmts 0 t

let rec class_arrow_typ cmts ({ast= typ; _} as xtyp) =
  let ctx = Cty typ in
  let {pcty_desc; pcty_loc; _} = typ in
  match pcty_desc with
  | Pcty_arrow (l, t1, t2) ->
      Cmts.relocate cmts ~src:pcty_loc ~before:t1.ptyp_loc ~after:t2.pcty_loc ;
      let rest =
        match t2.pcty_attributes with
        | [] -> class_arrow_typ cmts (sub_cty ~ctx t2)
        | _ -> [(Nolabel, `class_type (sub_cty ~ctx t2))]
      in
      (l, `core_type (sub_typ ~ctx t1)) :: rest
  | _ -> [(Nolabel, `class_type xtyp)]

let rec or_pat ?(allow_attribute = true) cmts ({ast= pat; _} as xpat) =
  let ctx = Pat pat in
  match pat with
  | {ppat_desc= Ppat_or (pat1, pat2); ppat_loc; ppat_attributes= []; _} ->
      Cmts.relocate cmts ~src:ppat_loc ~before:pat1.ppat_loc
        ~after:pat2.ppat_loc ;
      or_pat ~allow_attribute:false cmts (sub_pat ~ctx pat1)
      @ or_pat ~allow_attribute:false cmts (sub_pat ~ctx pat2)
  | {ppat_desc= Ppat_or (pat1, pat2); ppat_loc; _} when allow_attribute ->
      Cmts.relocate cmts ~src:ppat_loc ~before:pat1.ppat_loc
        ~after:pat2.ppat_loc ;
      [sub_pat ~ctx pat1; sub_pat ~ctx pat2]
  | _ -> [xpat]

type arg_kind =
  | Val of arg_label * pattern xt * expression xt option
  | Newtypes of string loc list

let fun_ cmts ?(will_keep_first_ast_node = true) xexp =
  let rec fun_ ?(will_keep_first_ast_node = false) ({ast= exp; _} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc; pexp_attributes; _} = exp in
    if will_keep_first_ast_node || List.is_empty pexp_attributes then
      match pexp_desc with
      | Pexp_fun (label, default, pattern, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pexp_loc ~before:pattern.ppat_loc
              ~after:body.pexp_loc ;
          let xargs, xbody = fun_ (sub_exp ~ctx body) in
          ( Val
              ( label
              , sub_pat ~ctx pattern
              , Option.map default ~f:(sub_exp ~ctx) )
            :: xargs
          , xbody )
      | Pexp_newtype (name, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pexp_loc ~before:body.pexp_loc
              ~after:body.pexp_loc ;
          let xargs, xbody = fun_ (sub_exp ~ctx body) in
          let xargs =
            match xargs with
            | Newtypes names :: xargs -> Newtypes (name :: names) :: xargs
            | xargs -> Newtypes [name] :: xargs
          in
          (xargs, xbody)
      | _ -> ([], xexp)
    else ([], xexp)
  in
  fun_ ~will_keep_first_ast_node xexp

let cl_fun ?(will_keep_first_ast_node = true) cmts xexp =
  let rec fun_ ?(will_keep_first_ast_node = false) ({ast= exp; _} as xexp) =
    let ctx = Cl exp in
    let {pcl_desc; pcl_loc; pcl_attributes; _} = exp in
    if will_keep_first_ast_node || List.is_empty pcl_attributes then
      match pcl_desc with
      | Pcl_fun (label, default, pattern, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pcl_loc ~before:pattern.ppat_loc
              ~after:body.pcl_loc ;
          let xargs, xbody = fun_ (sub_cl ~ctx body) in
          ( Val
              ( label
              , sub_pat ~ctx pattern
              , Option.map default ~f:(sub_exp ~ctx) )
            :: xargs
          , xbody )
      | _ -> ([], xexp)
    else ([], xexp)
  in
  fun_ ~will_keep_first_ast_node xexp

let infix cmts prec xexp =
  let assoc = Option.value_map prec ~default:Assoc.Non ~f:Assoc.of_prec in
  let rec infix_ ?(relocate = true) xop ((lbl, {ast= exp; _}) as xexp) =
    assert (Poly.(lbl = Nolabel)) ;
    let ctx = Exp exp in
    match (assoc, exp) with
    | Left, {pexp_desc= Pexp_apply (e0, [(l1, e1); (l2, e2)]); pexp_loc; _}
      when Option.equal Prec.equal prec (prec_ast (Exp exp)) ->
        let op_args1 = infix_ None (l1, sub_exp ~ctx e1) in
        let src = pexp_loc in
        let after = e2.pexp_loc in
        ( match op_args1 with
        | (Some {ast= {pexp_loc= before; _}; _}, _) :: _
         |(None, (_, {ast= {pexp_loc= before; _}; _}) :: _) :: _ ->
            if relocate then Cmts.relocate cmts ~src ~before ~after
        | _ ->
            if relocate then
              Cmts.relocate cmts ~src ~before:e0.pexp_loc ~after ) ;
        op_args1 @ [(Some (sub_exp ~ctx e0), [(l2, sub_exp ~ctx e2)])]
    | Right, {pexp_desc= Pexp_apply (e0, [(l1, e1); (l2, e2)]); pexp_loc; _}
      when Option.equal Prec.equal prec (prec_ast (Exp exp)) ->
        let op_args2 =
          infix_ (Some (sub_exp ~ctx e0)) (l2, sub_exp ~ctx e2)
        in
        let src = pexp_loc in
        let after = e1.pexp_loc in
        let before =
          match xop with
          | Some {ast; _} -> ast.pexp_loc
          | None -> e1.pexp_loc
        in
        let may_relocate ~after =
          if relocate then Cmts.relocate cmts ~src ~before ~after
        in
        ( match List.last op_args2 with
        | Some (_, args2) -> (
          match List.last args2 with
          | Some (_, {ast= {pexp_loc= after; _}; _}) -> may_relocate ~after
          | None -> may_relocate ~after )
        | _ -> may_relocate ~after ) ;
        (xop, [(l1, sub_exp ~ctx e1)]) :: op_args2
    | _ -> [(xop, [xexp])]
  in
  infix_ None ~relocate:false (Nolabel, xexp)

let list_pat cmts pat =
  let rec list_pat_ pat acc =
    let ctx = Pat pat in
    let {ppat_desc; ppat_loc= src; _} = pat in
    match ppat_desc with
    | Ppat_construct ({txt= Lident "[]"; loc}, None) ->
        Cmts.relocate cmts ~src ~before:loc ~after:loc ;
        Some (List.rev acc, loc)
    | Ppat_construct
        ( {txt= Lident "::"; loc}
        , Some
            { ppat_desc= Ppat_tuple [hd; ({ppat_attributes= []; _} as tl)]
            ; ppat_loc
            ; ppat_attributes= []
            ; _ } ) ->
        list_pat_ tl (([src; loc; ppat_loc], sub_pat ~ctx hd) :: acc)
    | _ -> None
  in
  list_pat_ pat []

let list_exp cmts exp =
  let rec list_exp_ exp acc =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc= src; _} = exp in
    match pexp_desc with
    | Pexp_construct ({txt= Lident "[]"; loc}, None) ->
        Cmts.relocate cmts ~src ~before:loc ~after:loc ;
        Some (List.rev acc, loc)
    | Pexp_construct
        ( {txt= Lident "::"; loc}
        , Some
            { pexp_desc= Pexp_tuple [hd; ({pexp_attributes= []; _} as tl)]
            ; pexp_loc
            ; pexp_attributes= []
            ; _ } ) ->
        list_exp_ tl (([src; loc; pexp_loc], sub_exp ~ctx hd) :: acc)
    | _ -> None
  in
  list_exp_ exp []

let rec ite cmts ({ast= exp; _} as xexp) =
  let ctx = Exp exp in
  let {pexp_desc; pexp_loc; pexp_attributes; _} = exp in
  match pexp_desc with
  | Pexp_ifthenelse (cnd, thn, Some els) ->
      Cmts.relocate cmts ~src:pexp_loc ~before:cnd.pexp_loc
        ~after:els.pexp_loc ;
      (Some (sub_exp ~ctx cnd), sub_exp ~ctx thn, pexp_attributes)
      :: ite cmts (sub_exp ~ctx els)
  | Pexp_ifthenelse (cnd, thn, None) ->
      Cmts.relocate cmts ~src:pexp_loc ~before:cnd.pexp_loc
        ~after:thn.pexp_loc ;
      [(Some (sub_exp ~ctx cnd), sub_exp ~ctx thn, pexp_attributes)]
  | _ -> [(None, xexp, pexp_attributes)]

let sequence cmts xexp =
  let rec sequence_ ?(allow_attribute = true) ({ast= exp; _} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc; _} = exp in
    match pexp_desc with
    | Pexp_extension
        ( ext
        , PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( ( { pexp_desc= Pexp_sequence (e1, e2)
                        ; pexp_attributes
                        ; _ } as exp )
                    , _ )
              ; pstr_loc= _ } ] )
      when List.is_empty pexp_attributes
           && Source.extension_using_sugar ~name:ext ~payload:e1.pexp_loc ->
        let ctx = Exp exp in
        Cmts.relocate cmts ~src:pexp_loc ~before:e1.pexp_loc
          ~after:e2.pexp_loc ;
        if (not allow_attribute) && not (List.is_empty exp.pexp_attributes)
        then [(None, xexp)]
        else if Ast.exposed_right_exp Ast.Let_match e1 then
          [(None, sub_exp ~ctx e1); (Some ext, sub_exp ~ctx e2)]
        else
          let l1 = sequence_ ~allow_attribute:false (sub_exp ~ctx e1) in
          let l2 =
            match sequence_ ~allow_attribute:false (sub_exp ~ctx e2) with
            | [] -> []
            | (_, e2) :: l2 -> (Some ext, e2) :: l2
          in
          List.append l1 l2
    | Pexp_sequence (e1, e2) ->
        Cmts.relocate cmts ~src:pexp_loc ~before:e1.pexp_loc
          ~after:e2.pexp_loc ;
        if (not allow_attribute) && not (List.is_empty exp.pexp_attributes)
        then [(None, xexp)]
        else if Ast.exposed_right_exp Ast.Let_match e1 then
          [(None, sub_exp ~ctx e1); (None, sub_exp ~ctx e2)]
        else
          List.append
            (sequence_ ~allow_attribute:false (sub_exp ~ctx e1))
            (sequence_ ~allow_attribute:false (sub_exp ~ctx e2))
    | _ -> [(None, xexp)]
  in
  sequence_ xexp

type functor_arg = Unit | Named of label option loc * module_type Ast.xt

(* The sugar is different when used with the [functor] keyword. The syntax
   M(A : A)(B : B) cannot handle [_] as module name. *)
let rec functor_type cmts ~for_functor_kw ~source_is_long
    ({ast= mty; _} as xmty) =
  let ctx = Mty mty in
  match mty with
  | {pmty_desc= Pmty_functor (fp, body); pmty_loc; pmty_attributes}
    when for_functor_kw
         || (List.is_empty pmty_attributes && not (source_is_long mty)) ->
      let functor_arg =
        match fp with
        | Named (arg, arg_mty) -> Named (arg, sub_mty ~ctx arg_mty)
        | Unit -> Unit
      in
      let body = sub_mty ~ctx body in
      let xargs, xbody =
        match pmty_attributes with
        | [] -> functor_type cmts ~for_functor_kw ~source_is_long body
        | _ -> ([], body)
      in
      (Ppxlib.Loc.make ~loc:pmty_loc functor_arg :: xargs, xbody)
  | _ -> ([], xmty)

(* The sugar is different when used with the [functor] keyword. The syntax
   M(A : A)(B : B) cannot handle [_] as module name. *)
let rec functor_ cmts ~for_functor_kw ~source_is_long ({ast= me; _} as xme) =
  let ctx = Mod me in
  match me with
  | {pmod_desc= Pmod_functor (fp, body); pmod_loc; pmod_attributes}
    when for_functor_kw
         || (List.is_empty pmod_attributes && not (source_is_long me)) ->
      let functor_arg =
        match fp with
        | Named (arg, arg_mt) -> Named (arg, sub_mty ~ctx arg_mt)
        | Unit -> Unit
      in
      let ctx = Mod body in
      let body = sub_mod ~ctx body in
      let xargs, xbody_me =
        match pmod_attributes with
        | [] -> functor_ cmts ~for_functor_kw ~source_is_long body
        | _ -> ([], body)
      in
      (Ppxlib.Loc.make ~loc:pmod_loc functor_arg :: xargs, xbody_me)
  | _ -> ([], xme)

let mod_with pmty =
  let rec mod_with_ ({ast= me; _} as xme) =
    let ctx = Mty me in
    match me with
    | {pmty_desc= Pmty_with (mt, wcs); pmty_attributes; pmty_loc} ->
        let args, rest = mod_with_ (sub_mty ~ctx mt) in
        ((wcs, pmty_loc, pmty_attributes) :: args, rest)
    | _ -> ([], xme)
  in
  let l_rev, m = mod_with_ pmty in
  (List.rev l_rev, m)

let polynewtype cmts pat body =
  let ctx = Pat pat in
  match pat.ppat_desc with
  | Ppat_constraint (pat2, {ptyp_desc= Ptyp_poly (pvars, _); _}) ->
      let pvars0 = pvars in
      let xpat = sub_pat ~ctx pat2 in
      let rec polynewtype_ pvars body =
        let ctx = Exp body in
        match (pvars, body.pexp_desc) with
        | [], Pexp_constraint (exp, typ) ->
            Some (xpat, pvars0, sub_typ ~ctx typ, sub_exp ~ctx exp)
        | ( {txt= pvar; loc= loc1} :: pvars
          , Pexp_newtype ({txt= nvar; loc= loc2}, exp) )
          when String.equal pvar nvar ->
            Cmts.relocate cmts ~src:loc2 ~before:loc1 ~after:loc1 ;
            polynewtype_ pvars exp
        | _ -> None
      in
      Cmts.relocate cmts ~src:pat.ppat_loc ~before:pat2.ppat_loc
        ~after:pat2.ppat_loc ;
      polynewtype_ pvars body
  | _ -> None
