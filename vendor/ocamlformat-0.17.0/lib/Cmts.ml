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

(** Placing and formatting comments in a parsetree. *)

module Format = Format_
open Migrate_ast
open Parsetree

type t =
  { debug: bool
  ; mutable cmts_before: Cmt.t Multimap.M(Location).t
  ; mutable cmts_after: Cmt.t Multimap.M(Location).t
  ; mutable cmts_within: Cmt.t Multimap.M(Location).t
  ; source: Source.t
  ; mutable remaining: Set.M(Location).t
  ; remove: bool }

let update_remaining t ~f = t.remaining <- f t.remaining

let update_cmts t pos ~f =
  match pos with
  | `After -> t.cmts_after <- f t.cmts_after
  | `Before -> t.cmts_before <- f t.cmts_before
  | `Within -> t.cmts_within <- f t.cmts_within

let find_at_position t loc pos =
  let map =
    match pos with
    | `After -> t.cmts_after
    | `Before -> t.cmts_before
    | `Within -> t.cmts_within
  in
  Map.find map loc

(** Heuristic to determine if two locations should be considered "adjacent".
    Holds if there is only whitespace between the locations, or if there is a
    [|] character and the first location begins a line and the start column
    of the first location is lower than that of the second location. *)
let is_adjacent src (l1 : Location.t) (l2 : Location.t) =
  match
    Source.tokens_between src l1.loc_end l2.loc_start ~filter:(function
        | _ -> true )
  with
  | [] -> true
  | [(Token_latest.BAR, _)] ->
      Source.begins_line src l1
      && Position.column l1.loc_start < Position.column l2.loc_start
  | _ -> false

(** Whether the symbol preceding location [loc] is an infix symbol
    (corresponding to [Ast.String_id.is_infix]) or a semicolon. If it is the
    case, comments attached to the following item should be kept after the
    infix symbol. *)
let infix_symbol_before src (loc : Location.t) =
  match
    Source.find_token_before src ~filter:(function _ -> true) loc.loc_start
  with
  | Some
      ( ( SEMI | INFIXOP0 _ | INFIXOP1 _ | INFIXOP2 _ | INFIXOP3 _
        | INFIXOP4 _ | COLONCOLON | COLONEQUAL | BARBAR | LESSMINUS | EQUAL
        | COLON | LETOP _ | ANDOP _ | DOTDOT | DOTOP _ | AMPERAMPER )
      , _ ) ->
      true
  | _ -> false

(** Sets of comments supporting splitting by locations. *)
module CmtSet : sig
  type t

  val of_list : Cmt.t list -> t

  val to_list : t -> Cmt.t list
  (** ordered by start location *)

  val is_empty : t -> bool

  val split : t -> Location.t -> t * t * t
  (** [split s {loc_start; loc_end}] splits [s] into the subset of comments
      that end before [loc_start], those that start after [loc_end], and
      those within the loc. *)

  val partition :
    Source.t -> prev:Location.t -> next:Location.t -> t -> t * t
  (** Heuristic to choose between placing a comment after the previous
      location or before the next one. *)
end = struct
  type t = Cmt.t list Map.M(Position).t

  let empty = Map.empty (module Position)

  let is_empty = Map.is_empty

  let of_list cmts =
    List.fold cmts ~init:empty ~f:(fun map cmt ->
        let pos = cmt.Cmt.loc.loc_start in
        Map.add_multi map ~key:pos ~data:cmt )

  let to_list map = List.concat (Map.data map)

  (** Assuming loc_start <= loc_end, the locs are split in 5 sets:

      - a: before start
      - b: at start
      - c: after start, before end
      - d: at end
      - e: after end *)
  let split t {Location.loc_start; loc_end; _} =
    let add_opt kvo init =
      Option.fold kvo ~init ~f:(fun m (key, data) -> Map.set m ~key ~data)
    in
    let ( ++ ) = add_opt in
    let a_b_c, d, e = Map.split t loc_end in
    let a, b, c = Map.split a_b_c loc_start in
    (a, b ++ c, d ++ e)

  let partition src ~prev ~next cmts =
    match to_list cmts with
    | Cmt.{loc; _} :: _ as cmtl when is_adjacent src prev loc -> (
      match
        List.group cmtl ~break:(fun l1 l2 ->
            not (is_adjacent src (Cmt.loc l1) (Cmt.loc l2)) )
      with
      | [cmtl] when is_adjacent src (List.last_exn cmtl).loc next ->
          let open Location in
          let same_line_as_prev l =
            prev.loc_end.pos_lnum = l.loc_start.pos_lnum
          in
          let decide loc =
            match
              ( loc.loc_start.pos_lnum - prev.loc_end.pos_lnum
              , next.loc_start.pos_lnum - loc.loc_end.pos_lnum )
            with
            | 0, 0 -> `Before_next
            | 0, _ when infix_symbol_before src loc -> `Before_next
            | 0, _ -> `After_prev
            | 1, x when x > 1 && Source.empty_line_after src loc ->
                `After_prev
            | _ -> `Before_next
          in
          let prev, next =
            if not (same_line_as_prev next) then
              let next, prev =
                List.partition_tf cmtl ~f:(fun {Cmt.loc= l; _} ->
                    match decide l with
                    | `After_prev -> false
                    | `Before_next -> true )
              in
              (prev, next)
            else ([], cmtl)
          in
          (of_list prev, of_list next)
      | after :: befores -> (of_list after, of_list (List.concat befores))
      | [] -> impossible "by parent match" )
    | _ -> (empty, cmts)
end

let position_to_string = function
  | `Before -> "before"
  | `After -> "after"
  | `Within -> "within"

let add_cmts t ?prev ?next position loc cmts =
  if not (CmtSet.is_empty cmts) then (
    let cmtl = CmtSet.to_list cmts in
    if t.debug then
      List.iter cmtl ~f:(fun {Cmt.txt= cmt_txt; loc= cmt_loc} ->
          let string_between (l1 : Location.t) (l2 : Location.t) =
            match Source.string_between t.source l1.loc_end l2.loc_start with
            | None -> "swapped"
            | Some s -> s
          in
          let btw_prev =
            Option.value_map prev ~default:"no prev"
              ~f:(Fn.flip string_between cmt_loc)
          in
          let btw_next =
            Option.value_map next ~default:"no next"
              ~f:(string_between cmt_loc)
          in
          Caml.Format.eprintf "add %s %a: %a \"%s\" %s \"%s\"@\n%!"
            (position_to_string position)
            Location.fmt loc Location.fmt cmt_loc (String.escaped btw_prev)
            cmt_txt (String.escaped btw_next) ) ;
    update_cmts t position ~f:(Map.add_exn ~key:loc ~data:cmtl) )

(** Traverse the location tree from locs, find the deepest location that
    contains each comment, intersperse comments between that location's
    children. *)
let rec place t loc_tree ?prev_loc locs cmts =
  match locs with
  | curr_loc :: next_locs ->
      let before, within, after = CmtSet.split cmts curr_loc in
      let before_curr =
        match prev_loc with
        | None -> before
        (* Location.none is a special case, it shouldn't be the location of a
           previous element, so if a comment is at the beginning of the file,
           it should be placed before the next element. *)
        | Some prev_loc when Location.(compare prev_loc none) = 0 -> before
        | Some prev_loc ->
            let after_prev, before_curr =
              CmtSet.partition t.source ~prev:prev_loc ~next:curr_loc before
            in
            add_cmts t `After ~prev:prev_loc ~next:curr_loc prev_loc
              after_prev ;
            before_curr
      in
      add_cmts t `Before ?prev:prev_loc ~next:curr_loc curr_loc before_curr ;
      ( match Loc_tree.children loc_tree curr_loc with
      | [] ->
          add_cmts t `Within ?prev:prev_loc ~next:curr_loc curr_loc within
      | children -> place t loc_tree children within ) ;
      place t loc_tree ~prev_loc:curr_loc next_locs after
  | [] -> (
    match prev_loc with
    | Some prev_loc -> add_cmts t `After ~prev:prev_loc prev_loc cmts
    | None ->
        if t.debug then
          List.iter (CmtSet.to_list cmts) ~f:(fun {Cmt.txt; _} ->
              Format.eprintf "lost: %s@\n%!" txt ) )

(** Relocate comments, for Ast transformations such as sugaring. *)
let relocate (t : t) ~src ~before ~after =
  if t.remove then (
    if t.debug then
      Caml.Format.eprintf "relocate %a to %a and %a@\n%!" Location.fmt src
        Location.fmt before Location.fmt after ;
    let merge_and_sort x y =
      List.rev_append x y
      |> List.sort
           ~compare:(Comparable.lift Location.compare_start ~f:Cmt.loc)
    in
    update_cmts t `Before
      ~f:(Multimap.update_multi ~src ~dst:before ~f:merge_and_sort) ;
    update_cmts t `After
      ~f:(Multimap.update_multi ~src ~dst:after ~f:merge_and_sort) ;
    update_cmts t `Within
      ~f:(Multimap.update_multi ~src ~dst:after ~f:merge_and_sort) ;
    if t.debug then
      update_remaining t ~f:(fun s ->
          let s = Set.remove s src in
          let s = Set.add s after in
          Set.add s before ) )

let relocate_cmts_before (t : t) ~src ~sep ~dst =
  let f map =
    Multimap.partition_multi map ~src ~dst ~f:(fun Cmt.{loc; _} ->
        Location.compare_end loc sep < 0 )
  in
  update_cmts t `Before ~f ;
  update_cmts t `Within ~f

let relocate_pattern_matching_cmts (t : t) src tok ~whole_loc ~matched_loc =
  let kwd_loc =
    Option.value_exn (Source.loc_of_first_token_at src whole_loc tok)
  in
  relocate_cmts_before t ~src:matched_loc ~sep:kwd_loc ~dst:whole_loc

let relocate_ext_cmts (t : t) src ((_pre : string Location.loc), pld)
    ~whole_loc =
  match pld with
  | PStr
      [ { pstr_desc=
            Pstr_eval
              ( { pexp_desc= Pexp_constant (Pconst_string _)
                ; pexp_loc= _
                ; pexp_loc_stack= _
                ; pexp_attributes= _ }
              , [] )
        ; pstr_loc } ]
    when Source.is_quoted_string src pstr_loc ->
      ()
  | PStr [{pstr_desc= Pstr_eval _; pstr_loc; _}] ->
      let kwd_loc =
        match Source.loc_of_first_token_at src whole_loc LBRACKETPERCENT with
        | Some loc -> loc
        | None -> (
          match Source.loc_of_first_token_at src whole_loc PERCENT with
          | Some loc -> loc
          | None -> impossible "expect token starting extension" )
      in
      relocate_cmts_before t ~src:pstr_loc ~sep:kwd_loc ~dst:whole_loc
  | _ -> ()

let relocate_wrongfully_attached_cmts t src exp =
  match exp.pexp_desc with
  | Pexp_match (e0, _) ->
      relocate_pattern_matching_cmts t src Parser.MATCH
        ~whole_loc:exp.pexp_loc ~matched_loc:e0.pexp_loc
  | Pexp_try (e0, _) ->
      relocate_pattern_matching_cmts t src Parser.TRY ~whole_loc:exp.pexp_loc
        ~matched_loc:e0.pexp_loc
  | Pexp_extension ext -> relocate_ext_cmts t src ext ~whole_loc:exp.pexp_loc
  | _ -> ()

(** Initialize global state and place comments. *)
let init fragment ~debug source asts comments_n_docstrings =
  let t =
    { debug
    ; cmts_before= Map.empty (module Location)
    ; cmts_after= Map.empty (module Location)
    ; cmts_within= Map.empty (module Location)
    ; source
    ; remaining= Set.empty (module Location)
    ; remove= true }
  in
  let comments = Normalize.dedup_cmts fragment asts comments_n_docstrings in
  if debug then (
    Format.eprintf "\nComments:\n%!" ;
    List.iter comments ~f:(fun {Cmt.txt; loc} ->
        Caml.Format.eprintf "%a %s %s@\n%!" Location.fmt loc txt
          (if Source.ends_line source loc then "eol" else "") ) ) ;
  if not (List.is_empty comments) then (
    let loc_tree, locs = Loc_tree.of_ast fragment asts source in
    if debug then
      List.iter locs ~f:(fun loc ->
          if not (Location.compare loc Location.none = 0) then
            update_remaining t ~f:(fun s -> Set.add s loc) ) ;
    if debug then (
      let dump fs lt = Fmt.eval fs (Loc_tree.dump lt) in
      Format.eprintf "\nLoc_tree:\n%!" ;
      Format.eprintf "@\n%a@\n@\n%!" dump loc_tree ) ;
    let locs = Loc_tree.roots loc_tree in
    let cmts = CmtSet.of_list comments in
    match locs with
    | [] -> add_cmts t `After ~prev:Location.none Location.none cmts
    | _ -> place t loc_tree locs cmts ) ;
  let relocate_loc_stack loc stack =
    List.iter stack ~f:(fun src -> relocate t ~src ~before:loc ~after:loc)
  in
  let iter =
    object
      inherit Ppxlib.Ast_traverse.iter as super

      method! pattern x =
        relocate_loc_stack x.ppat_loc x.ppat_loc_stack ;
        super#pattern x

      method! core_type x =
        relocate_loc_stack x.ptyp_loc x.ptyp_loc_stack ;
        super#core_type x

      method! expression x =
        relocate_loc_stack x.pexp_loc x.pexp_loc_stack ;
        super#expression x
    end
  in
  Traverse.iter fragment iter asts ;
  t

let preserve fmt_x t =
  let buf = Buffer.create 128 in
  let fs = Format.formatter_of_buffer buf in
  Fmt.eval fs (fmt_x {t with remove= false}) ;
  Format.pp_print_flush fs () ;
  Buffer.contents buf

let pop_if_debug t loc =
  if t.debug && t.remove then
    update_remaining t ~f:(fun s -> Set.remove s loc)

let find_cmts t pos loc =
  pop_if_debug t loc ;
  let r = find_at_position t loc pos in
  if t.remove then update_cmts t pos ~f:(fun m -> Map.remove m loc) ;
  r

let break_comment_group source margin {Cmt.loc= a; _} {Cmt.loc= b; _} =
  let vertical_align =
    Location.line_difference a b = 1 && Location.compare_start_col a b = 0
  in
  let horizontal_align =
    Location.line_difference a b = 0
    && List.is_empty
         (Source.tokens_between source a.loc_end b.loc_start
              ~filter:(function _ -> true) )
  in
  not
    ( (Location.is_single_line a margin && Location.is_single_line b margin)
    && (vertical_align || horizontal_align) )

let fmt_cmts_aux t (conf : Conf.t) cmts ~fmt_code pos =
  let open Fmt in
  let groups =
    List.group cmts ~break:(break_comment_group t.source conf.margin)
  in
  vbox 0
    (list_pn groups (fun ~prev:_ group ~next ->
         ( match group with
         | [] -> impossible "previous match"
         | [cmt] ->
             Cmt.fmt cmt t.source ~wrap:conf.wrap_comments
               ~ocp_indent_compat:conf.ocp_indent_compat
               ~fmt_code:(fmt_code conf) pos
         | group ->
             list group "@;<1000 0>" (fun cmt ->
                 wrap "(*" "*)" (str (Cmt.txt cmt)) ) )
         $
         match next with
         | Some ({loc= next; _} :: _) ->
             let Cmt.{loc= last; _} = List.last_exn group in
             fmt_if (Location.line_difference last next > 1) "\n" $ fmt "@ "
         | _ -> noop ) )

(** Format comments for loc. *)
let fmt_cmts t conf ~fmt_code ?pro ?epi ?(eol = Fmt.fmt "@\n") ?(adj = eol)
    found loc pos =
  let open Fmt in
  match found with
  | None | Some [] -> noop
  | Some cmts ->
      let epi =
        let ({loc= last_loc; _} : Cmt.t) = List.last_exn cmts in
        let eol_cmt = Source.ends_line t.source last_loc in
        let adj_cmt = eol_cmt && Location.line_difference last_loc loc = 1 in
        fmt_or_k eol_cmt (fmt_or_k adj_cmt adj eol) (fmt_opt epi)
      in
      fmt_opt pro $ fmt_cmts_aux t conf cmts ~fmt_code pos $ epi

let fmt_before t conf ~fmt_code ?pro ?(epi = Fmt.break 1 0) ?eol ?adj loc =
  fmt_cmts t conf
    (find_cmts t `Before loc)
    ~fmt_code ?pro ~epi ?eol ?adj loc Cmt.Before

let fmt_after t conf ~fmt_code ?(pro = Fmt.break 1 0) ?epi loc =
  let open Fmt in
  let within =
    fmt_cmts t conf
      (find_cmts t `Within loc)
      ~fmt_code ~pro ?epi loc Cmt.Within
  in
  let after =
    fmt_cmts t conf
      (find_cmts t `After loc)
      ~fmt_code ~pro ?epi ~eol:noop loc Cmt.After
  in
  within $ after

let fmt_within t conf ~fmt_code ?(pro = Fmt.break 1 0) ?(epi = Fmt.break 1 0)
    loc =
  fmt_cmts t conf
    (find_cmts t `Within loc)
    ~fmt_code ~pro ~epi ~eol:Fmt.noop loc Cmt.Within

module Toplevel = struct
  let fmt_cmts t conf ~fmt_code found (pos : Cmt.pos) =
    let open Fmt in
    match found with
    | None | Some [] -> noop
    | Some (({loc= first_loc; _} : Cmt.t) :: _ as cmts) ->
        let pro =
          match pos with
          | Before -> noop
          | Within | After ->
              if Source.begins_line t.source first_loc then
                fmt_or
                  (Source.empty_line_before t.source first_loc)
                  "\n@;<1000 0>" "@\n"
              else break 1 0
        in
        let epi =
          let ({loc= last_loc; _} : Cmt.t) = List.last_exn cmts in
          match pos with
          | Before | Within ->
              if Source.ends_line t.source last_loc then
                fmt_or
                  (Source.empty_line_after t.source last_loc)
                  "\n@;<1000 0>" "@\n"
              else break 1 0
          | After -> noop
        in
        pro $ fmt_cmts_aux t conf cmts ~fmt_code pos $ epi

  let fmt_before t conf ~fmt_code loc =
    fmt_cmts t conf (find_cmts t `Before loc) ~fmt_code Cmt.Before

  let fmt_after t conf ~fmt_code loc =
    let open Fmt in
    let within =
      fmt_cmts t conf (find_cmts t `Within loc) ~fmt_code Cmt.Within
    in
    let after =
      fmt_cmts t conf (find_cmts t `After loc) ~fmt_code Cmt.After
    in
    within $ after
end

let drop_inside t loc =
  let clear pos =
    update_cmts t pos
      ~f:
        (Multimap.filter ~f:(fun {Cmt.loc= cmt_loc; _} ->
             not (Location.contains loc cmt_loc) ) )
  in
  clear `Before ;
  clear `Within ;
  clear `After

let drop_before t loc =
  update_cmts t `Before ~f:(fun m -> Map.remove m loc) ;
  t

let has_before t loc = pop_if_debug t loc ; Map.mem t.cmts_before loc

let has_within t loc = pop_if_debug t loc ; Map.mem t.cmts_within loc

let has_after t loc =
  pop_if_debug t loc ;
  Map.mem t.cmts_within loc || Map.mem t.cmts_after loc

(** returns comments that have not been formatted *)
let remaining_comments t =
  List.concat_map ~f:Multimap.to_list
    [t.cmts_before; t.cmts_within; t.cmts_after]

let remaining_before t loc = Map.find_multi t.cmts_before loc

let remaining_locs t = Set.to_list t.remaining

let diff (conf : Conf.t) x y =
  let norm z =
    let norm_non_code {Cmt.txt; _} = Normalize.comment txt in
    let f z =
      match Cmt.txt z with
      | "" | "$" -> norm_non_code z
      | str ->
          if Char.equal str.[0] '$' then
            let chars_removed =
              if Char.equal str.[String.length str - 1] '$' then 2 else 1
            in
            let len = String.length str - chars_removed in
            let str = String.sub ~pos:1 ~len str in
            try
              let t = Parse_with_comments.parse Structure conf ~source:str in
              Normalize.normalize Structure conf t.ast
              |> Caml.Format.asprintf "%a" Printast.implementation
            with _ -> norm_non_code z
          else norm_non_code z
    in
    Set.of_list (module String) (List.map ~f z)
  in
  Set.symmetric_diff (norm x) (norm y)
