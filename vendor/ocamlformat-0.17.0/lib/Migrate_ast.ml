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

module Ast_helper = Ppxlib.Ast_helper

module Parsetree = struct
  include Ppxlib.Parsetree

  let equal_core_type : core_type -> core_type -> bool = Poly.equal

  let equal_structure : structure -> structure -> bool = Poly.equal

  let equal_signature : signature -> signature -> bool = Poly.equal

  let equal_toplevel_phrase : toplevel_phrase -> toplevel_phrase -> bool =
    Poly.equal
end

module Asttypes = struct
  include Ppxlib.Asttypes

  let is_private = function Private -> true | Public -> false

  let is_open = function Open -> true | Closed -> false

  let is_override = function Override -> true | Fresh -> false

  let is_mutable = function Mutable -> true | Immutable -> false
end

module Traverse = struct
  type 'a fragment =
    | Structure : Parsetree.structure fragment
    | Signature : Parsetree.signature fragment
    | Use_file : Parsetree.toplevel_phrase list fragment

  let equal (type a) (x : a fragment) : a -> a -> bool =
    match x with
    | Structure -> Parsetree.equal_structure
    | Signature -> Parsetree.equal_signature
    | Use_file -> List.equal Parsetree.equal_toplevel_phrase

  let map (type a) (x : a fragment) (m : Ppxlib.Ast_traverse.map) : a -> a =
    match x with
    | Structure -> m#structure
    | Signature -> m#signature
    | Use_file -> m#list m#toplevel_phrase

  let iter (type a) (fragment : a fragment) (i : Ppxlib.Ast_traverse.iter) :
      a -> unit =
    match fragment with
    | Structure -> i#structure
    | Signature -> i#signature
    | Use_file -> i#list i#toplevel_phrase

  let fold (type a) (fragment : a fragment) (f : _ Ppxlib.Ast_traverse.fold)
      : a -> _ =
    match fragment with
    | Structure -> f#structure
    | Signature -> f#signature
    | Use_file -> f#list f#toplevel_phrase
end

module Parse = struct
  let implementation = Ppxlib_ast.Parse.implementation

  let interface = Ppxlib_ast.Parse.interface

  let use_file lexbuf =
    List.filter (Ppxlib_ast.Parse.use_file lexbuf)
      ~f:(fun (p : Parsetree.toplevel_phrase) ->
        match p with
        | Ptop_def [] -> false
        | Ptop_def (_ :: _) | Ptop_dir _ -> true )

  let fragment (type a) (fragment : a Traverse.fragment) lexbuf : a =
    match fragment with
    | Traverse.Structure -> implementation lexbuf
    | Traverse.Signature -> interface lexbuf
    | Traverse.Use_file -> use_file lexbuf

  let parser_version = Ocaml_version.sys_version
end

module Printast = struct
  let pp_sexp ppf sexp = Format.fprintf ppf "%a" (Sexp.pp_hum_indent 2) sexp

  let sexp_of = Ppxlib.Ast_traverse.sexp_of

  let implementation ppf x = pp_sexp ppf (sexp_of#structure x)

  let interface ppf x = pp_sexp ppf (sexp_of#signature x)

  let expression ppf x = pp_sexp ppf (sexp_of#expression x)

  let payload ppf x = pp_sexp ppf (sexp_of#payload x)

  let use_file ppf x = pp_sexp ppf (List.sexp_of_t sexp_of#toplevel_phrase x)

  let fragment (type a) : a Traverse.fragment -> _ -> a -> _ = function
    | Traverse.Structure -> implementation
    | Traverse.Signature -> interface
    | Traverse.Use_file -> use_file
end

module Pprintast = Ppxlib.Pprintast

module Position = struct
  open Lexing

  type t = position

  let column {pos_bol; pos_cnum; _} = pos_cnum - pos_bol

  let fmt fs {pos_lnum; pos_bol; pos_cnum; pos_fname= _} =
    if pos_lnum = -1 then Format.fprintf fs "[%d]" pos_cnum
    else Format.fprintf fs "[%d,%d+%d]" pos_lnum pos_bol (pos_cnum - pos_bol)

  let to_string x = Format.asprintf "%a" fmt x

  let sexp_of_t x = Sexp.Atom (to_string x)

  let compare_col p1 p2 = Int.compare (column p1) (column p2)

  let compare p1 p2 =
    if phys_equal p1 p2 then 0 else Int.compare p1.pos_cnum p2.pos_cnum

  include (val Comparator.make ~compare ~sexp_of_t)

  let distance p1 p2 = p2.pos_cnum - p1.pos_cnum

  let to_point x = Odoc_model.Location_.{line= x.pos_lnum; column= column x}
end

module Location = struct
  include Ppxlib.Location

  let fmt fs {loc_start; loc_end; loc_ghost} =
    Format.fprintf fs "(%a..%a)%s" Position.fmt loc_start Position.fmt
      loc_end
      (if loc_ghost then " ghost" else "")

  let to_string x = Format.asprintf "%a" fmt x

  let sexp_of_t x = Sexp.Atom (to_string x)

  let compare {loc_start; loc_end; loc_ghost} b =
    match Position.compare loc_start b.loc_start with
    | 0 -> (
      match Position.compare loc_end b.loc_end with
      | 0 -> Bool.compare loc_ghost b.loc_ghost
      | c -> c )
    | c -> c

  type location = t

  module Location_comparator = Comparator.Make (struct
    type t = location

    let sexp_of_t = sexp_of_t

    let compare = compare
  end)

  include Location_comparator

  let compare_start x y = Position.compare x.loc_start y.loc_start

  let compare_start_col x y = Position.compare_col x.loc_start y.loc_start

  let compare_end x y = Position.compare x.loc_end y.loc_end

  let compare_end_col x y = Position.compare_col x.loc_end y.loc_end

  let line_difference fst snd = snd.loc_start.pos_lnum - fst.loc_end.pos_lnum

  let contains l1 l2 = compare_start l1 l2 <= 0 && compare_end l1 l2 >= 0

  let width x = Position.distance x.loc_start x.loc_end

  let descending cmp a b = -cmp a b

  let compare_width_decreasing =
    Comparable.lexicographic [compare_start; descending compare_end; compare]

  let is_single_line x margin =
    (* The last character of a line can exceed the margin if it is not
       preceded by a break. Adding 1 here is a workaround for this bug. *)
    width x <= margin + 1 && x.loc_start.pos_lnum = x.loc_end.pos_lnum

  let smallest loc stack =
    let min a b = if width a < width b then a else b in
    List.reduce_exn (loc :: stack) ~f:min

  let to_span loc =
    let open Odoc_model.Location_ in
    { file= loc.loc_start.pos_fname
    ; start= Position.to_point loc.loc_start
    ; end_= Position.to_point loc.loc_end }
end

module Longident = struct
  type t = Longident.t =
    | Lident of string
    | Ldot of t * string
    | Lapply of t * t

  let flatten = Longident.flatten

  let last = Longident.last

  let lident s =
    assert (not (String.contains s '.')) ;
    Lident s
end

module Parser = Token_latest

module Lexer = struct
  let token_with_comments lexbuf =
    Token_latest.of_compiler_libs (Lexer.token_with_comments lexbuf)

  type error = Lexer.error

  exception Error = Lexer.Error
end
