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
open Parsetree
include Non_overlapping_interval_tree.Make (Location)

let fold src =
  object
    inherit [Location.t list] Ppxlib.Ast_traverse.fold as super

    method! location loc locs = loc :: locs

    method! pattern p locs =
      let extra_loc =
        match p.ppat_desc with
        | Ppat_record (flds, Open) ->
            Option.to_list (Source.loc_of_underscore src flds p.ppat_loc)
        | _ -> []
      in
      super#pattern p (extra_loc @ locs)

    method! attribute attr locs =
      (* ignore location of docstrings *)
      if Ast.Attr.is_doc attr then locs else super#attribute attr locs

    (** Ast_traverse recurses down to locations in stacks *)
    method! location_stack _ l = l

    method! expression e locs =
      let extra_loc =
        match e.pexp_desc with
        | Pexp_constant _ -> [Source.loc_of_expr_constant src e]
        | _ -> []
      in
      super#expression e (extra_loc @ locs)
  end

(** Use Ast_mapper to collect all locs in ast, and create tree of them. *)
let of_ast fragment ast src =
  let locs = Traverse.fold fragment (fold src) ast [] in
  (of_list locs, locs)
