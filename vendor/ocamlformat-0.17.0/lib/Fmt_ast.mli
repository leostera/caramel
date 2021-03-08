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

(** Format OCaml Ast *)

val fmt_fragment :
     'a list Migrate_ast.Traverse.fragment
  -> debug:bool
  -> Source.t
  -> Cmts.t
  -> Conf.t
  -> 'a list
  -> Fmt.t
(** Format a fragment. *)
