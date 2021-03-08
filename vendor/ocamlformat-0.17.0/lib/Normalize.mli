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

val dedup_cmts : 'a Traverse.fragment -> 'a -> Cmt.t list -> Cmt.t list

val comment : string -> string
(** Normalize a comment. *)

val docstring : Conf.t -> string -> string
(** Normalize a docstring. *)

val normalize : 'a Traverse.fragment -> Conf.t -> 'a -> 'a
(** Normalize an AST fragment. *)

val equal :
     'a Traverse.fragment
  -> ignore_doc_comments:bool
  -> Conf.t
  -> 'a
  -> 'a
  -> bool
(** Compare fragments for equality up to normalization. *)

type docstring_error =
  | Moved of Location.t * Location.t * string
  | Unstable of Location.t * string

val moved_docstrings :
  'a Traverse.fragment -> Conf.t -> 'a -> 'a -> docstring_error list
