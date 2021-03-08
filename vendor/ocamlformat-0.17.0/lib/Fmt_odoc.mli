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

val fmt :
     fmt_code:(string -> (Fmt.t, unit) Result.t)
  -> Odoc_parser.Ast.docs
  -> Fmt.t

val diff :
  Conf.t -> Cmt.t list -> Cmt.t list -> (string, string) Either.t Sequence.t
(** Difference between two lists of doc comments. *)

val is_tag_only : Odoc_parser.Ast.docs -> bool
(** [true] if the documentation only contains tags *)
