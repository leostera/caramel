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

(** Placing and formatting comments in a parsetree.

    This module provides an interface to the global mutable data structure
    that maintains the relationship between comments and Ast terms within a
    parsetree.

    Each comment is placed, by one of the [init] functions, either before or
    after a location appearing in the parsetree. The [relocate] function can
    be used to adjust this placement.

    When comments are formatted by one of the [fmt] functions, they are
    removed from the data structure. This is significant in cases where there
    are multiple Ast terms with the same location. *)

module Format = Format_
open Migrate_ast

type t

val init :
  'a Traverse.fragment -> debug:bool -> Source.t -> 'a -> Cmt.t list -> t
(** [init fragment source x comments] associates each comment in [comments]
    with a source location appearing in [x]. It uses [Source] to help resolve
    ambiguities. Initializes the state used by the [fmt] functions. *)

val relocate :
  t -> src:Location.t -> before:Location.t -> after:Location.t -> unit
(** [relocate src before after] moves (changes the association with
    locations) comments before [src] to [before] and comments after [src] to
    [after]. *)

val relocate_wrongfully_attached_cmts :
  t -> Source.t -> Parsetree.expression -> unit
(** [relocate_wrongfully_attached_cmts] relocates wrongfully attached
    comments, e.g. comments that should be attached to the whole
    pattern-matching expressions ([match-with] or [try-with] expressions) but
    are wrongfully attached to the matched expression. *)

val fmt_before :
     t
  -> Conf.t
  -> fmt_code:(Conf.t -> string -> (Fmt.t, unit) Result.t)
  -> ?pro:Fmt.t
  -> ?epi:Fmt.t
  -> ?eol:Fmt.t
  -> ?adj:Fmt.t
  -> Location.t
  -> Fmt.t
(** [fmt_before loc] formats the comments associated with [loc] that appear
    before [loc]. *)

val fmt_after :
     t
  -> Conf.t
  -> fmt_code:(Conf.t -> string -> (Fmt.t, unit) Result.t)
  -> ?pro:Fmt.t
  -> ?epi:Fmt.t
  -> Location.t
  -> Fmt.t
(** [fmt_after loc] formats the comments associated with [loc] that appear
    after [loc]. *)

val fmt_within :
     t
  -> Conf.t
  -> fmt_code:(Conf.t -> string -> (Fmt.t, unit) Result.t)
  -> ?pro:Fmt.t
  -> ?epi:Fmt.t
  -> Location.t
  -> Fmt.t
(** [fmt_within loc] formats the comments associated with [loc] that appear
    within [loc]. *)

module Toplevel : sig
  val fmt_before :
       t
    -> Conf.t
    -> fmt_code:(Conf.t -> string -> (Fmt.t, unit) Result.t)
    -> Location.t
    -> Fmt.t
  (** [fmt_before loc] formats the comments associated with [loc] that appear
      before [loc]. *)

  val fmt_after :
       t
    -> Conf.t
    -> fmt_code:(Conf.t -> string -> (Fmt.t, unit) Result.t)
    -> Location.t
    -> Fmt.t
  (** [fmt_after loc] formats the comments associated with [loc] that appear
      after [loc]. *)
end

val drop_inside : t -> Location.t -> unit

val drop_before : t -> Location.t -> t

val has_before : t -> Location.t -> bool
(** [has_before t loc] holds if [t] contains some comment before [loc]. *)

val has_within : t -> Location.t -> bool
(** [has_within t loc] holds if [t] contains some comment within [loc]. *)

val has_after : t -> Location.t -> bool
(** [has_after t loc] holds if [t] contains some comment after [loc]. *)

val remaining_comments : t -> Cmt.t list
(** Returns comments that have not been formatted yet. *)

val remaining_locs : t -> Location.t list

val remaining_before : t -> Location.t -> Cmt.t list
(** [remaining_before c loc] returns the comments before [loc] *)

val diff :
  Conf.t -> Cmt.t list -> Cmt.t list -> (string, string) Either.t Sequence.t
(** Difference between two lists of comments. *)

val preserve : (t -> Fmt.t) -> t -> string
(** [preserve fmt_x x] formats like [fmt_x x] but returns a string and does
    not consume comments from the internal state. *)
