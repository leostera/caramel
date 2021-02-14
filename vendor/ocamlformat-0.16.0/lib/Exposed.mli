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

(** Predicates for determining if an AST starts/ends with a [<]/[>] symbol
    (respectively) when printed.

    These are used to avoid emitting the sequences [\{<], [\[<], [>\}] and
    [>\]], which are reserved keywords. *)

open Migrate_ast
open Parsetree

(** Predicates for [<] on the LHS of printed AST nodes. *)
module Left : sig
  val core_type : core_type -> bool
end

module Right : sig
  (** Predicates for [>] on the RHS of printed AST nodes. *)

  val core_type : core_type -> bool

  val label_declaration : label_declaration -> bool

  val row_field : row_field -> bool

  val payload : payload -> bool

  val list : elt:('a -> bool) -> 'a list -> bool
  (** [list ~elt l] holds iff [elt] holds of the {i last} element in [l], and
      is [false] if [l] is empty. *)
end
