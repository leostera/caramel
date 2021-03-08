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

(** A tree of non-overlapping intervals. Intervals are non-overlapping if
    whenever 2 intervals share more than an end-point, then one contains the
    other. *)

module type IN = sig
  include Comparator.S

  val contains : t -> t -> bool

  val compare_width_decreasing : t -> t -> int
end

module type S = sig
  type itv

  type t

  val of_list : itv list -> t
  (** If there are duplicates in the input list, earlier elements will be
      ancestors of later elements. *)

  val roots : t -> itv list

  val children : t -> itv -> itv list

  val dump : t -> Fmt.t
  (** Debug: dump debug representation of tree. *)
end

module Make (Itv : IN) : S with type itv = Itv.t
