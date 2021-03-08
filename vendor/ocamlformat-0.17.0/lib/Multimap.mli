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

(** Maps with a list of values attached to a key. This is an extension of the
    [Map] module, so these are compatible with [Map.mem], etc. *)

type ('key, 'value, 'cmp) t = ('key, 'value list, 'cmp) Map.t

module M (K : sig
  type t

  type comparator_witness
end) : sig
  type nonrec 'value t = (K.t, 'value, K.comparator_witness) t
end

val update_multi :
     ('key, 'value, 'cmp) t
  -> src:'key
  -> dst:'key
  -> f:('value list -> 'value list -> 'value list)
  -> ('key, 'value, 'cmp) t

val change_multi :
  ('key, 'value, 'cmp) t -> 'key -> 'value list -> ('key, 'value, 'cmp) t

val partition_multi :
     ('key, 'value, 'cmp) t
  -> src:'key
  -> dst:'key
  -> f:('value -> bool)
  -> ('key, 'value, 'cmp) t
(** Split the values of key [src] with [f], the values satisfying [f] are
    moved to key [dst] while the others remain associated to key [src]. *)

val filter :
  ('key, 'value, 'cmp) t -> f:('value -> bool) -> ('key, 'value, 'cmp) t

val to_list : (_, 'value, _) t -> 'value list
