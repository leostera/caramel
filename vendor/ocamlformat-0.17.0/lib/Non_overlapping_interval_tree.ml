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

module Make (Itv : IN) = struct
  (* simple but (asymptotically) suboptimal implementation *)

  type itv = Itv.t

  type t = {roots: Itv.t list; map: Itv.t list Map.M(Itv).t}

  let empty = {roots= []; map= Map.empty (module Itv)}

  let roots t = t.roots

  let map_add_multi map ~key ~data =
    Map.update map key ~f:(function None -> [data] | Some l -> data :: l)

  (** Descend tree from roots, find deepest node that contains elt. *)
  let rec parents map roots ~ancestors elt =
    Option.value ~default:ancestors
      (List.find_map roots ~f:(fun root ->
           if Itv.contains root elt then
             let ancestors = root :: ancestors in
             ( match Map.find map root with
             | Some children -> parents map children ~ancestors elt
             | None -> ancestors )
             |> Option.some
           else None ) )

  let add_root t root = {t with roots= root :: t.roots}

  let add_child t ~parent ~child =
    {t with map= map_add_multi t.map ~key:parent ~data:child}

  let map_lists ~f {roots; map} = {roots= f roots; map= Map.map map ~f}

  let rec find_in_previous t elt = function
    | [] -> parents t.map t.roots elt ~ancestors:[]
    | p :: ancestors when Itv.contains p elt ->
        parents t.map [p] elt ~ancestors
    | _ :: px -> find_in_previous t elt px

  (** Add elements in decreasing width order to construct tree from roots to
      leaves. That is, when adding an interval to a partially constructed
      tree, it will already contain all wider intervals, so the new
      interval's parent will already be in the tree. *)
  let of_list elts =
    let add (prev_ancestor, t) elt =
      let ancestors = find_in_previous t elt prev_ancestor in
      let t =
        match ancestors with
        | parent :: _ -> add_child t ~parent ~child:elt
        | [] -> add_root t elt
      in
      (ancestors, t)
    in
    elts
    |> List.dedup_and_sort ~compare:Itv.compare_width_decreasing
    |> List.fold ~init:([], empty) ~f:add
    |> snd
    |> map_lists ~f:(List.sort ~compare:Itv.compare_width_decreasing)

  let children {map; _} elt = Option.value ~default:[] (Map.find map elt)

  let dump tree =
    let open Fmt in
    let rec dump_ tree roots =
      vbox 0
        (list roots "@," (fun root ->
             let children = children tree root in
             vbox 1
               ( str (Sexp.to_string_hum (Itv.comparator.sexp_of_t root))
               $ wrap_if
                   (not (List.is_empty children))
                   "@,{" " }" (dump_ tree children) ) ) )
    in
    set_margin 100000000 $ dump_ tree tree.roots
end
