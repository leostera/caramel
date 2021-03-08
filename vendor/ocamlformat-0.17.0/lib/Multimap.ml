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

type ('key, 'value, 'cmp) t = ('key, 'value list, 'cmp) Map.t

module M (K : sig
  type t

  type comparator_witness
end) =
struct
  type nonrec 'v t = (K.t, 'v, K.comparator_witness) t
end

let update_multi map ~src ~dst ~f =
  Option.fold (Map.find map src) ~init:(Map.remove map src)
    ~f:(fun new_map src_data ->
      Map.update new_map dst ~f:(fun dst_data ->
          Option.fold dst_data ~init:src_data ~f ) )

let change_multi map key data =
  Map.change map key ~f:(function _ -> Some data)

let partition_multi map ~src ~dst ~f =
  let move, dontmove = List.partition_tf (Map.find_multi map src) ~f in
  let map =
    List.fold_left ~init:map (List.rev move) ~f:(fun map data ->
        Map.add_multi map ~key:dst ~data )
  in
  change_multi map src dontmove

let filter map ~f = Map.map map ~f:(List.filter ~f)

let to_list map = Map.to_alist map |> List.concat_map ~f:snd
