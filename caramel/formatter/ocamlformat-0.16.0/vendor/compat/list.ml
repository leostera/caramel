open Base
include List

let partition_map l ~f =
  let fst, snd =
    List.fold_left l
      ~f:(fun (fst, snd) x ->
        match f x with
        | Either0.First x' -> (x' :: fst, snd)
        | Either0.Second x' -> (fst, x' :: snd))
      ~init:([], [])
  in
  (List.rev fst, List.rev snd)
