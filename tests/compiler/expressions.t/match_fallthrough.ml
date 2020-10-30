let match_fall_through () =
  match 0 with
  | 1 | 2 -> true
  | _ -> false
