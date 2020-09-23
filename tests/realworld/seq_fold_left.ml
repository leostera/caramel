let fold_left f acc seq =
  let rec aux f acc seq =
    match seq () with
    | Nil -> acc
    | Cons (x, next) ->
        let acc = f acc x in
        aux f acc next
  in
  aux f acc seq
