let iter f seq =
  let rec aux seq = match seq () with
    | Nil -> ()
    | Cons (x, next) ->
        let _ = f x in
        aux next
  in
  aux seq

