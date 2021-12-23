external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external format : string -> 'a list -> unit = "io:format"

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rec aux count acc = function
  | [] -> [] (* Can only be reached if original list is empty *)
  | [ x ] -> (count + 1, x) :: acc
  | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t else aux 0 ((count + 1, a) :: acc) t

let encode list = rev (aux 0 [] list) []

let main _xs =
  let input =
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
  format "~p\n" [ encode input ]
