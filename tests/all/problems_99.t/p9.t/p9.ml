external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external format : string -> 'a list -> unit = "io:format"

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rec aux current acc = function
  | [] -> [] (* Can only be reached if original list is empty *)
  | [ x ] -> (x :: current) :: acc
  | a :: (b :: _ as t) ->
      if a = b then aux (a :: current) acc t
      else aux [] ((a :: current) :: acc) t

let pack list = rev (aux [] [] list) []

let main _xs =
  let input =
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
  format "~p\n" [ pack input ]
