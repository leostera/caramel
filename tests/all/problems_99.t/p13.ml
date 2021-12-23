external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external ( - ) : 'a -> 'a -> 'a = "erlang:-"

external format : string -> 'a list -> unit = "io:format"

type 'a rle = One of 'a | Many of int * 'a

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rle count x = if count = 0 then One x else Many (count + 1, x)

let rec aux count acc = function
  | [] -> [] (* Can only be reached if original list is empty *)
  | [ x ] -> rle count x :: acc
  | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t else aux 0 (rle count a :: acc) t

let encode list = rev (aux 0 [] list) []

let main _xs =
  let input =
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
  format "~p\n" [ encode input ]
