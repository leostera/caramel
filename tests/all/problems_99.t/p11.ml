external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external format : string -> 'a list -> unit = "io:format"

type 'a rle = One of 'a | Many of int * 'a

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let create_tuple cnt elem = if cnt = 1 then One elem else Many (cnt, elem)

let rec aux count acc = function
  | [] -> []
  | [ x ] -> create_tuple (count + 1) x :: acc
  | hd :: (snd :: _ as tl) ->
      if hd = snd then aux (count + 1) acc tl
      else aux 0 (create_tuple (count + 1) hd :: acc) tl

let encode l = rev (aux 0 [] l) []

let main _xs =
  let input =
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
  format "~p\n" [ encode input ]
