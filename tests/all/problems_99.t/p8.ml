external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external display : 'a -> unit = "erlang:display"

type 'a node = One of 'a | Many of 'a node list

let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller

let main _xs =
  let input =
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
  display (compress input)
