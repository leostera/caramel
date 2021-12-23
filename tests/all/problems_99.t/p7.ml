external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external display : 'a -> unit = "erlang:display"

type 'a node = One of 'a | Many of 'a node list

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rec flatten acc t =
  match t with
  | [] -> acc
  | One x :: t -> flatten (x :: acc) t
  | Many l :: t -> flatten (flatten acc l) t

let main _xs =
  let input =
    [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  in
  display (rev (flatten [] input) [])
