external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"
external ( - ) : 'a -> 'a -> 'a = "erlang:-"

external format : string -> 'a list -> unit = "io:format"

type 'a rle = One of 'a | Many of int * 'a

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rec many acc n x = if n = 0 then acc else many (x :: acc) (n - 1) x

let rec aux acc = function
  | [] -> acc
  | One x :: t -> aux (x :: acc) t
  | Many (n, x) :: t -> aux (many acc n x) t

let decode l = rev (aux [] l) []

let main _xs =
  let input =
    [
      Many (4, "a");
      One "b";
      Many (2, "c");
      Many (2, "a");
      One "d";
      Many (4, "e");
    ]
  in
  format "~p\n" [ decode input ]
