external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external display : 'a -> unit = "erlang:display"

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let is_palindrome list = list = rev list []

let rec main xs = display (is_palindrome xs)
