external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external display : 'a -> unit = "erlang:display"

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rec main xs = display (rev xs [])
