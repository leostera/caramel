external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external display : 'a -> unit = "erlang:display"

let rec length list n = match list with [] -> n | _ :: t -> length t (n + 1)

let rec main xs = display (length xs 0)
