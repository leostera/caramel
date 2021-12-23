external display : 'a -> unit = "erlang:display"

let rec main = function [] -> () | [ x ] -> display x | _ :: t -> main t
