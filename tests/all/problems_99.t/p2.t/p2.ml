external display : 'a -> unit = "erlang:display"

let rec main = function
  | [] | [ _ ] -> ()
  | [ x; y ] -> display (x, y)
  | _ :: t -> main t
