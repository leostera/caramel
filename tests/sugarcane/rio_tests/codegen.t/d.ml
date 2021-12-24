external display : 'a -> unit = "erlang:display"

let rec main x = 
  match x with
  | [] -> () 
  | x :: rest ->
      display x;
      main rest
