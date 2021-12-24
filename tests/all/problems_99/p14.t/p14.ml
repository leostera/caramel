external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external ( - ) : 'a -> 'a -> 'a = "erlang:-"

external format : string -> 'a list -> unit = "io:format"

let rec duplicate = function
    | [] -> []
    | h :: t -> h :: h :: duplicate t

let main xs =
  format "~p\n" [ duplicate xs ]
