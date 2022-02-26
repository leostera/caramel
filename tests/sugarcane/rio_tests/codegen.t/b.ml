external format : string -> 'a list -> unit = "io:format"

let f x () = format "hello ~p!" ["world"]

let main x = format "Hello, ~p!" x
