type 'message process

external spawn : (unit -> 'a) -> 'message process = ""

external send : 'message process -> 'message -> unit = ""

external self : unit -> 'message process = ""
