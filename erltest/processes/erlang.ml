type 'message process

external spawn : (unit -> 'a) -> 'message process = ""

external send : 'message process -> 'message -> unit = ""

external self : unit -> 'message process = ""

external register : 'a -> 'message process -> bool = ""

external whereis : 'a -> 'message process = ""

external is_atom : 'a -> bool = ""

external is_pid : 'a -> bool = ""

external is_port : 'a -> bool = ""
