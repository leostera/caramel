type 'm pid

external spawn : (unit -> 'a) -> 'm pid = ""

external send : 'm pid -> 'm -> unit = ""

external self : unit -> 'm pid = ""

external register : 'a -> 'm pid -> bool = ""

external whereis : 'a -> 'm pid = ""

external is_atom : 'a -> bool = ""

external is_pid : 'a -> bool = ""

external is_port : 'a -> bool = ""

external length : 'a list -> int = ""
