open Gen_tcp_types

external accept : socket -> (connection, string) result = ""

(* Sets up a socket to listen on port [port] on the local host. *)
external listen : int -> listen_opt list -> (socket, string) result = ""

external send : connection -> string -> unit = ""
external close : connection -> unit = ""
external controlling_process : connection -> 'a Erlang.pid -> unit = ""
