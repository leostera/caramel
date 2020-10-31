type socket
type connection

type packet = Http
type listen_opt =
  | Active of bool
  | Packet of packet

external accept : socket -> (connection, string) result = ""
external listen : int -> listen_opt list -> (socket, string) result = ""
external send : connection -> string -> unit = ""
external close : connection -> unit = ""
external controlling_process : connection -> 'a Erlang.pid -> unit = ""
