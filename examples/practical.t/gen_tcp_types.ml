type socket
type connection

type packet = Http | Line
type listen_opt =
  | Active of bool
  | Packet of packet

