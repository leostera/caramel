type t

val create : unit -> t

val signal : t -> (unit, [ `Closed ]) result

val await :
     ?timeout:float
  -> t
  -> (unit, [ `Timeout | `Closed of [ `Read of bool ] ]) result

val close : t -> unit
