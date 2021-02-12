open! Import

include Json.Jsonable.S

val equal : t -> t -> bool

val to_dyn : t -> Dyn.t

val hash : t -> int

val to_path : t -> string

val of_path : string -> t

val to_string : t -> string

val pp : Format.formatter -> t -> unit
