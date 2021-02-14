open! Import

type t

val content_length : t -> int

val content_type : t -> string

val create : content_length:int -> t

val write : t -> out_channel -> unit

val read : in_channel -> t
