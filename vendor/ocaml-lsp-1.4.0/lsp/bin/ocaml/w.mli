(** Generation ocaml code *)

open Import

type t = unit Pp.t

type w = t

val surround : [ `Curly | `Paren | `Square ] -> 'a Pp.t -> 'a Pp.t

module Json : sig
  val invalid_pat : string -> w * w

  val typ : string

  module Literal : sig
    val str : string -> string

    val int : int -> string

    val null : string

    val bool : bool -> string
  end

  val str : string -> string

  val int : string -> string

  val bool : string -> string
end

module Attr : sig
  type t

  val make : string -> unit Pp.t list -> t
end

module Type : sig
  val string : w

  val int : w

  val bool : w

  val name : string -> w

  val and_ : string -> w -> w

  val decl : string -> w -> w

  val record : (string * w) list -> w

  val field_attrs : field:w -> attrs:Attr.t list -> w

  val rec_decls : (string * w) list -> w

  val var : string -> w

  val poly : (string * w list) list -> w

  val app : w -> w list -> w

  val tuple : w list -> w

  val deriving : w -> record:bool -> w

  val opt_attr : w

  val opt_field : w -> w

  val default : w -> string -> w

  val key : string -> w

  val variant : (string * w list) list -> w
end

module Sig : sig
  val module_ : string -> w -> w

  val val_ : string -> w list -> w

  val assoc : w -> w -> w

  module Json : sig
    val arr : string -> w list

    val to_json : string -> w

    val of_json : string -> w
  end
end

val warnings : string -> w

val match_ : string -> (w * w) list -> w

val module_ : string -> w -> w

val opens : string list -> w

val of_json : string -> w -> w

val to_json : string -> w -> w

val record : (string * w) list -> w
