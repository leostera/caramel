module type Intf = sig
  type t

  val empty : unit -> t

  val concat : t -> t -> t

  val ( <+> ) : t -> t -> t
end

module type Base = sig
  type t

  val empty : unit -> t

  val concat : t -> t -> t
end

module Make (M : Base) : Intf with type t = M.t = struct
  type t = M.t

  let empty = M.empty

  let concat = M.concat

  let ( <+> ) = M.concat
end

(* FIXME: This module isn't being generated! D: *)
module Int_add = Make (struct
  type t = int

  let empty () = 0

  let concat a b = a + b
end)

open Int_add

let run () =
  let zero = empty () in
  let one = concat zero 1 in
  let two = IntAdd.( <+> ) one 1 in
  two
