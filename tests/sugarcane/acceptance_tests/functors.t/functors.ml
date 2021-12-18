module type Base = sig
  type t

  val empty : unit -> t

  val concat : t -> t -> t
end

module Make (M : Base) = struct
  type t = M.t

  let empty = M.empty

  let concat = M.concat

  let ( <+> ) = M.concat
end

module Int_add = Make (struct
  type t = int

  let empty () = 0

  let concat a _b = a
end)

include Make (struct
  type t = string

  let empty () = ""

  let concat a _b = a
end)
