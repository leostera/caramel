module A = struct
  let f () = ()
end
module B : sig
  val f : unit -> unit
end = struct
  let f () = ()
  let g () = ()
end
