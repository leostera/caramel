module A = struct
  let f () = 0
end

module B : sig
  val f : unit -> int
end = struct
  let f () = 2

  let g () = ()
end

let run () = A.f (), B.f ()
