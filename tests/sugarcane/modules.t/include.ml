(* FIXME: this doesn't __actually include__ the source of A, but it should *)
include (struct
  let f () = 0
end)

let run () = f ()
