module A = struct
  let f () = 0
end

(* FIXME: this doesn't __actually include__ the source of A, but it should *)
include A

let run () = f ()
