module A = struct
  let f () = 0
end

let scoped_open () = A.(f ())

let let_open () =
  let open A in
  f ()
