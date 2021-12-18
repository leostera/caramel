module B = struct
  module C = struct
    let z () = 900
  end

  let y () = C.z ()
end

let x () = B.y ()

let w () = x ()
