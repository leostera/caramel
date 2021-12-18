module M0 = struct
  let m0_f x = (x, x)
end

module M1 = struct
  let m1_f y = [ y; y ]
end

let f0 () = (M0.m0_f 1, M1.m1_f 2)
