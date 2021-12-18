module M0 = struct
  let m0_f x y = (x, y)
end

module M1 = struct
  let m1_f y = [ y; y ]
end

let f0 () = (M0.m0_f 1 2, M1.m1_f 2)
