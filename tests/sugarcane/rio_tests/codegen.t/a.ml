module M0 = struct
  type t = { first : int; second : int }

  let m0_f first second = { first; second }
end

module M1 = struct
  type t = { first : M0.t; second : M0.t }

  let m1_f y = { first = y; second = y }
end

let f0 () = M1.m1_f (M0.m0_f 1 2)
