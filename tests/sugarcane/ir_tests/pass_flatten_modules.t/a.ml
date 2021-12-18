(*** simple module ***)

module M0 = struct
  let m0_f () = 1

  let m0_g () = 2
end

let c0 () = M0.m0_f ()

(*** nested module ***)

module M1 = struct
  module M0 = struct
    let m0_f () = 1
  end

  let m1_f () = 2
end

let c1 () = M1.M0.m0_f ()
