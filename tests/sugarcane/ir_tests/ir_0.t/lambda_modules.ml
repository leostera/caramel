(*** simple module ***)

module A = struct
  let f _n = 1
end

let a () = A.f ()

(*** nested module ***)

module A2 = struct
  let f _n = 1

  module B = struct
    let g () = 2
  end
end

let a2 () = A2.f ()

(*** included module ***)

module A3 = struct
  let f _n = 1

  include struct
    let g () = f ()
  end
end

let a3 () = A3.g ()
