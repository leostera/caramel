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

(*** anonymous included module ***)

module A3 = struct
  let f _n = 1

  include struct
    let g () = f ()
  end
end

let a3 () = A3.g ()

(*** named included module ***)

module A4 = struct
  let f _n = 1

  include A3
end

let a4 () = A4.f (A4.g ())
