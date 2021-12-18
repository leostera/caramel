(*** simple module ***)

module M0 = struct
  let f () = 1
end

let c0 () = M0.f ()

(*** nested module ***)

module M1 = struct
  module S1 = struct
    module S2 = struct
      module S3 = struct
        let f () = 1
      end
    end
  end
end

let c1 () = M1.S1.S2.S3.f ()
