module A = struct
  let a () = true

  (* module C should only export the function c
   *)
  module C : sig
    val c : unit -> bool
  end = struct
    let c () = true

    let internal_c () = true

    (* module D should be generated empty *)
    module D : sig end = struct
      let d () = true
    end
  end
end

module B = struct
  let b x y = (x, y)
end
