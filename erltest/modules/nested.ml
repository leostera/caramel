module A = struct
  let a () = 1
  (* module C should only export the function c
   *)
  module C : sig
    val c : unit -> string
  end = struct
    let c () = ""
    let internal_c () = ""
    (* module D should not be generated, since it is completely private
       and not in use by anybody
     *)
    module D : sig end = struct
      let d () = true
    end
  end
end
module B = struct
  let b x y = x && y
end
