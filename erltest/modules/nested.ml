module A = struct
  let a () = 1
  module C = struct
    let c () = ""
    module D = struct
      let d () = true
    end
  end
end
module B = struct
  let b x y = x && y
end
