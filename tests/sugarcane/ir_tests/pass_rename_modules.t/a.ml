let f () = 1

module B = struct
  let f () = 1

  module C = struct
    let f () = 1

    module D = struct
      let f () = 1
    end
  end
end
