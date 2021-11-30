include (struct
  let f () = 0
  include (struct
    let g () = 1
  end)
end)

let run () = 1
