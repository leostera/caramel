module String = struct
  include String

  let foldi t ~init ~f =
    let n = length t in
    let rec loop i ac = if i = n then ac else loop (i + 1) (f i ac t.[i]) in
    loop 0 init
end
