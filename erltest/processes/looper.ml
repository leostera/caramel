let rec loop x = loop x

let start : int -> int Process.t =
  fun x -> Erlang.spawn (fun () -> loop x )

let do_work () =
  let pid = start 1 in
  Erlang.send pid 2
