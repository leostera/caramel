let rec loop x =
  Io.format "~p\n" [Erlang.self()];
  loop x

let start x =
  Erlang.spawn (fun () -> loop x )

let do_work () =
  let pid = start 1 in
  (* Erlang.send pid true *)
  Erlang.send pid 2112
