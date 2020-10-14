let rec loop t recv =
  Io.format "~p\n" [ t ];
  Timer.sleep t;
  loop (t * 2) recv

let start t = Process.spawn (fun _self r -> loop t r)
