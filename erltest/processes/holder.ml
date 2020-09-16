let handle_message state msg =
  let (x, y) = state in
  match msg with
  | Some `Reset -> ("", 0)
  | Some (`Add z) -> (x, z)
  | Some (`Hello n) -> (n, y)
  | None -> state

let rec loop ~recv state =
  Io.format "current_state: ~p\n" [state];
  let msg = recv ~timeout:5000 in
  let state2 = handle_message state msg in
  loop ~recv state2

let start x = Process.spawn (fun recv -> loop ~recv x )

let do_work () =
  let pid = start ("hi", 0) in
  Erlang.send pid (`Hello "joe")
