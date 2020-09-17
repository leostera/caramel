let run () =
  let pid = Adder.start 10 in
  match Adder.call pid (Add 1) with
  | Some rep -> Io.format "reply: ~p" [rep];
  | None -> Io.format "no reply?" []

