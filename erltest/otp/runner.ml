let run () =
  let pid = Adder.start_link 10 in
  let `Ok reply = Adder.add pid (Add 1) in
  Io.format "reply: ~p" [reply];
