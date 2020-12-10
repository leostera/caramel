let run_one (n, day) =
  Io.format "Running day #~p..." [ n ];
  Io.format "~p\n" [ day () ]

let run_days days = Lists.foreach (fun day -> run_one day) days

let main _ =
  Io.format "==# Advent Of Code 2020! #==\n\n" [];
  let days = [ (1, fun () -> Day_1.run ()); (2, fun () -> Day_2.run ()) ] in
  run_days days;
  Io.format "\n\n" []
