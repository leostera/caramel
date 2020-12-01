
let main _ =
  Io.format "\n\n# Advent Of Code 2020!\n\n" [];
  let days = [
    (1, fun () -> Day_1.run ());
  ] in
  Lists.foreach (fun (n, day) ->
    Io.format "Running day ~p..." [n];
    let result = day () in
    match result with
    | Ok _ -> Io.format "OK\n" []
    | Error _ -> Io.format "ERR\n" []
  ) days;
  Io.format "\n\n" []
