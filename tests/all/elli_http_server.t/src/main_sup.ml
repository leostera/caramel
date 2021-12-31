open Erlang

let start_link () =
  Supervisor.start_link
    (Local (Erlang.atom "Caramel.Main_sup"))
    (Erlang.atom "Caramel.Main_sup")
    []

let init _args =
  let sup_flags =
    Supervisor.{ strategy = One_for_all; intensity = 0; period = 1 }
  in
  let child_specs =
    [
      Elli.child_spec ~name:"http_server"
        ~opts:Elli.[ Callback (Erlang.atom "Caramel.Main"); Port 2112 ];
    ]
  in
  Ok (sup_flags, child_specs)
