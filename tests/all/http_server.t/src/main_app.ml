open Erlang

let start _args _opts = Main_sup.start_link ()

let stop _state = `ok
