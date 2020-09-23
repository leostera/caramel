type error = unit
type state = int
type opts = unit

type message = [ `Add of int | `Get ]

let start_link () = Gen_server.start_link __MODULE__ [] None

let init () = Ok 0
