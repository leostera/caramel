type pid = unit

let spawn : (unit -> unit) -> pid = fun _ -> ()
