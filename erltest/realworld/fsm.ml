type valid

type invalid

type 'a state = { counter : int; flag : bool }

let empty : unit -> 'a state = fun () -> { counter = 0; flag = false }

let make : unit -> invalid state = fun () -> empty ()

let validate : invalid state -> (valid state, string) result =
 fun { counter; flag } -> Ok { counter; flag }

let run : valid state -> unit =
 fun { counter; _ } ->
  let _ = Io.format "counter=~p\n" [ counter ] in
  ()

let nested_lists () = [ [ 1; 2 ]; [ 3; 4 ] ]

let start () =
  let empty = make () in
  match validate empty with
  | Ok s -> run s
  | Error err -> Io.format "~p" [ err; err ]
