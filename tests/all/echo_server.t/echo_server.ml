type ('ok, 'err) result = Ok of 'ok | Error of 'err

type socket

type conn

external dbg : string -> 'a list -> unit = "io:format"

external send : conn -> string -> unit = "gen_tcp:send"

external recv : conn -> int -> (string, [ `closed ]) result = "gen_tcp:recv"

external close : conn -> unit = "gen_tcp:close"

external accept : socket -> (conn, 'a) result = "gen_tcp:accept"

external listen : int -> 'a list -> (socket, 'a) result = "gen_tcp:listen"

let rec loop socket =
  let (Ok conn) = accept socket in
  match recv conn 0 with
  | Ok data ->
      dbg "data: ~p~n" [ data ];
      send conn data;
      loop socket
  | Error `closed -> close conn

let main _ =
  let port = 2112 in
  let (Ok socket) =
    listen port [ `binary; `active false; `packet `line; `reuseaddr true ]
  in
  dbg "listening on ~p~n" [ port ];
  loop socket
