type after_time = Infinity | Bounded of int

type 'm recv = timeout:after_time -> 'm option

external __caramel_recv : 'a -> 'm option = ""

let recv ~timeout =
  match timeout with
  | Infinity -> Caramel_runtime.recv_and_wait ()
  | Bounded t -> Caramel_runtime.recv_with_timeout t

let make : ('m Erlang.pid -> 'm recv -> 'a) -> 'm Erlang.pid =
 fun f ->
  Erlang.spawn (fun () ->
      let pid : 'm Erlang.pid = Erlang.self () in
      f pid recv)

let send : 'm Erlang.pid -> 'm -> unit = fun proc msg -> Erlang.send proc msg

let contramap : ('b -> 'a) -> 'a Erlang.pid -> 'b Erlang.pid =
 fun f pid ->
  make (fun _self recv ->
      match recv ~timeout:Infinity with Some a -> send pid (f a) | None -> ())
