type after_time = Infinity | Bounded of int

type 'm recv = timeout:after_time -> 'm option

external __caramel_recv : 'a -> 'm option = ""

let recv ~timeout =
  let f = __caramel_recv in
  match timeout with Infinity -> f `Infinity | Bounded t -> f t

let make f =
  Erlang.spawn (fun () ->
      let pid : 'm Erlang.pid = Erlang.self () in
      f pid recv)

let send proc msg = Erlang.send proc msg

let contramap f pid =
  make (fun _self recv ->
      match recv ~timeout:Infinity with Some a -> send pid (f a) | None -> ())
