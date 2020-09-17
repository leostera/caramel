type after_time = | Infinity | Bounded of int

type 'message recv = timeout:after_time -> 'message option

external __caramel_recv: 'a -> 'message option = ""

let recv ~timeout =
  let f = __caramel_recv in
  match timeout with
  | Infinity -> f `Infinity
  | Bounded t -> f t

let make : ('message Erlang.process -> 'message recv -> 'a) -> 'message Erlang.process =
  fun f ->
    Erlang.spawn(fun () ->
      let pid : 'message Erlang.process = Erlang.self () in
      f pid recv )

let send : 'message Erlang.process -> 'message -> unit =
  fun proc msg -> Erlang.send proc msg

let contramap : ('b -> 'a) -> 'a Erlang.process -> 'b Erlang.process =
  fun f pid ->
    make (fun _self recv ->
      match recv ~timeout:Infinity with
      | Some a -> send pid (f a)
      | None -> ())
