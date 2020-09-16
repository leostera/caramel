external __caramel_recv: timeout:int -> 'message option = ""

let spawn : ((timeout:int -> 'message option) -> 'a) -> 'message Erlang.process =
  fun f -> Erlang.spawn(fun () -> f __caramel_recv )

