type init_args = int
type message = Add of int
type reply = int
type state = int

type rpc

val start : init_args -> rpc Erlang.process

val call : rpc Erlang.process -> message -> reply option
