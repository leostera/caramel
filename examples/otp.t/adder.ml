type init_args = int

type message = Add of int

type reply = int

type state = int

type call_response = Reply of reply * state | No_reply of state

let init x = Ok x

let handle_cast _ state = No_reply state

let handle_call message _pid state =
  Timer.sleep 1000;
  match message with Add i -> Reply (state, state + i)

let add : message Erlang.process -> message -> _ =
 fun pid x -> `Ok (Gen_server.call pid x)

let start_link : init_args -> message Erlang.process =
 fun args -> Gen_server.start_link `Adder args []
