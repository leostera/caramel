type init_args = int
type message = Add of int
type reply = int
type state = int
type call_response =
  | Reply of reply * state
  | No_reply of state

let init x = Ok x

let handle_call message _pid state =
  match message with
  | Add i -> Reply (state, state + i)

  (*
module Server : Gen_server.Intf
  with type gs_init_args = init_args
   and type gs_message = message
   and type gs_reply = reply
   and type gs_state = state
= Gen_server.Make(struct
  type gs_call_response = call_response = 
    | Reply of reply * state
    | No_reply of state
  type gs_init_args = init_args
  type gs_message = message
  type gs_reply = reply
  type gs_state = state
  let init = init
  let handle_call = handle_call
end)

include Server
*)

type rpc =
  | Call : 'a Erlang.process * message -> rpc

let call pid msg =
  Erlang.send pid (Call (Process.self (), msg ));
  Process.recv ~timeout:(Process.Bounded 5000)

let handle_call pid msg state =
  match handle_call msg pid state with
  | Reply (reply, state2) ->
      Erlang.send pid reply;
      state2
  | No_reply state2 ->
      state2

let rec loop:
    'a. state ->
            recv: (timeout:Process.after_time -> rpc option) ->
            'b
= fun state ~recv ->
  let state2 =
    match recv ~timeout:Process.Infinity with
    | Some (Call (pid, msg)) -> handle_call (pid :> 'a Erlang.process) msg state
    | None -> state
  in
  loop state2 ~recv

let start args = Process.spawn ( fun recv ->
  match init args with
  | Ok initial_state -> loop initial_state ~recv
  | Error err -> Io.format "ERROR: Process crashed - ~p\n" [err];
)
