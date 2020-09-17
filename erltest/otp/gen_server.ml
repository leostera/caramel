module type Intf = sig
  type gs_state
  type gs_init_args
  type gs_message
  type gs_reply
  type rpc

  val start : gs_init_args -> rpc Erlang.process

  val call : rpc Erlang.process -> gs_message -> gs_reply option
end

module type Base = sig
  type gs_state
  type gs_init_args
  type gs_message
  type gs_reply

  val init : gs_init_args -> (gs_state, string) result

  type gs_call_response =
    | Reply of gs_reply * gs_state
    | No_reply of gs_state
    (*
    | Bounded_reply of 'reply * state * Erlang.processimeout
    | Bounded_no_reply of state * Erlang.processimeout
    | Stop of string * 'reply * state
    *)

  val handle_call : gs_message -> 'state Erlang.process -> gs_state -> gs_call_response
end

module Make (M: Base): (Intf with type gs_state = M.gs_state
                              and type gs_init_args = M.gs_init_args
                              and type gs_message = M.gs_message
                              and type gs_reply = M.gs_reply
                       ) = struct

  type gs_state = M.gs_state
  type gs_init_args = M.gs_init_args
  type gs_message = M.gs_message
  type gs_reply = M.gs_reply

  type call = { pid: 'a. unit -> 'a Erlang.process; msg: gs_message }

  type rpc = Call of call

  let call pid msg =
    Erlang.send pid (Call { pid = Erlang.self; msg });
    Process.recv ~timeout:(Process.Bounded 5000)

  let handle_call {msg; pid} state =
    let pid = pid () in
    match M.handle_call msg pid state with
    | Reply (reply, state2) ->
        Erlang.send pid reply;
        state2
    | No_reply state2 ->
        state2

  let rec loop state ~recv =
    let state2 =
      let open Process in
      match recv ~timeout:Infinity with
      | Some (Call rpc) -> handle_call rpc state
      | None -> state
    in
    loop state2 ~recv

  let start args = Process.spawn ( fun recv ->
    match M.init args with
    | Ok initial_state -> loop initial_state ~recv
    | Error err -> Io.format "ERROR: Process crashed - ~p\n" [err];
  )

end
