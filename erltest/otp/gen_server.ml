module type Intf = sig
  type state
  type msg
  type call_value
  type reply

  val call : msg Process.t -> msg -> unit

  val handle_call : state -> msg -> reply
end

module type Base = sig
  type msg

  type state

  type call_value

  type reply

  val handle_call : state -> msg -> reply
end

module Make (M: Base): (Intf with type msg = M.msg
                              and type state = M.state
                              and type call_value = M.call_value
                              and type reply = M.reply
                       ) = struct
  type state = M.state
  type msg = M.msg
  type reply = M.reply
  type call_value = M.call_value

  let handle_call = M.handle_call
  let call pid m = ()
end
