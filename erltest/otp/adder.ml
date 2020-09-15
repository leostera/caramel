type message =
  | Add of int
  | Reset

type t = int

type resp = unit

type call_reply =
  | No_reply of t
  | Reply of resp * t

include (Gen_server.Make(struct
  type nonrec msg = message

  type nonrec state = t

  type nonrec call_value = resp

  type nonrec reply = call_reply

  let handle_call state _msg =
    Io.format "~p" state;
    No_reply state

end): Gen_server.Intf with type msg = message
                       and type state = t
                       and type call_value = resp
                       and type reply = call_reply)


let add ~pid x = call pid (Add x)

let reset ~pid = call pid Reset
