open Import

type state =
  | Closed
  | Active of
      { r : Unix.file_descr
      ; w : Unix.file_descr
      ; await_mutex : Mutex.t
      ; mutex : Mutex.t
      ; buf : Bytes.t
      }

type t = state ref

let create () =
  let r, w = Unix.pipe () in
  ref
    (Active
       { r
       ; w
       ; mutex = Mutex.create ()
       ; await_mutex = Mutex.create ()
       ; buf = Bytes.create 1
       })

let close t =
  match !t with
  | Closed -> ()
  | Active { mutex; await_mutex = _; r; w; buf = _ } ->
    Mutex.lock mutex;
    (try Unix.close w with Unix.Unix_error _ -> ());
    (try Unix.close r with Unix.Unix_error _ -> ());
    t := Closed;
    Mutex.unlock mutex

let select fd timeout =
  match Unix.select [ fd ] [] [] timeout with
  | [], _, _ -> Ok `Empty
  | [ _ ], _, _ -> Ok `Ready_to_read
  | exception Unix.Unix_error (Unix.EBADF, _, _) -> Error `Closed
  | _ -> assert false

let rec drain_pipe fd buf read_once =
  match Unix.read fd buf 0 1 with
  | exception Unix.Unix_error (Unix.EBADF, _, _) ->
    Error (`Closed (`Read read_once))
  | 0 -> drain_pipe fd buf read_once
  | 1 -> (
    let read_once = true in
    match select fd 0. with
    | Ok `Empty -> Ok ()
    | Ok `Ready_to_read -> drain_pipe fd buf read_once
    | Error `Closed -> Error (`Closed (`Read read_once)))
  | _ -> assert false

let await ?(timeout = -1.) t =
  match !t with
  | Closed -> Error (`Closed (`Read false))
  | Active t ->
    with_mutex t.await_mutex ~f:(fun () ->
        match select t.r timeout with
        | Ok `Empty -> Error `Timeout
        | Ok `Ready_to_read -> drain_pipe t.r t.buf false
        | Error `Closed -> Error (`Closed (`Read false)))

let signal t =
  match !t with
  | Closed -> Error `Closed
  | Active { w; buf; _ } -> (
    match Unix.write w buf 0 1 with
    | exception Unix.Unix_error (Unix.EBADF, _, _) ->
      close t;
      Error `Closed
    | 1 -> Ok ()
    | _ -> assert false)
