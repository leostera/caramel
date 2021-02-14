open Import
open Fiber_stream

type t =
  { in_ : Jsonrpc.packet In.t
  ; out : Jsonrpc.packet Out.t
  ; in_thread : Scheduler.thread option ref
  ; out_thread : Scheduler.thread option ref
  ; io : Io.t
  }

let close_out out_thread =
  match !out_thread with
  | None -> ()
  | Some thread ->
    Scheduler.stop thread;
    out_thread := None

let close_in in_thread =
  match !in_thread with
  | None -> ()
  | Some thread ->
    Scheduler.stop thread;
    in_thread := None

let send t p = Out.write t.out (Some p)

let recv t = In.read t.in_

let make s io =
  let in_thread = ref (Some (Scheduler.create_thread s)) in
  let in_ =
    In.create (fun () ->
        let open Fiber.O in
        match !in_thread with
        | None -> Fiber.return None
        | Some thread ->
          let task =
            Scheduler.async_exn thread (fun () ->
                let res = Io.read io in
                (match res with
                | None -> Io.close_in io
                | Some _ -> ());
                res)
          in
          let+ res = Scheduler.await_no_cancel task in
          let res = Result.ok_exn res in
          (match res with
          | None -> close_in in_thread
          | Some _ -> ());
          res)
  in
  let out_thread = ref (Some (Scheduler.create_thread s)) in
  let out =
    let open Fiber.O in
    Out.create (fun t ->
        match (!out_thread, t) with
        | None, None -> Fiber.return ()
        | None, Some packet ->
          Log.log ~section:"Stream_io" (fun () ->
              Log.msg "dropped write"
                [ ("packet", Jsonrpc.yojson_of_packet packet) ]);
          Fiber.return ()
        | Some thread, _ ->
          let+ res =
            Scheduler.async_exn thread
              (match t with
              | None -> fun () -> Io.close_out io
              | Some p -> fun () -> Io.send io p)
            |> Scheduler.await_no_cancel
          in
          (match t with
          | None -> close_out out_thread
          | Some _ -> ());
          Result.ok_exn res)
  in
  { in_; out; in_thread; out_thread; io }

let close (t : t) =
  let in_ () = Out.write t.out None in
  let out () =
    match !(t.in_thread) with
    | None -> Fiber.return ()
    | Some thread ->
      let open Fiber.O in
      let+ close =
        Scheduler.async_exn thread (fun () -> Io.close_in t.io)
        |> Scheduler.await_no_cancel
      in
      close_in t.in_thread;
      Result.ok_exn close
  in
  Fiber.fork_and_join_unit in_ out
