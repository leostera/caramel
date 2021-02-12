open Stdune
open Fiber_unix.Fiber_stream
open! Jsonrpc
open Jsonrpc_fiber
open Fiber.O

module Stream_chan = struct
  type t = Jsonrpc.packet In.t * Jsonrpc.packet Out.t

  let close (_, o) = Out.write o None

  let send (_, o) p = Out.write o (Some p)

  let recv (i, _) = In.read i
end

module Jrpc = Jsonrpc_fiber.Make (Stream_chan)
module Context = Jrpc.Context

let print_json json =
  print_endline (Yojson.Safe.pretty_to_string ~std:false json)

let no_output () =
  let received_none = ref false in
  Out.create (function
    | None ->
      if !received_none then
        failwith "received None more than once"
      else
        received_none := true;
      Fiber.return ()
    | Some _ -> failwith "unexpected element")

let%expect_test "start and stop server" =
  let run () =
    let in_ = In.of_list [] in
    let jrpc = Jrpc.create ~name:"test" (in_, no_output ()) () in
    let run = Jrpc.run jrpc in
    Fiber.fork_and_join_unit (fun () -> run) (fun () -> Jrpc.stop jrpc)
  in
  let () = Fiber_test.test Dyn.Encoder.opaque (run ()) in
  [%expect {|
    "<opaque>" |}]

let%expect_test "server accepts notifications" =
  let notif =
    { Jsonrpc.Message.id = None
    ; method_ = "method"
    ; params = Some (`List [ `String "bar" ])
    }
  in
  let run () =
    let in_ = In.of_list [ Jsonrpc.Message notif ] in
    let on_notification c =
      let n = Context.message c in
      let state = Context.state c in
      assert (notif = { n with id = None });
      print_endline "received notification";
      Fiber.return (Notify.Stop, state)
    in
    let jrpc =
      Jrpc.create ~name:"test" ~on_notification (in_, no_output ()) ()
    in
    Jrpc.run jrpc
  in
  Fiber_test.test Dyn.Encoder.opaque (run ());
  [%expect {|
    received notification
    "<opaque>" |}]

let%expect_test "stopped fiber" =
  let run () =
    let in_ = In.create (fun () -> Fiber.never) in
    let jrpc = Jrpc.create ~name:"test" (in_, no_output ()) () in
    print_endline "running";
    let running = Jrpc.run jrpc in
    Fiber.fork_and_join_unit
      (fun () ->
        print_endline "stopping";
        Jrpc.stop jrpc)
      (fun () -> running)
  in
  Fiber_test.test Dyn.Encoder.opaque (run ());
  print_endline "stopped";
  [%expect {|
    running
    stopping
    "<opaque>"
    stopped |}]

let%expect_test "serving requests" =
  let id = `Int 1 in
  let request =
    { Jsonrpc.Message.id = Some id
    ; method_ = "bla"
    ; params = Some (`List [ `Int 100 ])
    }
  in
  let response_data = `String "response" in
  let run () =
    let responses = ref [] in
    let in_ = In.of_list [ Jsonrpc.Message request ] in
    let on_request c =
      let r = Context.message c in
      let state = Context.state c in
      assert (r = { request with id = r.id });
      let response = Jsonrpc.Response.ok r.id response_data in
      Fiber.return (Reply.now response, state)
    in
    let out = Out.of_ref responses in
    let jrpc = Jrpc.create ~name:"test" ~on_request (in_, out) () in
    let+ () = Jrpc.run jrpc in
    List.iter !responses ~f:(fun resp ->
        let json = Jsonrpc.yojson_of_packet resp in
        print_endline (Yojson.Safe.pretty_to_string ~std:false json))
  in
  Fiber_test.test Dyn.Encoder.opaque (run ());
  [%expect
    {|
    { "id": 1, "jsonrpc": "2.0", "result": "response" }
    "<opaque>" |}]

(* The current client/server implement has no concurrent handling of requests.
   We can show this when we try to send a request when handling a response. *)
let%expect_test "concurrent requests" =
  let print packet =
    print_endline
      (Yojson.Safe.pretty_to_string ~std:false
         (Jsonrpc.yojson_of_packet packet))
  in
  let waiter chan =
    let on_request c =
      let self = Context.session c in
      let request = Context.message c in
      print_endline "waiter: received request";
      print (Message { request with id = Some request.id });
      let response =
        Reply.later (fun send ->
            print_endline "waiter: making request";
            let* response =
              let request =
                Jsonrpc.Message.create ~id:(`Int 100) ~method_:"shutdown" ()
              in
              Jrpc.request self request
            in
            print_endline "waiter: received response:";
            print (Response response);
            let* () = send (Jsonrpc.Response.ok request.id `Null) in
            print_endline "waiter: stopping";
            let+ () = Jrpc.stop self in
            print_endline "waiter: stopped")
      in
      Fiber.return (response, ())
    in
    Jrpc.create ~name:"waiter" ~on_request chan ()
  in
  let waitee chan =
    let on_request c =
      print_endline "waitee: received request";
      let request = Context.message c in
      print (Message { request with id = Some request.id });
      let response =
        Reply.later (fun send ->
            let* () = send (Jsonrpc.Response.ok request.id (`Int 42)) in
            if request.method_ = "shutdown" then (
              let self = Context.session c in
              print_endline "waitee: stopping";
              let+ () = Jrpc.stop self in
              print_endline "waitee: stopped"
            ) else
              Fiber.return ())
      in
      let state = Context.state c in
      Fiber.return (response, state)
    in
    Jrpc.create ~on_request ~name:"waitee" chan ()
  in
  let waitee_in, waiter_out = pipe () in
  let waiter_in, waitee_out = pipe () in
  let waitee = waitee (waitee_in, waitee_out) in
  let waiter = waiter (waiter_in, waiter_out) in
  let initial_request () =
    let request =
      Jsonrpc.Message.create ~id:(`String "initial") ~method_:"init" ()
    in
    print_endline "initial: waitee requests from waiter";
    let+ resp = Jrpc.request waitee request in
    print_endline "initial request response:";
    print (Response resp)
  in
  let all = [ Jrpc.run waiter; Jrpc.run waitee ] in
  let all = initial_request () :: all in
  let run () = Fiber.parallel_iter all ~f:Fun.id in
  Fiber_test.test Dyn.Encoder.opaque (run ());
  [%expect
    {|
    initial: waitee requests from waiter
    waiter: received request
    { "id": "initial", "method": "init", "jsonrpc": "2.0" }
    waiter: making request
    waitee: received request
    { "id": 100, "method": "shutdown", "jsonrpc": "2.0" }
    waitee: stopping
    waiter: received response:
    { "id": 100, "jsonrpc": "2.0", "result": 42 }
    waiter: stopping
    [FAIL] unexpected Never raised |}]

let%expect_test "test from jsonrpc_test.ml" =
  Printexc.record_backtrace false;
  let response =
    let i = ref 0 in
    fun () ->
      incr i;
      `Int !i
  in
  let on_request ctx =
    let req = Context.message ctx in
    let state = Context.state ctx in
    Fiber.return (Reply.now (Jsonrpc.Response.ok req.id (response ())), state)
  in
  let on_notification ctx =
    let n = Context.message ctx in
    if n.method_ = "raise" then failwith "special failure";
    let json = Message.yojson_of_notification n in
    print_endline ">> received notification";
    print_json json;
    Fiber.return (Jsonrpc_fiber.Notify.Continue, ())
  in
  let responses = ref [] in
  let initial_requests =
    let request ?params id method_ =
      Jsonrpc.Message.create ?params ~id:(Some id) ~method_ ()
    in
    let notification ?params method_ =
      Jsonrpc.Message.create ~id:None ?params ~method_ ()
    in
    [ Message (request (`Int 10) "foo")
    ; Message (request (`String "testing") "bar")
    ; Message (notification "notif1")
    ; Message (notification "notif2")
    ; Message (notification "raise")
    ]
  in
  let reqs_in, reqs_out = pipe () in
  let chan =
    let out = Out.of_ref responses in
    (reqs_in, out)
  in
  let session = Jrpc.create ~on_notification ~on_request ~name:"test" chan () in
  let write_reqs () =
    let open Fiber.O in
    let* () =
      Fiber.sequential_iter initial_requests ~f:(fun req ->
          Out.write reqs_out (Some req))
    in
    Out.write reqs_out None
  in
  Fiber_test.test Dyn.Encoder.opaque
    (Fiber.fork_and_join_unit write_reqs (fun () -> Jrpc.run session));
  List.rev !responses
  |> List.iter ~f:(fun packet ->
         let json = Jsonrpc.yojson_of_packet packet in
         print_json json);
  [%expect
    {|
    >> received notification
    { "method": "notif1", "jsonrpc": "2.0" }
    >> received notification
    { "method": "notif2", "jsonrpc": "2.0" }
    Uncaught error when handling notification:
    { "method": "raise", "jsonrpc": "2.0" }
    Error:
    [ { exn = "(Failure \"special failure\")"; backtrace = "" } ]
    "<opaque>"
    { "id": "testing", "jsonrpc": "2.0", "result": 2 }
    { "id": 10, "jsonrpc": "2.0", "result": 1 } |}]
