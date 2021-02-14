module Barrier = Fiber_unix.Private.Barrier

let () = Printexc.record_backtrace false

let print_result x =
  print_endline
    (match x with
    | Ok () -> "ok"
    | Error (`Closed (`Read b)) -> Printf.sprintf "closed with read: %b" b
    | Error `Timeout -> "timeout")

let print_signal x =
  match x with
  | Ok () -> ()
  | Error `Closed -> print_endline "signal failed: closed"

let%expect_test "create & close" =
  let b = Barrier.create () in
  Barrier.close b;
  [%expect {||}]

let%expect_test "write" =
  let b = Barrier.create () in
  print_signal (Barrier.signal b);
  Barrier.close b;
  [%expect {||}]

let%expect_test "timeout" =
  let b = Barrier.create () in
  print_result (Barrier.await b ~timeout:0.1);
  Barrier.close b;
  [%expect {|
    timeout |}]

let%expect_test "read and write" =
  let b = Barrier.create () in
  for _ = 1 to 5 do
    print_signal (Barrier.signal b)
  done;
  print_result (Barrier.await b ~timeout:0.1);
  print_result (Barrier.await b ~timeout:0.1);
  Barrier.close b;
  [%expect {|
    ok
    timeout |}]

let%expect_test "read/write subsequent" =
  let b = Barrier.create () in
  for _ = 1 to 5 do
    print_signal (Barrier.signal b);
    print_result (Barrier.await b ~timeout:0.1)
  done;
  Barrier.close b;
  [%expect {|
    ok
    ok
    ok
    ok
    ok |}]

let%expect_test "await after close" =
  let b = Barrier.create () in
  Barrier.close b;
  print_result (Barrier.await b ~timeout:0.1);
  [%expect {| closed with read: false |}]

let%expect_test "write after close" =
  let b = Barrier.create () in
  Barrier.close b;
  print_signal (Barrier.signal b);
  [%expect {| signal failed: closed |}]

(** this test does not work as expected on linux

    {[
      let%expect_test "close without signal" =
        let b = Barrier.create () in
        let (_ : Thread.t) =
          Thread.create
            (fun () ->
              Unix.sleepf 0.5;
              print_endline "closing barrier";
              Barrier.close b)
            ()
        in
        print_endline "awaiting barrier";
        print_result (Barrier.await b ~timeout:10.0);
        [%expect {|
awaiting barrier
closing barrier
closed with read: false |}]
    ]} *)
