open Import
module Fiber_stream = Fiber_unix.Fiber_stream
open Fiber_stream

let failing_fiber () : unit Fiber.t =
  Scheduler.yield () >>= fun () -> raise Exit

let long_running_fiber () =
  let rec loop n =
    if n = 0 then
      Fiber.return ()
    else
      Scheduler.yield () >>= fun () -> loop (n - 1)
  in
  loop 10

let%expect_test "of_list & iter" =
  test unit
    (let i = In.of_list [ 1; 2; 3 ] in
     In.sequential_iter i ~f:(fun i -> Fiber.return (print_int i)));
  [%expect {| 123() |}]

let%expect_test "pipe" =
  test unit
    (let i, o = Fiber_stream.pipe () in
     Fiber.fork_and_join_unit
       (fun () ->
         let* () =
           Fiber.sequential_iter [ 1; 2; 3 ] ~f:(fun i ->
               printf "writing %d\n" i;
               Out.write o (Some i))
         in
         print_endline "closing stream";
         Out.write o None)
       (fun () ->
         let* () =
           In.sequential_iter i ~f:(fun i ->
               printf "reading %d\n" i;
               Fiber.return ())
         in
         print_endline "finished reading";
         Fiber.return ()));
  [%expect
    {|
    writing 1
    writing 2
    reading 1
    reading 2
    writing 3
    closing stream
    reading 3
    finished reading
    () |}]

let writer f =
  Out.create (function
    | None ->
      print_endline "closed out";
      Fiber.return ()
    | Some x ->
      printf "writing %s\n" (f x);
      Fiber.return ())

let%expect_test "connect" =
  test unit
    (let i = In.of_list [ 1; 2; 3 ] in
     let o = writer string_of_int in
     let+ () = Fiber_stream.connect i o in
     print_endline "connect finished");
  [%expect
    {|
    writing 1
    writing 2
    writing 3
    closed out
    connect finished
    () |}]

let%expect_test "supply" =
  test unit
    (let i = In.of_list [ 1; 2; 3 ] in
     let o = writer string_of_int in
     let* () = Fiber_stream.supply i o in
     print_endline "suply finished, but not closed. manually closing";
     Out.write o None);
  [%expect
    {|
    writing 1
    writing 2
    writing 3
    suply finished, but not closed. manually closing
    closed out
    () |}]
