open Import
open Fiber_unix

let%expect_test "start & stop" =
  test unit
    (let detached = Fiber_detached.create () in
     Fiber_detached.stop detached);
  [%expect {| () |}]

let%expect_test "run 2 task" =
  test unit
    (let detached = Fiber_detached.create () in
     let task n () =
       printf "task %d\n" n;
       Fiber.return ()
     in
     let tasks () =
       Fiber.parallel_iter [ 1; 2 ] ~f:(fun n ->
           Fiber_detached.task_exn detached ~f:(task n))
     in
     Fiber.fork_and_join_unit
       (fun () -> Fiber_detached.run detached)
       (fun () ->
         let* () = tasks () in
         Fiber_detached.stop detached));
  [%expect {|
    task 1
    task 2
    () |}]
