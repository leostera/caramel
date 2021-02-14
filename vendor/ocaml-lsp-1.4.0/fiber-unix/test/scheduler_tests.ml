open! Import
open Fiber.O
module S = Fiber_unix.Scheduler
module Fiber_detached = Fiber_unix.Fiber_detached

let test s f =
  let f =
    Fiber.with_error_handler f ~on_error:(fun exn ->
        Format.printf "%a@." Exn_with_backtrace.pp_uncaught exn)
  in
  S.run s f

let%expect_test "scheduler starts and runs a fiber" =
  let s = S.create () in
  S.run s
    (Fiber.of_thunk (fun () ->
         print_endline "running";
         Fiber.return ()));
  [%expect {|
    running |}]

let%expect_test "run an async task and wait it for it to finish" =
  let s = S.create () in
  let th = S.create_thread s in
  let async () =
    S.async_exn th (fun () -> print_endline "running in a different thread")
  in
  let run () =
    print_endline "running in scheduler";
    let task = async () in
    let+ res = S.await_no_cancel task in
    match res with
    | Error _ -> assert false
    | Ok () ->
      print_endline "finished running task";
      S.stop th;
      print_endline "stopped thread"
  in
  S.run s (Fiber.of_thunk run);
  [%expect
    {|
    running in scheduler
    running in a different thread
    finished running task
    stopped thread |}]

let%expect_test "create timer & schedule task" =
  let s = S.create () in
  let timer = S.create_timer s ~delay:0.001 in
  let run () =
    print_endline "scheduling timer";
    let+ res =
      S.schedule timer (fun () ->
          print_endline "timer running";
          Fiber.return ())
    in
    match res with
    | Error `Cancelled -> assert false
    | Ok () -> print_endline "timer done"
  in
  test s run;
  [%expect {|
    scheduling timer
    timer running
    timer done |}]

let%expect_test "create timer & schedule task & cancel it" =
  let s = S.create () in
  let timer = S.create_timer s ~delay:3.0 in
  let run () =
    print_endline "scheduling timer";
    let task =
      S.schedule timer (fun () ->
          print_endline "timer running";
          Fiber.return ())
    in
    Fiber.fork_and_join_unit
      (fun () ->
        let+ res = task in
        match res with
        | Error `Cancelled -> print_endline "timer cancelled successfully"
        | Ok () -> assert false)
      (fun () -> S.cancel_timer timer)
  in
  test s run;
  [%expect {|
    scheduling timer
    timer cancelled successfully |}]

let%expect_test "create multiple timers" =
  let s = S.create () in
  let timer () = S.create_timer s ~delay:0.001 in
  let run () =
    Fiber.sequential_iter [ 1; 2; 3 ] ~f:(fun i ->
        let timer = timer () in
        printf "%d: scheduling timer\n" i;
        let+ res =
          S.schedule timer (fun () ->
              printf "%d: timer running\n" i;
              Fiber.return ())
        in
        match res with
        | Error `Cancelled -> assert false
        | Ok () -> printf "%d: timer done\n" i)
  in
  test s run;
  [%expect
    {|
    1: scheduling timer
    1: timer running
    1: timer done
    2: scheduling timer
    2: timer running
    2: timer done
    3: scheduling timer
    3: timer running
    3: timer done |}]

let%expect_test "create multiple timers" =
  let s = S.create () in
  let timer () = S.create_timer s ~delay:0.001 in
  let run () =
    let counter = ref 0 in
    let+ () =
      Fiber.parallel_iter [ 1; 2; 3 ] ~f:(fun _ ->
          let timer = timer () in
          let+ res =
            S.schedule timer (fun () ->
                printf "timer. ";
                Fiber.return ())
          in
          match res with
          | Error `Cancelled -> assert false
          | Ok () -> incr counter)
    in
    printf "counter: %d\n" !counter
  in
  test s run;
  [%expect {|
    timer. timer. timer. counter: 3 |}]

let%expect_test "tests rescheduling" =
  let s = S.create () in
  let timer = S.create_timer s ~delay:0.05 in
  let run () =
    let counter = ref 0 in
    let+ () =
      Fiber.parallel_iter [ 1; 2; 3 ] ~f:(fun _ ->
          let+ res =
            S.schedule timer (fun () ->
                printf "timer. ";
                Fiber.return ())
          in
          match res with
          | Error `Cancelled -> printf "cancel. "
          | Ok () -> incr counter)
    in
    printf "counter: %d\n" !counter
  in
  test s run;
  [%expect {| cancel. cancel. timer. counter: 1 |}]

let%expect_test "detached + timer" =
  let s = S.create () in
  let detached = Fiber_detached.create () in
  let timer = S.create_timer s ~delay:0.05 in
  let run () =
    Fiber.fork_and_join_unit
      (fun () ->
        let* () =
          Fiber_detached.task_exn detached ~f:(fun () ->
              let* res =
                S.schedule timer (fun () ->
                    print_endline "inside timer";
                    Fiber.return ())
              in
              match res with
              | Ok () -> Fiber.return (print_endline "timer finished")
              | Error `Cancelled -> assert false)
        in
        Fiber_detached.stop detached)
      (fun () -> Fiber_detached.run detached)
  in
  test s run;
  [%expect {|
    inside timer
    timer finished |}]

let%expect_test "multiple timers" =
  let s = S.create () in
  let timer delay = S.create_timer s ~delay in
  let run () =
    [ timer 0.06; timer 0.03; timer 0.01 ]
    |> List.mapi ~f:(fun i timer ->
           let+ res =
             S.schedule timer (fun () ->
                 printf "timer %d\n" i;
                 Fiber.return ())
           in
           match res with
           | Error `Cancelled -> assert false
           | Ok () -> ())
    |> Fiber.parallel_iter ~f:Fun.id
  in
  test s run;
  [%expect {|
    timer 2
    timer 1
    timer 0 |}]
