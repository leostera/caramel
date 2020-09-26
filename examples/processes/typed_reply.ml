let rec a_loop pid recv i =
  match recv ~timeout:Process.Infinity with
  | None -> a_loop pid recv i
  | Some `Noop  -> a_loop pid recv i
  | Some (`Call (cb, msg)) ->
      Erlang.send cb i;
      a_loop pid recv (i + msg)

let rec b_loop pid recv a =
  Timer.sleep 1000;
  Erlang.send a (`Call (pid, 1));
  match recv ~timeout:(Process.Bounded 0) with
  | None -> b_loop pid recv a
  | Some 11 -> Io.format "aaadn i'm out\n" []
  | Some i ->
      Io.format "received: ~p, state must be ~p\n" [ i; i + 1 ];
      b_loop pid recv a

let rec c_loop pid recv a =
  Timer.sleep 1000;
  (* contramap this process to map ints to booleans *)
  let cb = Process.contramap (fun x -> x > 10) pid in
  Erlang.send a (`Call (cb, 1));
  match recv ~timeout:(Process.Bounded 0) with
  | None -> c_loop pid recv a
  | Some true -> Io.format "yay, it is true\n" []
  | Some false ->
      Io.format "oh noes!\n" [];
      c_loop pid recv a

let run () =
  let a = Process.make (fun pid recv -> a_loop pid recv 0) in
  let _b = Process.make (fun pid recv -> b_loop pid recv a) in
  let _c = Process.make (fun pid recv -> c_loop pid recv a) in
  ()
