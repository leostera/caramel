module Proc = struct
  type t = int Erlang.process

  let name () = `Worker_pool

  (* NOTE: Process registry boilerplate. We only need to specify
   * the type of our process once in this function to keep things
   * safe.
   *)
  let where_is () : t option = Proc_registry.where_is (name ())

  (* NOTE: Actual process loop here *)
  let rec a_loop pid recv i =
    match recv ~timeout:Process.Infinity with
    | None -> a_loop pid recv i
    | Some j ->
        Io.format "recv: ~p\n" [ j ];
        a_loop pid recv (i + j)

  let start () =
    let loop pid recv = a_loop pid recv 0 in
    let pid = Process.make loop in
    Proc_registry.register (name ()) pid
end

let run () =
  (* NOTE: in practice you can force handling of all cases for both
   * start and where_is here *)
  let (Ok pid) = Proc.start () in
  let (Some pid2) = Proc.where_is () in
  Erlang.send pid 1;
  Erlang.send pid2 1

(* Erlang.send pid2 "hello"; *)
