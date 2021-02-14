open Import
open Fiber.O

type mvar =
  | Done
  | Task of (unit -> unit Fiber.t)

type status =
  | Open
  | Closed

type t =
  { mvar : mvar Fiber.Mvar.t
  ; mutable status : status
  }

let create () = { mvar = Fiber.Mvar.create (); status = Open }

let task t ~f =
  match t.status with
  | Closed -> Fiber.return (Error `Stopped)
  | Open ->
    let+ () = Fiber.Mvar.write t.mvar (Task f) in
    Ok ()

let task_exn t ~f =
  let+ res = task t ~f in
  match res with
  | Ok () -> ()
  | Error `Stopped -> Code_error.raise "detached already closed" []

let rec sequence t =
  let+ next = Fiber.Mvar.read t.mvar in
  match next with
  | Done -> Fiber.Sequence.Nil
  | Task task -> Cons (task, sequence t)

let check_open t =
  if t.status = Closed then Code_error.raise "detached already closed" []

let stop t =
  check_open t;
  let+ () = Fiber.Mvar.write t.mvar Done in
  check_open t;
  t.status <- Closed

let run t = sequence t |> Fiber.Sequence.parallel_iter ~f:(fun task -> task ())
