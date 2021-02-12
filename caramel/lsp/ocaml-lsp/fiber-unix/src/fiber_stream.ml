module In = struct
  type 'a t = unit -> 'a option Fiber.t

  let create f = f

  let read t = t ()

  let empty () () = Fiber.return None

  let of_list xs =
    let xs = ref xs in
    fun () ->
      match !xs with
      | [] -> Fiber.return None
      | x :: xs' ->
        xs := xs';
        Fiber.return (Some x)

  let rec filter_map t ~f () =
    let open Fiber.O in
    let* next = read t in
    match next with
    | None -> Fiber.return None
    | Some x -> (
      match f x with
      | None -> filter_map t ~f ()
      | Some y -> Fiber.return (Some y))

  let map t ~f = filter_map t ~f:(fun x -> Some (f x))

  let filter (type a) (t : a t) ~f : a t =
    filter_map t ~f:(fun x ->
        if f x then
          Some x
        else
          None)

  let rec sequential_iter t ~f =
    let open Fiber.O in
    let* e = t () in
    match e with
    | None -> Fiber.return ()
    | Some x ->
      let* () = f x in
      sequential_iter t ~f
end

module Out = struct
  type 'a t = 'a option -> unit Fiber.t

  let create f = f

  let write f x = f x

  let of_ref ref = function
    | None -> Fiber.return ()
    | Some x -> Fiber.return (ref := x :: !ref)

  let null () _ = Fiber.return ()
end

let connect i o =
  let open Fiber.O in
  let rec go () =
    let* a = In.read i in
    let* () = Out.write o a in
    match a with
    | None -> Fiber.return ()
    | Some _ -> go ()
  in
  go ()

let supply i o =
  let open Fiber.O in
  let rec go () =
    let* a = In.read i in
    match a with
    | None -> Fiber.return ()
    | Some _ ->
      let* () = Out.write o a in
      go ()
  in
  go ()

let pipe () =
  let mvar = Fiber.Mvar.create () in
  let i = In.create (fun () -> Fiber.Mvar.read mvar) in
  let o = Out.create (fun x -> Fiber.Mvar.write mvar x) in
  (i, o)
