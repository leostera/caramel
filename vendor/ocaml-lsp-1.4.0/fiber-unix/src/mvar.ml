open Import

type 'a t =
  { m : Mutex.t
  ; cv : Condition.t
  ; mutable cell : 'a option
  }

let create () = { m = Mutex.create (); cv = Condition.create (); cell = None }

let get t =
  let rec await_value t =
    match t.cell with
    | None ->
      Condition.wait t.cv t.m;
      await_value t
    | Some v ->
      t.cell <- None;
      v
  in
  with_mutex t.m ~f:(fun () -> await_value t)

let set t v =
  with_mutex t.m ~f:(fun () -> t.cell <- Some v);
  Condition.signal t.cv
