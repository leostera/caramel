type name = ..

type registry = name list

let registry : registry ref = ref []

let where_is f =
  let rec where_is' ns =
    match ns with
    | [] -> None
    | n :: ns -> ( match f n with Some t -> Some t | None -> where_is' ns )
  in
  where_is' registry.contents

let register f n pid =
  match where_is f with
  | Some _ -> Error "process already registered"
  | None ->
      registry := n :: registry.contents;
      Ok ()
