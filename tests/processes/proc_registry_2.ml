type name = ..

type registry = name list

let registry : registry ref = ref []

let where_is f =
  let rec find' ns =
    match ns with
    | [] -> None
    | n :: ns -> ( match f n with Some t -> Some t | None -> find' ns )
  in
  find' registry.contents

let register f n pid =
  match find f with
  | Some _ -> Error "process already registered"
  | None ->
      registry := n :: registry.contents;
      Ok ()
