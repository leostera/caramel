open Import

type 'a node =
  { mutable next : 'a node option
  ; mutable prev : 'a node option
  ; data : 'a
  ; mutable queue : 'a t option
  }

and 'a non_empty =
  { tail : 'a node
  ; head : 'a node
  }

and 'a queue =
  | Empty
  | Non_empty of 'a non_empty

and 'a t = 'a queue ref

let create () = ref Empty

let is_empty t = !t = Empty

let push (type a) (t : a t) (a : a) =
  let node prev = { next = None; prev; data = a; queue = Some t } in
  match !t with
  | Empty ->
    let node = node None in
    t := Non_empty { tail = node; head = node };
    node
  | Non_empty ne ->
    let node = node (Some ne.tail) in
    assert (ne.tail.next = None);
    ne.tail.next <- Some node;
    t := Non_empty { ne with tail = node };
    node

let pop (type a) (t : a t) : a option =
  match !t with
  | Empty -> None
  | Non_empty ne ->
    let node = ne.head in
    if Option.is_none node.next then
      t := Empty
    else
      t := Non_empty { ne with head = Option.value_exn node.next };
    node.prev <- None;
    node.next <- None;
    node.queue <- None;
    Some node.data

let remove node =
  if Option.is_some node.queue then (
    (match (node.next, node.prev) with
    | None, None -> Option.value_exn node.queue := Empty
    | _, _ -> (
      (match node.next with
      | None -> ()
      | Some next -> next.prev <- node.prev);
      match node.prev with
      | None -> ()
      | Some prev -> prev.next <- node.next));
    node.prev <- None;
    node.next <- None;
    node.queue <- None
  )
