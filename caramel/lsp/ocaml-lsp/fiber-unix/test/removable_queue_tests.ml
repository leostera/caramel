open! Import
module Q = Fiber_unix.Private.Removable_queue

let printf = Printf.printf

let print_all q f =
  while not (Q.is_empty q) do
    let elem = Option.value_exn (Q.pop q) in
    printf "elem: %s\n" (f elem)
  done

let add_all q xs = List.iter xs ~f:(fun x -> ignore (Q.push q x))

let%expect_test "create is empty" =
  let q = Q.create () in
  printf "new queue is empty: %b" (Q.is_empty q);
  [%expect {| new queue is empty: true |}]

let%expect_test "pushing adds an element" =
  let q = Q.create () in
  let (_ : int Q.node) = Q.push q 100 in
  printf "empty: %b\n" (Q.is_empty q);
  [%expect {| empty: false |}]

let%expect_test "remove works with a 1 element queue" =
  let q = Q.create () in
  let node = Q.push q 100 in
  printf "empty: %b\n" (Q.is_empty q);
  Q.remove node;
  printf "empty: %b\n" (Q.is_empty q);
  [%expect {|
    empty: false
    empty: true |}]

let%expect_test "remove works with a 1 element queue" =
  let q = Q.create () in
  let (_ : int Q.node) = Q.push q 100 in
  printf "empty: %b\n" (Q.is_empty q);
  print_all q Int.to_string;
  printf "empty: %b\n" (Q.is_empty q);
  [%expect {|
    empty: false
    elem: 100
    empty: true |}]

let%expect_test "deque all" =
  let q = Q.create () in
  add_all q [ 1; 2; 3 ];
  print_all q Int.to_string;
  [%expect {|
    elem: 1
    elem: 2
    elem: 3 |}]

let%expect_test "remove in the middle" =
  let q = Q.create () in
  add_all q [ 100; 200 ];
  let elem = Q.push q 300 in
  add_all q [ 400 ];
  Q.remove elem;
  print_all q Int.to_string;
  [%expect {|
    elem: 100
    elem: 200
    elem: 400 |}]
