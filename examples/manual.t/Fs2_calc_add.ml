let main (a :: b :: _) =

  (* First we will try to convert our strings to integers *)
  let a_int = Erlang.list_to_integer a in
  let b_int = Erlang.list_to_integer b in

  (* Then we will add them both together *)
  let result = a_int + b_int in

  (* Finally we print out the results *)
  Io.format "~p + ~p = ~p\n" [a_int; b_int; result]
