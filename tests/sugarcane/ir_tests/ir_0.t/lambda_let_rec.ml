(* recursion *)
let rec f () = f ()

(* co-recursion *)
let rec a () = b ()
and b () = a ()

(* recursion within functions *)
let with_aux () =
  let rec aux () = aux () in
  aux ()

(* co-recursion within functions *)
let g () =
  let rec a () = b ()
  and b () = a () in
  b ()
