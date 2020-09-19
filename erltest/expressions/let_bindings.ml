let let_one () =
  let a = 1 in
  a

let let_ignore () =
  let _ = 1 in
  2

let let_many () =
  let a = 1 in
  let b = 2 in
  let c = 3 in
  let d = 4 in
  a + b + c + d

let let_nested f g h =
  let a =
    g ();
    (* <-- a wild side-effect appears! D: *)
    let b =
      h ();
      let c = 1 in
      c + 1
    in
    b + 1
  in
  f a

(* FIXME: Erlang does not support let rec bindings, and we're forced to use a
   combinator instead.

   Right now it believes that the name is supposed to be a module level
   function, which compiles fine but crashes at runtime.

   So this:
    let rec f x = f (x + 1) in
    f 0

   could look like:

     let_rec() ->
       RecF = fun(G) -> fun (X) -> (G(G))(X+1) end end,
       F = RecF(RecF),
       F(0).

   alternatively we coudl wrap this in a support library too

     F = caramel:letrec1(fun (F, X) -> F(X + 1) end)

 *)
let let_rec () =
  let rec f x = f (x + 1) in
  f 0
