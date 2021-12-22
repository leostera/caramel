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
