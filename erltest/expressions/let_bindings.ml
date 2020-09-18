let let_one () = let a = 1 in a

let let_ignore () = let _ = 1 in 2

let let_many () =
  let a = 1 in
  let b = 2 in
  let c = 3 in
  let d = 4 in
  a + b + c + d


(* FIXME: Erlang doesn't have let scopes like OCaml does, so we can work
   around this by flatten it out the expressions from the inside out and
   reverting the order.

     let a =
       let b =
         let c = 1 in
         c + 1
       in b + 1
     in f a

   would be come:

     C = 1,
     B = C + 1,
     A = B + 1,
     F(A)

   However, this does not guarantee that we will preserve the semantics
   because in between things, side effects could lurk:

     let a =
       g (); <-- a wild side-effect appears! D:
       let b =
         h (); <-- this one actually _depends_ on g having run! D:
         let c = 1 in
         c + 1
       in b + 1
     in f a

    This would translate to:

    H(),
    C = 1,
    B = C + 1
    G(),
    A = B + 1,
    F(A)

    So there's that. Might be a good idea to only allow more let's in the right
    hand side, as this would force you to write a function instead.

    The last option here would be to automatically turn a let binding into a
    arity 0 function that is immediately invoked.


  FIXME: for now this should generate the following code:

    A = (fun () ->
      G(),
      B = (fun () ->
        H(),
        C = 1,
        C + 1
      end)(),
      B + 1
    end)(),
    F(A)

 *)
let let_nested f g h =
  let a =
    g (); (* <-- a wild side-effect appears! D: *)
    let b =
      h ();
     let c = 1 in
     c + 1
    in b + 1
  in f a


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

