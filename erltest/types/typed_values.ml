open Type_defs

let ok x = Ok x
let err x = Error x

let nil () = Nil
let cons x xs = Cons (x, xs)

let some x = Some x
let none () = None

let poly x = `Poly x
