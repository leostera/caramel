type 'a list = Cons of ('a * 'a list) | Nil

type 'value tree = Leaf of 'value * 'value | Node of 'value * 'value tree
