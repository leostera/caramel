type 'a a_list = Cons of ('a * 'a a_list) | Nil

type 'value tree = Leaf of 'value * 'value | Node of 'value * 'value tree
