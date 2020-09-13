
type 'a predicate = 'a -> bool

type add = int -> int -> int

type ('a, 'b) f = 'a -> 'b -> bool

type nested = unit -> (int -> bool) -> string

type ('a, 'b) f_with_tuples = ('a * 'b) -> unit -> bool

type 'a r = {
  f: unit -> 'a * int
}
