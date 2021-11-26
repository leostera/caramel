/* FIXME: this should compile to: fun() -> ok
 * since ok is the "unit" or Erlang for return calls
 */
type ignore = unit => unit;

/* FIXME: this should compile to: fun() -> A
 * since "unit" should go away if its the only argument
 */
type defer('a) = unit => 'a;

let ignore: 'a => ignore;

let add: (int, int) => int;

let add_slow: (int, int) => defer(int);

let add_really_slow: int => defer(int => defer(int));
