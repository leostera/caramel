  $ echo "" > test.caramel
  $ cat test.caramel
  
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] 

  $ echo -e "\n" > test.caramel
  $ cat test.caramel
  
  
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] 

  $ echo -e "type t = | Print { message: string }" > test.caramel
  $ cat test.caramel
  type t = | Print { message: string }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] type t =
                     | Print of {
                     message: string } 

  $ echo -e "type t = | Print { message: string, }" > test.caramel
  $ cat test.caramel
  type t = | Print { message: string, }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] type t =
                     | Print of {
                     message: string } 

  $ echo -e "type t = | Print { message: string, other: field }" > test.caramel
  $ cat test.caramel
  type t = | Print { message: string, other: field }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] type t =
                     | Print of {
                     message: string ;
                     other: field } 

  $ echo -e "type t = | Print { message: string, other: field, and_third_field: no_way, }" > test.caramel
  $ cat test.caramel
  type t = | Print { message: string, other: field, and_third_field: no_way, }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] type t =
                     | Print of
                     {
                     message: string ;
                     other: field ;
                     and_third_field: no_way } 

  $ echo -e "type t =\n\t| Print (string, int, bool)\n\t| Write { file: File.t, contents: string}" > test.caramel
  $ cat test.caramel
  type t =
  	| Print (string, int, bool)
  	| Write { file: File.t, contents: string}
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] type t =
                     | Print of string * int * bool 
                     | Write of {
                     file: File.t ;
                     contents: string } 

  $ echo -e "type t = | Print" > test.caramel
  $ cat test.caramel
  type t = | Print
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] type t =
                     | Print 

  $ echo -e "type t = | Print\ntype t2 = | Write" > test.caramel
  $ cat test.caramel
  type t = | Print
  type t2 = | Write
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] type t =
                     | Print 
                   type t2 =
                     | Write 

  $ echo -e "@clipper\ntype t = | Print" > test.caramel
  $ cat test.caramel
  @clipper
  type t = | Print
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] type t =
                     | Print 

  $ echo -e "@clipper(name)\ntype t = | Print" > test.caramel
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] type t =
                     | Print 

  $ echo -e "@clipper(\n\tname = \"stuff\"\n)\ntype t = | Print" > test.caramel
  $ cat test.caramel
  @clipper(
  	name = "stuff"
  )
  type t = | Print
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] type t =
                     | Print 

  $ echo -e "@clipper(\n\tname = \"stuff\",\n\tdescription = \"a cli for stuff!\"\n)\ntype t = | Print" > test.caramel
  $ cat test.caramel
  @clipper(
  	name = "stuff",
  	description = "a cli for stuff!"
  )
  type t = | Print
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] type t =
                     | Print 

  $ echo -e "type t = | Print { @clipper(short = \"m\") message: string }" > test.caramel
  $ cat test.caramel
  type t = | Print { @clipper(short = "m") message: string }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] type t =
                     | Print of {
                     message: string } 

  $ echo -e "fn hello() { joe }" > test.caramel
  $ cat test.caramel
  fn hello() { joe }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec hello () = joe

  $ echo -e "pub fn hello() { :joe }" > test.caramel
  $ cat test.caramel
  pub fn hello() { :joe }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec hello () = `joe

  $ echo -e "pub fn hello(_my, _good) { :joe }" > test.caramel
  $ cat test.caramel
  pub fn hello(_my, _good) { :joe }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec hello _my _good = `joe

  $ echo -e "pub fn fst(x, _) { x }" > test.caramel
  $ cat test.caramel
  pub fn fst(x, _) { x }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec fst x _ = x

  $ echo -e "pub fn snd(_, y) { y }" > test.caramel
  $ cat test.caramel
  pub fn snd(_, y) { y }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec snd _ y = y

  $ echo -e "pub fn pair(x, y) { (x, y) }" > test.caramel
  $ cat test.caramel
  pub fn pair(x, y) { (x, y) }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec pair x y = (x, y)

  $ echo -e "pub fn double_pair(x, y) { ((x, x), (y, y)) }" > test.caramel
  $ cat test.caramel
  pub fn double_pair(x, y) { ((x, x), (y, y)) }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec double_pair x y = ((x, x), (y, y))

  $ echo -e "pub fn pair_list(x, y) { [x, y] }" > test.caramel
  $ cat test.caramel
  pub fn pair_list(x, y) { [x, y] }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec pair_list x y = [x; y]

  $ echo -e "pub fn cons(x, y) { [x, ...y] }" > test.caramel
  $ cat test.caramel
  pub fn cons(x, y) { [x, ...y] }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec cons x y = x :: y

  $ echo -e "pub fn cons_and_splat(x, y) { [x, x, x, ...y] }" > test.caramel
  $ cat test.caramel
  pub fn cons_and_splat(x, y) { [x, x, x, ...y] }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec cons_and_splat x y = x :: x :: x :: y

  $ echo -e "pub fn cons_pairs(x, y) { [(x, x), ...y] }" > test.caramel
  $ cat test.caramel
  pub fn cons_pairs(x, y) { [(x, x), ...y] }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec cons_pairs x y = (x, x) :: y

  $ echo -e "pub fn f() {\n  match n {\n  | [] -> :empty\n  | _ -> :non_empty\n  }\n}" > test.caramel
  $ cat test.caramel
  pub fn f() {
    match n {
    | [] -> :empty
    | _ -> :non_empty
    }
  }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec f () = match n with | [] -> `empty | _ -> `non_empty

  $ echo -e "pub fn f() {\n  match n {\n  | () -> :empty\n  | (_, (_, _)) -> :non_empty\n  }\n}" > test.caramel
  $ cat test.caramel
  pub fn f() {
    match n {
    | () -> :empty
    | (_, (_, _)) -> :non_empty
    }
  }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec f () =
                     match n with | () -> `empty | (_, (_, _)) -> `non_empty

  $ echo -e "pub fn f() {\n  match n {\n  | [(x, _), (y, _), ...z] -> :empty\n  | (a, (b, c)) -> :non_empty\n  }\n}" > test.caramel
  $ cat test.caramel
  pub fn f() {
    match n {
    | [(x, _), (y, _), ...z] -> :empty
    | (a, (b, c)) -> :non_empty
    }
  }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec f () =
                     match n with
                     | (x, _)::(y, _)::z -> `empty
                     | (a, (b, c)) -> `non_empty

  $ echo -e "pub fn fix(n) {\n  fix(n)\n}" > test.caramel
  $ cat test.caramel
  pub fn fix(n) {
    fix(n)
  }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec fix n = fix n

  $ echo -e "pub fn fix(n) {\n  fix(n);\n  fix(n);\n  fix(n)\n}" > test.caramel
  $ cat test.caramel
  pub fn fix(n) {
    fix(n);
    fix(n);
    fix(n)
  }
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] let rec fix n = fix n; fix n; fix n

  $ echo -e "external f: unit -> 'a = \"hello\"" > test.caramel
  $ cat test.caramel
  external f: unit -> 'a = "hello"
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] external f : unit -> 'a = "hello"

  $ echo -e "external format: string -> list<'a> -> unit = \"io:format\"" > test.caramel
  $ cat test.caramel
  external format: string -> list<'a> -> unit = "io:format"
  $ caramel parse --file test.caramel --dump-caml --debug
  caramel: [DEBUG] external format : string -> 'a list -> unit = "io:format"
