  $ echo "" > test.caramel 
  $ cat test.caramel
  
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] ()

  $ echo -e "\n" > test.caramel
  $ cat test.caramel
  
  
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] ()

  $ echo -e "@(\n)" > test.caramel
  $ cat test.caramel
  @(
  )
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At Parens_left Parens_right)

  $ echo "@()" > test.caramel
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At Parens_left Parens_right)

  $ echo "@(a)" > test.caramel
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At Parens_left (Id a) Parens_right)

  $ echo "@(ab)" > test.caramel
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At Parens_left (Id ab) Parens_right)

  $ echo "@(abcdefghijklmno)" > test.caramel
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At Parens_left (Id abcdefghijklmno) Parens_right)

  $ echo "@(AbcDefGhijKlmNO)" > test.caramel
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At Parens_left (Id AbcDefGhijKlmNO) Parens_right)

  $ echo "@( )" > test.caramel
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At Parens_left Parens_right)

  $ echo "@(a )" > test.caramel
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At Parens_left (Id a) Parens_right)

  $ echo "@(ab )" > test.caramel
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At Parens_left (Id ab) Parens_right)

  $ echo -e "@(a \nb )" > test.caramel
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At Parens_left (Id a) (Id b) Parens_right)

  $ echo -e "@(abcdefg    \n hijklmno \n)" > test.caramel
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At Parens_left (Id abcdefg) (Id hijklmno) Parens_right)

  $ echo -e "@(Abc  Def\nGhijKlm  \n  NO)" > test.caramel
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At Parens_left (Id Abc) (Id Def) (Id GhijKlm) (Id NO)
                     Parens_right)

  $ echo "@clipper()" > test.caramel
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At (Id clipper) Parens_left Parens_right)

  $ echo "@clipper(name)" > test.caramel
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At (Id clipper) Parens_left (Id name) Parens_right)

  $ echo "@clipper(name = )" > test.caramel
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At (Id clipper) Parens_left (Id name) Equal Parens_right)

  $ echo -e "@clipper(\n\tname = \"stuff\"\n)" > test.caramel
  $ cat test.caramel
  @clipper(
  	name = "stuff"
  )
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At (Id clipper) Parens_left (Id name) Equal (String stuff)
                     Parens_right)

  $ echo -e "@clipper(name = \"stuff\" \"and\" (\"more stuff\"))" > test.caramel
  $ cat test.caramel
  @clipper(name = "stuff" "and" ("more stuff"))
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At (Id clipper) Parens_left (Id name) Equal (String stuff)
                     (String and) Parens_left (String "more stuff")
                     Parens_right Parens_right)

  $ echo -e "@clipper(\n\tname = \"stuff\",\n\tdescription = \"a cli for stuff!\"\n)" > test.caramel
  $ cat test.caramel
  @clipper(
  	name = "stuff",
  	description = "a cli for stuff!"
  )
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (At (Id clipper) Parens_left (Id name) Equal (String stuff)
                     Comma (Id description) Equal (String "a cli for stuff!")
                     Parens_right)

  $ echo -e "type t = | Print { message: string }" > test.caramel
  $ cat test.caramel
  type t = | Print { message: string }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Type (Id t) Equal Pipe (Id Print) Brace_left (Id message)
                     Colon (Id string) Brace_right)

  $ echo -e "type t =\n\t| Print { message: string }\n\t| Write { file: File.t; contents: string}" > test.caramel
  $ cat test.caramel
  type t =
  	| Print { message: string }
  	| Write { file: File.t; contents: string}
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Type (Id t) Equal Pipe (Id Print) Brace_left (Id message)
                     Colon (Id string) Brace_right Pipe (Id Write) Brace_left
                     (Id file) Colon (Id File.t) Semicolon (Id contents) Colon
                     (Id string) Brace_right)

  $ echo -e "fn hello() { joe }" > test.caramel
  $ cat test.caramel
  fn hello() { joe }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Fn (Id hello) Parens_left Parens_right Brace_left (Id joe)
                     Brace_right)

  $ echo -e "pub fn hello() { :joe }" > test.caramel
  $ cat test.caramel
  pub fn hello() { :joe }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id hello) Parens_left Parens_right Brace_left
                     (Atom joe) Brace_right)

  $ echo -e "pub fn hello(_my, _good) { :joe }" > test.caramel
  $ cat test.caramel
  pub fn hello(_my, _good) { :joe }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id hello) Parens_left (Id _my) Comma (Id _good)
                     Parens_right Brace_left (Atom joe) Brace_right)

  $ echo -e "pub fn fst(x, _) { x }" > test.caramel
  $ cat test.caramel
  pub fn fst(x, _) { x }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id fst) Parens_left (Id x) Comma Any Parens_right
                     Brace_left (Id x) Brace_right)

  $ echo -e "pub fn snd(_, y) { y }" > test.caramel
  $ cat test.caramel
  pub fn snd(_, y) { y }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id snd) Parens_left Any Comma (Id y) Parens_right
                     Brace_left (Id y) Brace_right)

  $ echo -e "pub fn pair(x, y) { (x, y) }" > test.caramel
  $ cat test.caramel
  pub fn pair(x, y) { (x, y) }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id pair) Parens_left (Id x) Comma (Id y)
                     Parens_right Brace_left Parens_left (Id x) Comma (Id y)
                     Parens_right Brace_right)

  $ echo -e "pub fn double_pair(x, y) { ((x, x), (y, y)) }" > test.caramel
  $ cat test.caramel
  pub fn double_pair(x, y) { ((x, x), (y, y)) }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id double_pair) Parens_left (Id x) Comma (Id y)
                     Parens_right Brace_left Parens_left Parens_left (Id x)
                     Comma (Id x) Parens_right Comma Parens_left (Id y) Comma
                     (Id y) Parens_right Parens_right Brace_right)

  $ echo -e "pub fn pair_list(x, y) { [x, y] }" > test.caramel
  $ cat test.caramel
  pub fn pair_list(x, y) { [x, y] }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id pair_list) Parens_left (Id x) Comma (Id y)
                     Parens_right Brace_left Bracket_left (Id x) Comma 
                     (Id y) Bracket_right Brace_right)

  $ echo -e "pub fn nil() { [] }" > test.caramel
  $ cat test.caramel
  pub fn nil() { [] }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id nil) Parens_left Parens_right Brace_left
                     Bracket_left Bracket_right Brace_right)

  $ echo -e "pub fn cons(x, y) { [x, ...y] }" > test.caramel
  $ cat test.caramel
  pub fn cons(x, y) { [x, ...y] }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id cons) Parens_left (Id x) Comma (Id y)
                     Parens_right Brace_left Bracket_left (Id x) Comma
                     Dot_dot_dot (Id y) Bracket_right Brace_right)

  $ echo -e "pub fn cons_and_splat(x, y) { [x, x, x, ...y] }" > test.caramel
  $ cat test.caramel
  pub fn cons_and_splat(x, y) { [x, x, x, ...y] }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id cons_and_splat) Parens_left (Id x) Comma 
                     (Id y) Parens_right Brace_left Bracket_left (Id x) Comma
                     (Id x) Comma (Id x) Comma Dot_dot_dot (Id y) Bracket_right
                     Brace_right)

  $ echo -e "pub fn cons_pairs(x, y) { [(x, x), ...y] }" > test.caramel
  $ cat test.caramel
  pub fn cons_pairs(x, y) { [(x, x), ...y] }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id cons_pairs) Parens_left (Id x) Comma (Id y)
                     Parens_right Brace_left Bracket_left Parens_left (Id x)
                     Comma (Id x) Parens_right Comma Dot_dot_dot (Id y)
                     Bracket_right Brace_right)

  $ echo -e "pub fn f() {\n  match n {\n  | [] -> :empty\n  | _ -> :non_empty\n  }\n}" > test.caramel
  $ cat test.caramel
  pub fn f() {
    match n {
    | [] -> :empty
    | _ -> :non_empty
    }
  }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id f) Parens_left Parens_right Brace_left Match
                     (Id n) Brace_left Pipe Bracket_left Bracket_right Arrow
                     (Atom empty) Pipe Any Arrow (Atom non_empty) Brace_right
                     Brace_right)

  $ echo -e "pub fn f() {\n  match n {\n  | () -> :empty\n  | (_, (_, _)) -> :non_empty\n  }\n}" > test.caramel
  $ cat test.caramel
  pub fn f() {
    match n {
    | () -> :empty
    | (_, (_, _)) -> :non_empty
    }
  }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id f) Parens_left Parens_right Brace_left Match
                     (Id n) Brace_left Pipe Parens_left Parens_right Arrow
                     (Atom empty) Pipe Parens_left Any Comma Parens_left Any
                     Comma Any Parens_right Parens_right Arrow (Atom non_empty)
                     Brace_right Brace_right)

  $ echo -e "pub fn f() {\n  match n {\n  | [(x, _), (y, _), ...z] -> :empty\n  | (a, (b, c)) -> :non_empty\n  }\n}" > test.caramel
  $ cat test.caramel
  pub fn f() {
    match n {
    | [(x, _), (y, _), ...z] -> :empty
    | (a, (b, c)) -> :non_empty
    }
  }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id f) Parens_left Parens_right Brace_left Match
                     (Id n) Brace_left Pipe Bracket_left Parens_left (Id x)
                     Comma Any Parens_right Comma Parens_left (Id y) Comma Any
                     Parens_right Comma Dot_dot_dot (Id z) Bracket_right Arrow
                     (Atom empty) Pipe Parens_left (Id a) Comma Parens_left
                     (Id b) Comma (Id c) Parens_right Parens_right Arrow
                     (Atom non_empty) Brace_right Brace_right)

  $ echo -e "pub fn fix(n) {\n  fix(n)\n}" > test.caramel
  $ cat test.caramel
  pub fn fix(n) {
    fix(n)
  }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id fix) Parens_left (Id n) Parens_right Brace_left
                     (Id fix) Parens_left (Id n) Parens_right Brace_right)

  $ echo -e "pub fn fix(n) {\n  fix(n);\n  fix(n)\n}" > test.caramel
  $ cat test.caramel
  pub fn fix(n) {
    fix(n);
    fix(n)
  }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Fn (Id fix) Parens_left (Id n) Parens_right Brace_left
                     (Id fix) Parens_left (Id n) Parens_right Semicolon
                     (Id fix) Parens_left (Id n) Parens_right Brace_right)

  $ echo -e "external f: unit -> 'a = \"hello\"" > test.caramel
  $ cat test.caramel
  external f: unit -> 'a = "hello"
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (External (Id f) Colon (Id unit) Arrow (Type_var a) Equal
                     (String hello))

  $ echo -e "external format: string -> list<'a> -> unit = \"io:format\"" > test.caramel
  $ cat test.caramel
  external format: string -> list<'a> -> unit = "io:format"
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (External (Id format) Colon (Id string) Arrow (Id list)
                     Lesser_than (Type_var a) Greater_than Arrow (Id unit)
                     Equal (String io:format))

  $ echo -e "macro hello(a) { quote { unquote { a } } }\nfn f() { hello(:joe) }" > test.caramel
  $ cat test.caramel
  macro hello(a) { quote { unquote { a } } }
  fn f() { hello(:joe) }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Macro (Id hello) Parens_left (Id a) Parens_right Brace_left
                     Quote Brace_left Unquote Brace_left (Id a) Brace_right
                     Brace_right Brace_right Fn (Id f) Parens_left Parens_right
                     Brace_left (Id hello) Parens_left (Atom joe) Parens_right
                     Brace_right)

  $ echo -e "macro hello(a) { quote { [ unquote { a }, unquote { a } ] } }\nfn f() { hello(:joe) }" > test.caramel
  $ cat test.caramel
  macro hello(a) { quote { [ unquote { a }, unquote { a } ] } }
  fn f() { hello(:joe) }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Macro (Id hello) Parens_left (Id a) Parens_right Brace_left
                     Quote Brace_left Bracket_left Unquote Brace_left (Id a)
                     Brace_right Comma Unquote Brace_left (Id a) Brace_right
                     Bracket_right Brace_right Brace_right Fn (Id f)
                     Parens_left Parens_right Brace_left (Id hello) Parens_left
                     (Atom joe) Parens_right Brace_right)

  $ echo -e "macro if(a, b, c) { quote { match unquote { a } { | :true -> unquote { b } | :false ->  unquote { c } } } }\nfn f() { if(:true, :joe, :armstrong) }" > test.caramel
  $ cat test.caramel
  macro if(a, b, c) { quote { match unquote { a } { | :true -> unquote { b } | :false ->  unquote { c } } } }
  fn f() { if(:true, :joe, :armstrong) }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Macro (Id if) Parens_left (Id a) Comma (Id b) Comma 
                     (Id c) Parens_right Brace_left Quote Brace_left Match
                     Unquote Brace_left (Id a) Brace_right Brace_left Pipe
                     (Atom true) Arrow Unquote Brace_left (Id b) Brace_right
                     Pipe (Atom false) Arrow Unquote Brace_left (Id c)
                     Brace_right Brace_right Brace_right Brace_right Fn 
                     (Id f) Parens_left Parens_right Brace_left (Id if)
                     Parens_left (Atom true) Comma (Atom joe) Comma
                     (Atom armstrong) Parens_right Brace_right)

  $ echo -e "pub macro debug(ast) {\n  match ast {\n  | Str_type { typ_name } ->\n    let name = quote {\n      pub fn unquote { id(typ_name) }() {\n        unquote { str(typ_name) }\n      }\n    };\n    [ ast, name ]\n  | _ -> [ ast ]\n  }" > test.caramel
  $ cat test.caramel
  macro if(a, b, c) { quote { match unquote { a } { | :true -> unquote { b } | :false ->  unquote { c } } } }
  fn f() { if(:true, :joe, :armstrong) }
  $ caramel parse --file test.caramel --dump-tokens --debug
  caramel: [DEBUG] (Macro (Id if) Parens_left (Id a) Comma (Id b) Comma 
                     (Id c) Parens_right Brace_left Quote Brace_left Match
                     Unquote Brace_left (Id a) Brace_right Brace_left Pipe
                     (Atom true) Arrow Unquote Brace_left (Id b) Brace_right
                     Pipe (Atom false) Arrow Unquote Brace_left (Id c)
                     Brace_right Brace_right Brace_right Brace_right Fn 
                     (Id f) Parens_left Parens_right Brace_left (Id if)
                     Parens_left (Atom true) Comma (Atom joe) Comma
                     (Atom armstrong) Parens_right Brace_right)
