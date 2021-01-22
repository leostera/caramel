let impl_tests =
  let valid_test =
    {|
let fooooooooooooooo =
  let fooooooooooooo =
    let foooooooooooooo =
      foooooooooooooo
    in
    fooooo, fooooooo
  in
  fooooooooooooooooo;
  foooooooooooooo
|}
  in
  let invalid_after_eq_test = {|let fooooooooooooooo =|} in
  let invalid_after_in_test =
    {| (* line 1 *)
let fooooooooooooooooooooo = (* line 2 *)
  let foooooooooooooooooooo =
    let foooooooooooooooo =
      foooooooo
    in
    foooooo
  in (* line 8 *)
|}
  in
  let invalid_seq_modules_test =
    {|
module M = struct
  let foooooo = foooo

  let foooooooooooooo = (* line 5 *)

  let foooooooooooo = fooooo
end
|}
  in
  let not_closed_module_test =
    {|
module M = struct
  let foo = foo
  let foo =
|}
  in
  let not_closed_module_test_2 = {|
module M = struct
  let foo = foo in
|} in
  let not_closed_sig = {|
module K : sig
  type t
|} in
  let not_closed_begin = {| let x = if x then begin a |} in
  let not_closed_if = {| let x = if k |} in
  let not_closed_if_2 = {| let x = if k then |} in
  let invalid_if = {| let x = if k then else |} in
  let invalid_if_2 = {| let x = if k then x else |} in
  let not_closed_class = {| class c = object |} in
  let not_closed_class_2 = {| class c |} in
  let not_closed_class_3 = {| class c = |} in
  let not_closed_class_4 = {| class |} in
  let binop = {| x + |} in
  let many_not_closed =
    {|
let foooooo
= fooooooooo

let foooooooo
= bar baaaaar barrr

module K = struct
  let k

;;
begin
  let x = in
  ()

;;
let foooooo =
  fooooo
  foooooooo

let k =
|}
  in
  let _escape_error = {|
try foo () with ;;

(3 : );;

(3 :> );;
|} in
  let _expecting =
    {|
let f = function
  | 3 as 3 -> ()
;;

let f = function
  | 3 :: -> ()
;;

let f = function
  | 3 | -> ()
;;

let f = function
  | List.( -> ()
;;

let f = function
  | (3 : 3) -> ()
;;

let f = function
  | (3,) -> ()
;;

let f = function
  | ( -> ()
;;

let f = function
  | (module -> ()
;;
|}
  in
  let _pr7847 = {| external x : unit -> (int,int)`A.t = "x" |} in
  let unclosed_class_simpl_expr1 = {|
class c = object
  method x = 1
|} in
  let _unclosed_class_simpl_expr2 = {| class c = (object end : object end |} in
  let _unclosed_class_simpl_expr3 = {| class c = (object end |} in
  let _unclosed_object = {| let o = object |} in
  let _unclosed_paren_module_expr1 = {| module M = (struct end : sig end |} in
  let _unclosed_paren_module_expr2 = {| module M = (struct end |} in
  let _unclosed_paren_module_expr3 = {| module M = (val 3 : |} in
  let _unclosed_paren_module_expr4 = {| module M = (val 3 :> |} in
  let _unclosed_paren_module_expr5 = {| module M = (val 3 |} in
  let _unclosed_simple_expr =
    {|
(3; 2;;

begin 3; 2;;

List.(3; 2;;

simple_expr.(3; 2;;

simple_expr.[3; 2;;

simple_expr.%[3;;

simple_expr.%(3;;

simple_expr.%{3;;

foo.Bar.%[3;;

foo.Bar.%(3;;

foo.Bar.%{3;;

simple_expr.{3, 2;;

{ x = 3; y;;

List.{ x = 3; y ;;

[| 3; 2;;

List.[|3; 2;;

[3; 2;;

List.[3; 2;;

{< x = 3; y; ;;

List.{< x = 3; y ;;

(module struct end :;;

List.(module struct end :;;

(=;
|}
  in
  let _unclosed_simple_pattern =
    {|
let f = function
  | List.(_
;;

let f = function
  | (_
;;

let f = function
  | (_ : int
;;

(* Impossible to get the "unclosed (" message here. This case gets absorbed by
   val_ident... *)

let f = function
  | (module Foo : sig end
;;

(* As with expressions, impossible to get the unclosed message for the following
   cases. *)

let f = function
  | { foo; bar;
;;

let f = function
  | [ 1; 2;
;;

let f = function
  | [| 3; 4;
;;
|}
  in
  let unclosed_struct = {|
module M = struct
  type t = T
|} in
  [
    ("empty", "", []);
    ("valid", valid_test, []);
    ("invalid after eq", invalid_after_eq_test, [ ((1, 0), (1, 22)) ]);
    ("invalid after in", invalid_after_in_test, [ ((2, 0), (8, 4)) ]);
    ("invalid seq modules", invalid_seq_modules_test, [ ((2, 0), (7, 28)) ]);
    ("not closed module", not_closed_module_test, [ ((2, 0), (4, 11)) ]);
    ("not closed module 2", not_closed_module_test_2, [ ((2, 0), (3, 18)) ]);
    ("not closed sig", not_closed_sig, [ ((2, 0), (3, 8)) ]);
    ("not closed begin", not_closed_begin, [ ((1, 1), (1, 26)) ]);
    ("not closed if", not_closed_if, [ ((1, 1), (1, 13)) ]);
    ("not closed if 2", not_closed_if_2, [ ((1, 1), (1, 18)) ]);
    ("invalid if", invalid_if, [ ((1, 1), (1, 18)) ]);
    ("invalid if 2", invalid_if_2, [ ((1, 1), (1, 25)) ]);
    ("not closed class", not_closed_class, [ ((1, 1), (1, 17)) ]);
    ("not closed class 2", not_closed_class_2, [ ((1, 1), (1, 8)) ]);
    ("not closed class 3", not_closed_class_3, [ ((1, 1), (1, 10)) ]);
    ("not closed class 4", not_closed_class_4, [ ((1, 1), (1, 6)) ]);
    ("binop", binop, [ ((1, 1), (1, 4)) ]);
    ( "many not closed",
      many_not_closed,
      [ ((8, 0), (14, 4)); ((21, 0), (21, 7)) ] );
    (*
    ( "escape_error",
      escape_error,
      [ ((2, 0), (2, 18)); ((4, 0), (4, 8)); ((6, 0), (6, 9)) ] );
    ( "expecting",
      expecting,
      [
        ((6, 0), (8, 2));
        ((10, 0), (12, 2));
        ((14, 0), (16, 2));
        ((22, 0), (24, 2));
        ((26, 0), (28, 2));
        ((30, 0), (32, 2));
      ] );
    ("pr7847", pr7847, [ ((1, 1), (1, 41)) ]);
*)
    ( "unclosed class simpl expr1",
      unclosed_class_simpl_expr1,
      [ ((2, 0), (3, 14)) ] );
    (*
    ( "unclosed class simpl expr2",
      unclosed_class_simpl_expr2,
      [ ((1, 1), (1, 35)) ] );
    ( "unclosed class simpl expr3",
      unclosed_class_simpl_expr3,
      [ ((1, 1), (1, 22)) ] );
    ("unclosed object", unclosed_object, [ ((1, 1), (1, 15)) ]);
    ( "unclosed paren module expr1",
      unclosed_paren_module_expr1,
      [ ((1, 1), (1, 33)) ] );
    ( "unclosed paren module expr2",
      unclosed_paren_module_expr2,
      [ ((1, 1), (1, 23)) ] );
    ( "unclosed paren module expr3",
      unclosed_paren_module_expr3,
      [ ((1, 1), (1, 20)) ] );
    ( "unclosed paren module expr4",
      unclosed_paren_module_expr4,
      [ ((1, 1), (1, 21)) ] );
    ( "unclosed paren module expr5",
      unclosed_paren_module_expr5,
      [ ((1, 1), (1, 19)) ] );
    ("unclosed simple expr", unclosed_simple_expr, [ ((2, 0), (46, 3)) ]);
    ("unclosed simple pattern", unclosed_simple_pattern, [ ((2, 0), (34, 2)) ]);
*)
    ("unclosed struct", unclosed_struct, [ ((2, 0), (3, 12)) ]);
  ]

let intf_tests =
  let _unclosed_class_signature = {| class c : object |} in
  let _unclosed_paren_module_type = {| module M : (sig end |} in
  let unclosed_sig = {|
module M : sig
  type t = T
 |} in
  [
    ("empty", "", []);
    (*
    ("unclosed class signature", unclosed_class_signature, [ ((1, 1), (1, 17)) ]);
    ( "unclosed paren module type",
      unclosed_paren_module_type,
      [ ((1, 1), (1, 20)) ] );
*)
    ("unclosed sig", unclosed_sig, [ ((2, 0), (3, 12)) ]);
  ]

let check_tests checker tests =
  List.map
    (fun (name, input, locs) ->
      (name, `Quick, fun () -> checker ~name ~input ~locs))
    tests

let tests =
  [
    ("impl", check_tests Utils.check_impl impl_tests);
    ("intf", check_tests Utils.check_intf intf_tests);
    ("use_file", check_tests Utils.check_use_file impl_tests);
  ]

let () = Alcotest.run "Parse_wyc" tests
