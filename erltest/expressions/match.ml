
let match_unit () =
  match () with
  | () -> true

let match_ignore () =
  match () with
  | _ -> true

let match_int () =
  match 1 with
  | 1 -> true

let match_str () =
  match "hello" with
  | "xavier" -> true
  | "remy" -> true
  | "gal" -> true
  | "mike" -> true
  | "robert" -> true
  | "joe" -> true

(* FIXME: none of the cases below that have a literal in the list
 * are generated appropriately. All of them become _
 *)
type int_pair = { fst: int; snd: int }
let match_record () =
  match { fst=0; snd=1 } with
  | {fst=10; snd=10} -> true
  | {fst=0} -> true
  | {snd=1} -> true

(* FIXME: none of the cases below that have a literal in the list
 * are generated appropriately. All of them become _
 *)
let match_list () =
  match [0; 1] with
  | [] -> true
  | 1 :: xs -> true
  | 1 :: [] -> true
  | 0 :: 1 :: _ -> true
  | [0; 1] -> true

(* FIXME: none of the cases below that have a literal in the list
 * are generated appropriately. All of them become _
 *)
let match_tuples () =
  match 1, true, "hello" with
  | 1, _, _ -> true
  | 1, true, _ -> true
  | 1, true, "hello" -> true

let match_atoms () =
  match `Hello with
  | `Xavier -> true
  | `Joe -> true
  | _ -> false
