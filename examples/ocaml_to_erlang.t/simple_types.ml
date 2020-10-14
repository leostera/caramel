type ('ok, 'err) result = Ok of 'ok | Error of 'err

type info = unit

type type_ = Type_arrow of type_ * type_ | Type_bool

let rec t_to_str type_ =
  match type_ with
  | Type_arrow (ta, tr) ->
      let ta2 = t_to_str ta in
      let tr2 = t_to_str tr in
      "(lambda " ^ ta2 ^ " -> " ^ tr2 ^ ")"
  | Type_bool -> "bool"

type t =
  | Term_var of info * int * int
  | Term_abs of info * string * type_ * t
  | Term_app of info * t * t
  | Term_true of info
  | Term_false of info
  | Term_if of info * t * t * t

type binding = Name_bind | Var_bind of type_

type ctx = (string * binding) list

let bind : ctx -> string -> binding -> ctx =
 fun ctx name binding -> (name, binding) :: ctx

let rec find_binding : ctx -> int -> binding option =
 fun ctx i ->
  match ctx with
  | [] -> None
  | (_x, b) :: rest -> if i = 0 then Some b else find_binding rest (i - 1)

let rec type_of : ctx -> t -> (type_, string) result =
 fun ctx term ->
  match term with
  | Term_var (_info, i, _) -> (
      match find_binding ctx i with
      | None -> Error "Unbound_name"
      | Some (Var_bind t) -> Ok t
      | Some Name_bind -> Error "Untyped_binding" )
  | Term_abs (_info, name, arg_type, ret_type) -> (
      let ctx2 = bind ctx name (Var_bind arg_type) in
      match type_of ctx2 ret_type with
      | Ok ret_type2 -> Ok (Type_arrow (arg_type, ret_type2))
      | err -> err )
  | Term_app (_info, fn, arg) -> (
      match (type_of ctx fn, type_of ctx arg) with
      | Ok (Type_arrow (arr_t, ret_t)), Ok arg_t ->
          if arr_t = arg_t then Ok ret_t
          else
            let a = t_to_str arr_t in
            let b = t_to_str arg_t in
            Error
              ( "Expected parameter to be of type:  " ^ a
              ^ " but instead found: " ^ b )
      | Ok _, _ -> Error "Expected_arrow_type"
      | err, _ -> err )
  | Term_false _ -> Ok Type_bool
  | Term_true _ -> Ok Type_bool
  | Term_if (_, t_if, t_then, t_else) -> (
      match (type_of ctx t_if, type_of ctx t_then, type_of ctx t_else) with
      | Ok Type_bool, Ok a, Ok b ->
          if a = b then Ok a
          else Error "then and else branches must have the same type"
      | _, _, _ -> Error "condition must be boolean" )

let run () =
  let print_result r =
    match r with
    | Error str -> Io.format "ERROR: ~s\n\n" [ str ]
    | _ -> Io.format "~p\n\n" [ r ]
  in
  let f = Term_abs ((), "f", Type_arrow (Type_bool, Type_bool), Term_true ()) in
  let app1 = Term_app ((), f, f) in
  let app2 = Term_app ((), f, Term_abs ((), "g", Type_bool, Term_true ())) in
  let app3 = Term_var ((), 0, 0) in
  let ctx = [ ("", Var_bind Type_bool) ] in
  print_result (type_of [] app1);
  print_result (type_of [] app2);
  print_result (type_of [] app3);
  print_result (type_of ctx app3)
