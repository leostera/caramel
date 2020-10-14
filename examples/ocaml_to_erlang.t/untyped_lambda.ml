(* Adapted from Pierce's TAPL *)

type info = { line : int; col : int }

type expr = Var of int * int | Abs of string * t | App of t * t

and t = { expr : expr; info : info }

type binding = Name_bind

type ctx = (string * binding) list

let with_info t = { info = { line = 0; col = 0 }; expr = t }

let rec is_bound : ctx -> string -> bool =
 fun ctx x ->
  match ctx with
  | [] -> false
  | (y, _) :: rest -> if x = y then true else is_bound rest x

let rec fresh_name ctx x =
  if is_bound ctx x then fresh_name ctx (x ^ "'") else ((x, Name_bind) :: ctx, x)

let rec index_to_name : ctx -> int -> string option =
 fun ctx n ->
  match ctx with
  | [] -> None
  | (x, _) :: xs -> if n = 0 then Some x else index_to_name xs (n - 1)

let rec pp_term ctx { expr; info } =
  match expr with
  | Abs (name, term) ->
      let ctx2, name2 = fresh_name ctx name in
      Io.format "(lambda ~p.\n\t" [ name2 ];
      pp_term ctx term;
      Io.format ")" []
  | App (fn, arg) ->
      Io.format "(" [];
      pp_term ctx fn;
      pp_term ctx arg;
      Io.format ")" []
  | Var (x, n) ->
      if Erlang.length ctx = n then
        match index_to_name ctx x with
        | Some y -> Io.format "~p" [ y ]
        | None -> Io.format "[name was missing: ~p]" [ x ]
      else Io.format "[bad index: ~p]" [ n ]

let sample () =
  with_info
    (Abs
       ( "sample",
         with_info
           (App
              ( with_info (Abs ("f", with_info (Var (0, 0)))),
                with_info (Var (0, 1)) )) ))
