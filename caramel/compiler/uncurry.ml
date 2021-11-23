open Types

let rec is_unit (t : Types.type_expr) =
  match t.desc with
  | Tconstr (p, _, _) -> Path.same p Predef.path_unit
  | Tlink t' -> is_unit (Btype.repr t')
  | _ -> false

let drop_last_unit acc = match acc with [ x ] when is_unit x -> [] | _ -> acc

let rec uncurry_tarrow type_expr acc =
  match type_expr.desc with
  | Tarrow (_, param, out, _) -> uncurry_tarrow out (param :: acc)
  | Tlink t -> uncurry_tarrow (Btype.repr t) acc
  | _ ->
      let acc = drop_last_unit acc in
      (List.rev acc, type_expr)

let rec from_type_expr :
    Types.type_expr ->
    [ `Uncurried of Types.type_expr list * Types.type_expr | `Not_a_function ] =
 fun type_expr ->
  match type_expr.desc with
  | Tarrow (_, _, _, _) -> `Uncurried (uncurry_tarrow type_expr [])
  | Tlink t -> from_type_expr (Btype.repr t)
  | _ -> `Not_a_function
