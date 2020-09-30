open Ast

let find_fun_by_name mod_ name =
  mod_.functions |> List.find_opt (fun { fd_name; _ } -> fd_name = name)
