type macro_unit = {
  parsetree : Parsetree.t;
  dump_macro_env : bool;
  dump_expanded : bool;
}

let run { parsetree; dump_expanded; _ } =
  let ( let* ) = Result.bind in

  let* parsetree = Macro_expander.run parsetree in

  let* () =
    if dump_expanded then (
      Logs.debug (fun f -> f "Expanded program: %a" Parsetree.pp parsetree);
      Error `Early_exit)
    else Ok ()
  in

  Ok parsetree
