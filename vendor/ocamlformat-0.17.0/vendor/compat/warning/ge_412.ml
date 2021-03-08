let () = Clflags.error_style := Some Misc.Error_style.Short

let with_warning_filter ~filter ~f =
  let warning_reporter = !Location.warning_reporter in
  (Location.warning_reporter :=
     fun loc warn ->
       if filter loc warn then Location.default_warning_reporter loc warn
       else None) ;
  let reset () = Location.warning_reporter := warning_reporter in
  try
    let x = f () in
    reset () ; x
  with e -> reset () ; raise e

let print_warning l w =
  match Location.default_warning_reporter l w with
  | Some reporter -> Location.print_report Caml.Format.err_formatter reporter
  | None -> ()

let is_unexpected_docstring = function
  | Warnings.Unexpected_docstring _ -> true
  | _ -> false
