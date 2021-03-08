let () = Clflags.error_style := Some Misc.Error_style.Short

let add_warning_name s =
  match int_of_string s with
  | exception _ -> s
  | n ->
      begin
        match Warning_name.warning_name n with
        | Some s -> Printf.sprintf "%d [%s]" n s
        | None -> s
      end

let map_kind_code ~f = function
  | Location.Report_error -> Location.Report_error
  | Report_warning s -> Report_warning (f s)
  | Report_warning_as_error s -> Report_warning_as_error (f s)
  | Report_alert s -> Report_alert (f s)
  | Report_alert_as_error s -> Report_alert_as_error (f s)

let map_report_code report ~f =
  { report with Location.kind = map_kind_code ~f report.Location.kind
  }

let report_add_warning_name report =
  map_report_code report ~f:add_warning_name

let print_warning l w =
  match Location.default_warning_reporter l w with
  | Some report ->
      report_add_warning_name report |>
      Location.print_report Caml.Format.err_formatter
  | None -> ()

let is_unexpected_docstring = function
  | Warnings.Bad_docstring _ -> true
  | _ -> false

let with_warning_filter ~filter ~f =
  let warning_reporter = !Location.warning_reporter in
  (Location.warning_reporter :=
     fun loc warn ->
       let report =
         if filter loc warn then Location.default_warning_reporter loc warn
         else None
       in
       Option.map report_add_warning_name report) ;
  let reset () = Location.warning_reporter := warning_reporter in
  try
    let x = f () in
    reset () ; x
  with e -> reset () ; raise e
