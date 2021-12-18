let ppf = Format.err_formatter

let todo_lambda hint =
  Format.fprintf ppf
    "TODO: Support for this lambda construct has not been implemented yet: %a\n"
    Printlambda.lambda hint;
  exit 1

let todo_lambda_prim hint =
  Format.fprintf ppf
    "TODO: Support for this lambda primitive has not been implemented yet: %a\n"
    Printlambda.primitive hint;
  exit 1

let todo hint =
  Format.fprintf ppf "TODO: This function has not been implemented yet: %s\n"
    hint;
  exit 1

let panic hint =
  Format.fprintf ppf "PANIC: %s\n" hint;
  assert false
