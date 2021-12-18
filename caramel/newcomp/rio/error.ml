let p = Format.fprintf Format.err_formatter

let todo hint =
  p "PRINTER: %s\n" hint;
  exit 1
