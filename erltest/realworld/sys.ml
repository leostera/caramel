type backend_type =
  | Native
  | Bytecode
  | Other of string

let backend_type () = Other "BEAM"
