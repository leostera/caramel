type t = Erlang | Core_erlang | Native | Type_check | Archive

let to_string = function
  | Erlang -> "erl"
  | Core_erlang -> "core"
  | Native -> "native"
  | Type_check -> "type_check"
  | Archive -> "archive"
