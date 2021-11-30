module Erl = Erlang.Parsetree_helper

let export_from_signature ~name ~value_desc:_ =
  let name = Identifier.function_name name in
  Erl.Attr.export_type ~name ~arity:1
