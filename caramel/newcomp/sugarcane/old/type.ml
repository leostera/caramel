let export_from_signature ~ident ~value_desc:_ =
  let _ident = Identifier.from_ident ~ident in
  Error.todo "export_from_signature"
(*
   let name = Identifier.local_symbol ident in
    Erl.Attr.export_type ~name ~arity:1 *)
