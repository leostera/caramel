module Erl = Erlang.Parsetree_helper

module Well_known = struct
  let unit = Erl.Atom.mk ~ctx:Erl.Loc.empty "()"

  let is_well_known id = if id = unit then `unit else `not_well_known
end

let constructor ident =
  let name = Longident.last ident in
  Erl.Atom.mk ~ctx:Erl.Loc.empty name

let binding ident =
  let name = Ident.name ident in
  Erl.Name.var ~ctx:Erl.Loc.empty ~name

let function_name ident =
  let name = Ident.name ident in
  Erl.Atom.mk ~ctx:Erl.Loc.empty name

let type_name ident =
  let name = Ident.name ident in
  Erl.Atom.mk ~ctx:Erl.Loc.empty name
