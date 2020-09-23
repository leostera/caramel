type arity = int

and atom = string

and export = {
  name: atom;
  arity: arity;
}

(**
 * The type of an Erlang module. Intentionally incomplete for now.
 *
 * See: http://erlang.org/doc/reference_manual/modules.html for missing fields.
 *)
and t = {
  file_name: string;
  behaviour: atom option;
  module_name: atom;
  exports: export list;
}

let make ~name ~exports = {
  file_name = name ^ ".erl";
  behaviour = None;
  module_name = name;
  exports = exports;
}

let make_export (name, arity) = { name; arity }
