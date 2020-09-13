type arity = int

and atom = string

and export_type = Export_function | Export_type

and export = {
  export_type: export_type;
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

let make_fn_export name arity = {export_type = Export_function; name; arity }
let make_type_export name arity = {export_type = Export_type; name; arity }
