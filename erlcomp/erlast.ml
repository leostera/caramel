type arity = int

and atom = string

and name = string

and guard = unit

and expr =
  | Exp_name of atom
  | Exp_tuple of expr list

and pattern_match =
  | Pattern_ignore
  | Pattern_binding of atom

and fun_case = {
  fc_lhs: pattern_match list;
  fc_guards: guard list;
  fc_rhs: expr;
}

and fun_decl = {
  fd_name: atom;
  fd_cases: fun_case list;
}

(** A type declaration in an Erlang module. This follows what is currently
    representable by Dialyzer.

    See:
      http://erlang.org/doc/reference_manual/typespec.html
 *)

and record_field = { rf_name: atom; rf_type: type_kind }

and variant_constructor = { vc_name: atom; vc_args: type_kind list }

and type_constr = { tc_name: atom; tc_args: type_kind list }

and type_kind =
  | Type_function of type_kind list
  | Type_constr of type_constr
  | Type_variable of name
  | Type_tuple of type_kind list
  | Type_record of { fields: record_field list; }
  | Type_variant of { constructors: variant_constructor list; }

and type_decl = {
  typ_kind: type_kind;
  typ_name: atom;
  typ_params: atom list;
}

(** An exported symbol in an Erlang module. This could be a function or a type.
    See:
      http://erlang.org/doc/reference_manual/modules.html for missing fields.
      http://erlang.org/doc/reference_manual/typespec.html
 *)
and export_type = Export_function | Export_type

and export = {
  exp_type: export_type;
  exp_name: atom;
  exp_arity: arity;
}

(** The type of an Erlang module. Intentionally incomplete for now.
    See:
      http://erlang.org/doc/reference_manual/modules.html
 *)
and t = {
  file_name: string;
  behaviour: atom option;
  module_name: atom;
  exports: export list;
  types: type_decl list;
  functions: fun_decl list;
}

let make ~name ~exports ~types ~functions = {
  file_name = name ^ ".erl";
  behaviour = None;
  module_name = name;
  exports = exports;
  types = types;
  functions = functions;
}

let make_fn_export exp_name exp_arity = {exp_type=Export_function; exp_name; exp_arity }
let make_type_export exp_name exp_arity = {exp_type=Export_type; exp_name; exp_arity }

let make_named_type typ_name typ_params typ_kind = { typ_name; typ_params; typ_kind }

let type_any = Type_constr { tc_name="any"; tc_args=[] }
