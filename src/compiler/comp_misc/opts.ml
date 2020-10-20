type t = {
  sources : string list;
  dump_ast : bool;
  targets : Target.t list;
  no_stdlib : bool;
  stdlib_path : string;
}
