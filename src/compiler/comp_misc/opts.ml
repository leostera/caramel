type t = {
  sources : string list;
  dump_ast : bool;
  target : Target.t;
  no_stdlib : bool;
  stdlib_path : string;
}
