open Caramel_misc
open Caramel_typing

type t = {
  ctx : Context.t;
  comp_unit : Compilation_unit.t;
  program : Typing.typed_unit;
  ir : Ir.t;
  dump_pass : int;
}

let make ~unit ~program ~dump_pass =
  { ctx = Context.make (); comp_unit = unit; program; ir = Ir.empty; dump_pass }
