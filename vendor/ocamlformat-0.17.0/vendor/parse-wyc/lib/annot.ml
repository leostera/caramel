open Ast_helper
open Migrate_ast.Parsetree

module Ext = struct
  let mk () = (Location.mkloc "merlin.hole" !default_loc, PStr [])

  let is_generated = function
    | (({ txt = "merlin.hole"; _ }, PStr []) : Ppxlib.extension) -> true
    | _ -> false
end

module Exp = struct
  let mk () = Exp.extension (Ext.mk ())

  let is_generated (e : Ppxlib.expression) =
    match e.pexp_desc with
    | Pexp_extension ext when Ext.is_generated ext -> true
    | _ -> false
end

module Attr = struct
  let mk () = Attr.mk { txt = "merlin.hole.gen"; loc = Location.none } (PStr [])

  let is_generated (a : Ppxlib.attribute) =
    match (a.attr_name.txt, a.attr_payload) with
    | "merlin.hole.gen", PStr [] -> true
    | _ -> false
end

module Class_exp = struct
  let mk () = Cl.extension (Ext.mk ())

  let is_generated (e : Ppxlib.class_expr) =
    match e.pcl_desc with
    | Pcl_extension ext when Ext.is_generated ext -> true
    | _ -> false
end
