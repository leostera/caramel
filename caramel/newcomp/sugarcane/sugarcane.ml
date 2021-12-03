module Erl = Erlang.Parsetree_helper

(* A translation unit is the minimum data we need to translate an OCaml module
   hierarchy into a series of Erlang modules
*)
type translation_unit = {
  file_name : string;
  module_name : string;
  signature : Types.signature;
  structure : Typedtree.structure;
}

(**
  Turn an OCaml Typedtree into a number of Erlang modules.

  This is achieved by traversing the OCaml typed tree, and finding every module
  defined in it. Once that list is available, we translate each individual
  module into an Erlang module.

  The resulting list can then be used for pretty-printing Erlang sources.

  For example, given the following OCaml:

  ```ocaml
  (* file: a.ml *)
  let x () = 1

  module A = struct
    let y () = 2
  end
  ```

  We will end up with the following Erlang modules:


  ```erlang
  (* file: a.erl *)
  -module(a).
  -export([x/0]).
  x() -> 1.

  (* file: a_a.erl *)
  -module(a_a).
  -export([y/0]).
  y() -> 2.
  ```

 *)
let translate { file_name; module_name; signature; structure; _ } =
  Logs.debug (fun f ->
      f "Translating file: %s (module %s)" file_name module_name);
  (* Build the root module name. This name will be used to build a hierarchy of
     module names. *)
  let module_name = Identifier.Module_name.root module_name in
  let root_module = Module.make { module_name; structure; signature } in
  let modules =
    Module.Tree_visitor.find_modules ~prefix:module_name ~structure
  in
  root_module :: modules
