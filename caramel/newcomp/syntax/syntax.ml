open Caramel_caml

(* Exported modules *)
module Parsetree = Parsetree
module Parse_error = Parse_error

let parse_implementation = Caml.parse_impl

let parse_interface = Caml.parse_intf

let pp_impl = Printast.implementation

let pp_intf = Printast.interface
