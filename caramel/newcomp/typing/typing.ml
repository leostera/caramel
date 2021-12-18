open Caramel_caml

type typed_interface = Typedtree.signature

type typed_unit = Typedtree.implementation * Lambda.program

let check_interface = Caml.typecheck_intf

let check_implementation = Caml.typecheck_impl

let pp_intf = Printtyped.interface

let pp_impl ppf (_, lambda) = Printlambda.program ppf lambda
