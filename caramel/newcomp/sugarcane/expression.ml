module Erl = Erlang.Parsetree_helper

let rec from_ocaml_expression ~expression =
  let open Typedtree in
  match expression.exp_desc with
  | Texp_apply
      ({ exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ }, _)
  | Texp_array _
  | Texp_assert { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ }
    ->
      Error.todo ()
  | Texp_constant constant -> from_ocaml_constant ~constant
  | Texp_construct
      ( { loc = { loc_start = _; loc_end = _; _ }; _ },
        { cstr_res = _; cstr_loc = { loc_start = _; loc_end = _; _ }; _ },
        _ )
  | Texp_extension_constructor
      ({ loc = { loc_start = _; loc_end = _; _ }; _ }, _)
  | Texp_field
      ( { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ },
        { loc = { loc_start = _; loc_end = _; _ }; _ },
        {
          lbl_res = _;
          lbl_arg = _;
          lbl_loc = { loc_start = _; loc_end = _; _ };
          _;
        } )
  | Texp_for
      ( _,
        { ppat_loc = { loc_start = _; loc_end = _; _ }; _ },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ },
        _,
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ } )
  | Texp_function _ ->
      Error.todo ()
  | Texp_ident (_path, { txt = ident; _ }, _types) ->
      from_ocaml_identifier ~ident
  | Texp_ifthenelse
      ( { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ },
        _ )
  | Texp_instvar (_, _, { loc = { loc_start = _; loc_end = _; _ }; _ })
  | Texp_lazy { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ }
  | Texp_let
      (_, _, { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ })
  | Texp_letexception
      ( {
          ext_name = { loc = { loc_start = _; loc_end = _; _ }; _ };
          ext_type = { ext_loc = { loc_start = _; loc_end = _; _ }; _ };
          ext_loc = { loc_start = _; loc_end = _; _ };
          _;
        },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ } )
  | Texp_letmodule
      ( _,
        { loc = { loc_start = _; loc_end = _; _ }; _ },
        _,
        { mod_loc = { loc_start = _; loc_end = _; _ }; _ },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ } )
  | Texp_letop _
  | Texp_match
      ({ exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ }, _, _)
  | Texp_new
      ( _,
        { loc = { loc_start = _; loc_end = _; _ }; _ },
        { cty_loc = { loc_start = _; loc_end = _; _ }; _ } )
  | Texp_object (_, _) ->
      Error.todo ()
  | Texp_open (_open, expression) -> from_ocaml_expression ~expression
  | Texp_override (_, _)
  | Texp_pack { mod_loc = { loc_start = _; loc_end = _; _ }; _ }
  | Texp_record _
  | Texp_send
      ({ exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ }, _, _)
  | Texp_sequence
      ( { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ } )
  | Texp_setfield
      ( { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ },
        { loc = { loc_start = _; loc_end = _; _ }; _ },
        {
          lbl_res = _;
          lbl_arg = _;
          lbl_loc = { loc_start = _; loc_end = _; _ };
          _;
        },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ } )
  | Texp_setinstvar
      ( _,
        _,
        { loc = { loc_start = _; loc_end = _; _ }; _ },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ } )
  | Texp_try
      ({ exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ }, _)
  | Texp_tuple _ | Texp_unreachable
  | Texp_variant (_, _)
  | Texp_while
      ( { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ } ) ->
      Error.todo ()

and from_ocaml_constant ~constant =
  let term = Term.from_ocaml_constant ~constant in
  Erl.Expr.term ~term

and from_ocaml_identifier ~ident =
  let name = Identifier.from_ocaml ~ident in
  Erl.Expr.var ~name

let from_ocaml_case :
    case:Typedtree.value Typedtree.case -> 'ctx Erlang.Parsetree.expression =
 fun ~case -> from_ocaml_expression ~expression:case.c_rhs
