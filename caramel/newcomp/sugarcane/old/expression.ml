(** Translate an OCaml expression into an IR expression description.
  *)
let rec from_ocaml_expression ~ctx ~expression =
  let open Typedtree in
  match expression.exp_desc with
  | Texp_apply (fn, args) -> from_ocaml_apply ~ctx ~fn ~args
  | Texp_array parts -> from_ocaml_array ~parts
  | Texp_assert expr -> from_ocaml_assert ~expr
  | Texp_constant constant -> from_ocaml_constant ~constant
  | Texp_construct (loc, desc, exprs) ->
      from_ocaml_construct ~ctx ~ident:loc.txt ~desc ~exprs
  | Texp_extension_constructor
      ({ loc = { loc_start = _; loc_end = _; _ }; _ }, _) ->
      Error.todo "Texp_extension_constructor"
  | Texp_field
      ( { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ },
        { loc = { loc_start = _; loc_end = _; _ }; _ },
        {
          lbl_res = _;
          lbl_arg = _;
          lbl_loc = { loc_start = _; loc_end = _; _ };
          _;
        } ) ->
      Error.todo "Texp_field"
  | Texp_for
      ( _,
        { ppat_loc = { loc_start = _; loc_end = _; _ }; _ },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ },
        _,
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ } ) ->
      Error.todo "Texp_for"
  | Texp_function { arg_label; param; cases; partial } ->
      from_ocaml_function ~arg_label ~param ~cases ~partial
  | Texp_ident (_path, loc, _types) -> from_ocaml_identifier ~ident:loc.txt
  | Texp_ifthenelse (cond, then_expr, else_expr) ->
      from_ocaml_ifthenelse ~cond ~then_expr ~else_expr
  | Texp_instvar (_, _, _) | Texp_lazy _ -> Error.todo "Texp_instvar"
  | Texp_let (flags, binds, expr) -> from_ocaml_let ~flags ~binds ~expr
  | Texp_letexception
      ( {
          ext_name = { loc = { loc_start = _; loc_end = _; _ }; _ };
          ext_type = { ext_loc = { loc_start = _; loc_end = _; _ }; _ };
          ext_loc = { loc_start = _; loc_end = _; _ };
          _;
        },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ } ) ->
      Error.todo "Texp_letexception"
  | Texp_letmodule
      ( _,
        { loc = { loc_start = _; loc_end = _; _ }; _ },
        _,
        { mod_loc = { loc_start = _; loc_end = _; _ }; _ },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ } ) ->
      Error.todo "Texp_letmodule"
  | Texp_letop _ -> Error.todo "Texp_letop"
  | Texp_match (expr, cases, partial) -> from_ocaml_match ~expr ~cases ~partial
  | Texp_new
      ( _,
        { loc = { loc_start = _; loc_end = _; _ }; _ },
        { cty_loc = { loc_start = _; loc_end = _; _ }; _ } ) ->
      Error.todo "Texp_new"
  | Texp_object (_, _) -> Error.todo "Texp_object"
  | Texp_open (desc, expression) -> from_ocaml_open ~desc ~expression
  | Texp_override (_, _) -> Error.todo "Texp_override"
  | Texp_pack { mod_loc = { loc_start = _; loc_end = _; _ }; _ } ->
      Error.todo "Texp_pack"
  | Texp_record _ -> Error.todo "Texp_record"
  | Texp_send
      ({ exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ }, _, _)
    ->
      Error.todo "Texp_send"
  | Texp_sequence (a, b) -> from_ocaml_sequence ~a ~b
  | Texp_setfield
      ( { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ },
        { loc = { loc_start = _; loc_end = _; _ }; _ },
        {
          lbl_res = _;
          lbl_arg = _;
          lbl_loc = { loc_start = _; loc_end = _; _ };
          _;
        },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ } ) ->
      Error.todo "Texp_setfield"
  | Texp_setinstvar
      ( _,
        _,
        { loc = { loc_start = _; loc_end = _; _ }; _ },
        { exp_loc = { loc_start = _; loc_end = _; _ }; exp_type = _; _ } ) ->
      Error.todo "Texp_setinstvar"
  | Texp_try (expr, cases) -> from_ocaml_try ~expr ~cases
  | Texp_tuple parts -> from_ocaml_tuple ~ctx ~parts
  | Texp_unreachable -> from_ocaml_unreachable ()
  | Texp_variant (name, args) -> from_ocaml_variant ~name ~args
  | Texp_while (cond, body) -> from_ocaml_while ~cond ~body

(** Translate an OCaml Constant into an Erlang Term.

    ```ocaml
    1, "hello world"
    ```

    ```erlang
    1, <<"hello world">>
    ```

  *)
and from_ocaml_constant ~constant = Term.from_ocaml_constant ~constant

(** Translate an OCaml Array into Erlang.

    ```ocaml
    let a = [| 1; 2; 3 |] in
    ```

    TODO(@ostera): should we disallow arrays entirely?
  *)
and from_ocaml_array ~parts:_ = Error.todo "from_ocaml_array"

(** Turn an OCaml assertion into an Erlang one.

    ```ocaml
    let e = false in
    assert expr
    ```

    ```erlang
    case E of
      true -> ok;
      _ -> erlang:throw({assert_error, {value, E}})
    end
    ```

    NOTE(@ostera): can we capture more information about the context? Eg.
    filename+loc?
  *)
and from_ocaml_assert ~expr:_ = Error.todo "from_ocaml_assert"

(** Translate an OCaml function application into Erlang.

    TODO(@ostera): ~args will have a list of arguments (more `Texp_*` values)
    and their corresponding labels, if any. I need to play around with more examples
    of this to see how different labeled applications look like and are translated.

    Do we want to collect the arguments in a map?
  *)
and from_ocaml_apply ~ctx ~fn ~args =
  let _fn = from_ocaml_expression ~ctx ~expression:fn in
  let _args =
    let build_arg = function
      | _label, Some arg -> Some (from_ocaml_expression ~ctx ~expression:arg)
      | _label, None -> None
    in
    List.filter_map build_arg args
  in
  Error.todo "from_ocaml_apply"

(** Translate an OCaml value construction that includes a full path to the
    constructor, and any number of arguments required by it.

    ```ocaml
    A.B.Hello (1, "two")
    ```

    ```erlang
    {'Caramel@A.B.Hello', 2, [1, <<"two">>]}
    ```

    NOTE(@ostera): how do GADTs get encoded here? Do we have any relevant
    information to pass on?

    TODO(@ostera): define how the structure of a constructor on Erlang would
    look like. This does not need to be super idiomatic, but we can look into
    Gleam/Purerl/Hamler to see what does encodings look like.
  *)
and from_ocaml_construct ~ctx ~ident ~desc:_ ~exprs =
  let _ident = Identifier.from_longident ~ident in
  let _parts =
    List.map (fun expression -> from_ocaml_expression ~ctx ~expression) exprs
  in
  Error.todo "from_ocaml_construct"
(*
  match Identifier.Well_known.is_well_known ident with
  | `unit -> Erl.Expr.empty_tuple
  | `list -> Erl.Expr.list ~elements:parts
  | `not_well_known ->
      let tag = Erl.Expr.atom (Identifier.constructor ident) in
      let size =
        let size = List.length parts |> Int.to_string in
        let int = Erl.Term.int ~ctx:Erl.Loc.empty size in
        Erl.Expr.term ~term:int
      in
      let parts = Erl.Expr.tuple ~parts in
      Erl.Expr.tuple ~parts:[ tag; size; parts ]
      *)

(** Translate an OCaml function (lambda) into an Erlang function.


    ```ocaml
    let f = fun x y -> x + y in ...
    ```

    ```erlang
    F = fun(X) -> fun(Y) -> X + Y end end,
    ...
    ```

  *)
and from_ocaml_function ~arg_label:_ ~param:_ ~cases:_ ~partial:_ =
  Error.todo "from_ocaml_function"

(** Translate an OCaml identifier into Erlang.

    ```ocaml
    let f () = A.B.c ()
    ```

    ```erlang
    f({}) -> 'Caramel@A.B':c({}).
    ```

  *)
and from_ocaml_identifier ~ident =
  let _ident = Identifier.from_longident ~ident in
  Error.todo "from_ocaml_identifier"
(*
  let ref = Identifier.symbol ident in
  Erl.Expr.symbol ref
  *)

(** Translate an OCaml if .. then .. else .. expression into Erlang:

    ```ocaml
    if a then b else c
    ```

    ```erlang
    if
      a -> b;
      true -> c
    end
    ```
  *)
and from_ocaml_ifthenelse ~cond:_ ~then_expr:_ ~else_expr:_ =
  Error.todo "from_ocaml_ifthenelse"

(** Translate an OCaml let binding into Erlang bindings.

    ```ocaml
    let rec f y = f y in
    let x = 2 in
    f x
    ```

    ```erlang
    Caramel@F = (fun (Fn) -> fun (Y) -> Fn(Fn, Y) end end)(
      fun (Fn, Y) -> Fn(Y) end
    ),
    Caramel@X = 2,
    Caramel@F(Caramel@X).
    ```

    NOTE(@ostera): since there isn't let-rec in Erlang, we need to introduce a
    little lambda that'll allow us to pass a function to itself. This
    transformation should only happen for let-rec bindings.

    TODO(@ostera): to preserve let-scoping we should wrap each scope in a
    lambda.

  *)
and from_ocaml_let ~flags:_ ~binds:_ ~expr:_ = Error.todo "from_ocaml_let"

(** Translate an OCaml match expression into an Erlang case.

    If the match expression is not `~partial`, then we can report a nicer error.

    ```ocaml
    match x with
    | 0 -> true
    | _ -> false
    ```

    ```erlang
    case X of
      0 -> true;
      _ -> false
    end
    ```

  *)
and from_ocaml_match ~expr:_ ~cases:_ ~partial:_ = Error.todo "from_ocaml_match"

(** Handles an OCaml module open by making sure the symbols get properly
    namespaced while they are translated to Erlang.

  *)
and from_ocaml_open ~desc:_ ~expression:_ = Error.todo "from_ocaml_open"

(** Translate the OCaml sequencing operator `;` into the Erlang `,`

    ```ocaml
    a; b
    ```

    ```erlang
    A, B
    ```

  *)
and from_ocaml_sequence ~a:_ ~b:_ = Error.todo "from_ocaml_sequence"

(** Translate an OCaml try expression into an Erlang try-catch

    ```ocaml
    try
      f ()
    with
    | 1 -> true
    | _ -> false
    ```

    ```erlang
    try F({})
    catch
      1 -> true;
      _ -> false
    end
    ```

  *)
and from_ocaml_try ~expr:_ ~cases:_ = Error.todo "from_ocaml_try"

(** Translate an OCaml anonymous tuple into an Erlang tuple.

    ```ocaml
    (1, 2, 3)
    ```

    ```erlang
    {1, 2, 3}
    ```
  *)
and from_ocaml_tuple ~ctx:_ ~parts:_ = Error.todo "from_ocaml_tuple"
(*
  Erl.Expr.tuple
    ~parts:
      (List.map
         (fun expression -> from_ocaml_expression ~ctx ~expression)
         parts)
         *)

(** Translate an Unreachable expression in OCaml to a runtime error in Erlang.

    ```ocaml
    match x with
    | 1 -> true
    | unreachable
    ```

    ```erlang
    case X of
      1 -> true;
      _ -> erlang:throw({unreachable_reached, ...})
    end
    ```

  *)
and from_ocaml_unreachable () = Error.todo "from_ocaml_unreachable"

(** Translate an OCaml polymorphic variant into Erlang.

    ```ocaml
    `hello "joe"
    ```

    ```erlang
    {'Caramel@hello', 1, [<<"joe">>]}
    ```

    TODO(@ostera): see also `from_ocaml_construct`

  *)
and from_ocaml_variant ~name:_ ~args:_ = Error.todo "from_ocaml_variant"

(** Translate an OCaml while expression into an Erlang recursion.

    ```ocaml
    while c do f end
    ```

    ```erlang
    while_helper() ->
      C = fun () -> ... end,
      F = fun () -> ... end,
      loop(C(), C, F).

    loop(true, C, F) -> F(), loop(C(), C, F);
    loop(false, _, _) -> {}.
    ```

    TODO(@ostera): should we build an external helper function that recurses
    over this?

  *)
and from_ocaml_while ~cond:_ ~body:_ = Error.todo "from_ocaml_while"

let from_ocaml_case :
    ctx:Context.t -> case:Typedtree.value Typedtree.case -> Ir.expr_desc =
 fun ~ctx ~case -> from_ocaml_expression ~ctx ~expression:case.c_rhs
