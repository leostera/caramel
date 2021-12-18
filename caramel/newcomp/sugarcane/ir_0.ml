open Lambda
open Caramel_misc

let rec ir_of_lambda ~unit code =
  match code with
  | Lapply apply -> of_lambda_apply ~unit apply
  | Lassign (_id, _body) -> Error.todo_lambda code
  | Lconst c -> of_lambda_const ~unit c
  | Levent (next, event) -> of_lambda_event ~unit next event
  | Lfor (_, _, _, _, _) -> Error.todo_lambda code
  | Lfunction fn -> of_lambda_function ~unit fn
  | Lifthenelse (cond, t, f) -> of_lambda_ifthenelse ~unit cond t f
  | Lifused (_, _) -> Error.todo_lambda code
  | Llet (kind, Pgenval, id, expr, body) ->
      of_lambda_let ~unit kind id expr body
  | Llet (_, _, _, _, _) -> Error.todo_lambda code
  | Lletrec (bindings, body) -> of_lambda_letrec ~unit bindings body
  | Lmutlet (_, _, _, _) -> Error.todo_lambda code
  | Lmutvar _ -> Error.todo_lambda code
  | Lprim (prim, exprs, loc) -> of_lambda_primitive ~unit prim exprs loc
  | Lsend (_, _, _, _, _) -> Error.todo_lambda code
  | Lsequence (e1, e2) -> of_lambda_seq ~unit e1 e2
  | Lstaticcatch (e1, (_, _), e2) -> of_lambda_catch ~unit e1 e2
  | Lstaticraise (code, args) -> of_lambda_raise ~unit code args
  | Lstringswitch (e1, cases, e2, _loc) ->
      of_lambda_stringswitch ~unit e1 cases e2
  | Lswitch (e1, cases, _) -> of_lambda_switch ~unit e1 cases
  | Ltrywith (_, _, _) -> Error.todo_lambda code
  | Lvar id -> of_lambda_var ~unit id
  | Lwhile (_, _) -> Error.todo_lambda code

and of_lambda_apply ~unit { ap_func; ap_args; _ } =
  let fn = ir_of_lambda ~unit ap_func in
  let args = List.map (ir_of_lambda ~unit) ap_args in
  Ir.apply ~fn ~args

and of_lambda_const ~unit c =
  match c with
  | Const_base c' -> Literal.of_const c' |> Ir.lit
  | Const_block (_size, parts) ->
      let fields =
        List.mapi (fun idx p -> (idx, of_lambda_const ~unit p)) parts
      in
      Ir.record ~fields
  | Const_float_array _ -> Error.todo "Const_float_array"
  | Const_immstring str -> Literal.string str |> Ir.lit

(**
    Lambda events are used to describe whether modules should be defined, and
    thus include some useful information about the structure of the program
    that needs to be leveraged.
*)
and of_lambda_event ~unit lambda event =
  match event.lev_kind with
  | Lev_module_definition id ->
      Ir.module_ ~name:(Identifier.of_ident id)
        ~expr:(ir_of_lambda ~unit lambda)
  | _ -> ir_of_lambda ~unit lambda

and of_lambda_function ~unit { params; body; _ } =
  let body = ir_of_lambda ~unit body in
  let args = List.map (fun (id, _) -> Identifier.of_ident id) params in
  Ir.fun_ ~args ~body

and of_lambda_ifthenelse ~unit cond then_expr else_expr =
  Ir.if_ ~cond:(ir_of_lambda ~unit cond)
    ~then_expr:(ir_of_lambda ~unit then_expr)
    ~else_expr:(ir_of_lambda ~unit else_expr)

and of_lambda_let ~unit _kind id expr body =
  Ir.let_ ~id:(Identifier.of_ident id) ~expr:(ir_of_lambda ~unit expr)
    ~body:(ir_of_lambda ~unit body)

and of_lambda_letrec ~unit bindings body =
  let bindings =
    List.map
      (fun (id, expr) -> (Identifier.of_ident id, ir_of_lambda ~unit expr))
      bindings
  in
  Ir.letrec ~bindings ~body:(ir_of_lambda ~unit body)

and of_lambda_primitive ~unit prim args _loc =
  Primitive.of_lambda_prim ~unit prim (List.map (ir_of_lambda ~unit) args)

and of_lambda_seq ~unit e1 e2 =
  let e1 = ir_of_lambda ~unit e1 in
  let e2 = ir_of_lambda ~unit e2 in
  Ir.seq e1 e2

and of_lambda_catch ~unit e1 e2 =
  let e1 = ir_of_lambda ~unit e1 in
  let e2 = ir_of_lambda ~unit e2 in
  Ir.catch e1 e2

and of_lambda_raise ~unit code args =
  let args = List.map (ir_of_lambda ~unit) args in
  Ir.throw ~code ~args

and of_lambda_stringswitch ~unit e1 cases e2 =
  let cond = ir_of_lambda ~unit e1 in
  let cases =
    List.map
      (fun (str, e3) ->
        (Ir.pat_lit (Literal.string str), ir_of_lambda ~unit e3))
      cases
  in
  let default =
    match e2 with
    | Some e2 -> [ (Ir.pat_ignore, ir_of_lambda ~unit e2) ]
    | None -> []
  in
  Ir.case ~cond ~cases:(cases @ default)

and of_lambda_switch ~unit e1 { sw_blocks; sw_consts; _ } =
  let cond = ir_of_lambda ~unit e1 in
  let cases =
    List.map
      (fun (idx, exp) -> (Ir.pat_lit (Literal.int idx), ir_of_lambda ~unit exp))
      (sw_consts @ sw_blocks)
  in
  Ir.case ~cond ~cases

and of_lambda_var ~unit:_ id = Ir.var (Identifier.of_ident id)

let ir_of_unit Translation_unit.({ program = _, lambda; _ } as unit) =
  let name =
    Identifier.module_name_of_string
      (Compilation_unit.module_name unit.comp_unit)
  in
  let ir =
    Ir.program [ Ir.module_ ~name ~expr:(ir_of_lambda ~unit lambda.code) ]
  in
  { unit with ir }
