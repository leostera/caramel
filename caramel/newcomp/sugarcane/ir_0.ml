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
  | Lifthenelse (cond, t, f, meta) -> of_lambda_ifthenelse ~unit cond t f meta
  | Lifused (_, _) -> Error.todo_lambda code
  | Llet (kind, _, id, expr, body) -> of_lambda_let ~unit kind id expr body
  | Lletrec (bindings, body) -> of_lambda_letrec ~unit bindings body
  | Lmutlet (_, _, _, _) -> Error.todo_lambda code
  | Lmutvar _ -> Error.todo_lambda code
  | Lprim (prim, exprs, _loc) -> of_lambda_primitive ~unit prim exprs
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

and of_lambda_const ~unit c = Constant.of_lambda_const ~unit c

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

and of_lambda_ifthenelse ~unit cond then_expr else_expr meta =
  let cond = ir_of_lambda ~unit cond in
  let then_expr = ir_of_lambda ~unit then_expr in
  let else_expr = ir_of_lambda ~unit else_expr in
  match meta with
  | Some cstrs ->
      (* NOTE: when optimizing a pattern match down to an ifthenelse, OCaml will
         pick an "empty variant" to use as the condition of the if because
         that constructor will typically be represented with the integer value 0.

         This means that that constructor can be used as `true` as well.
      *)
      let zero_pat =
        List.fold_left
          (fun acc cstr -> if cstr.cm_arity == 0 then Some cstr else acc)
          None cstrs
      in

      let zero_pat =
        match zero_pat with
        | None -> Ir.pat_nil
        | Some cstr -> (
            let id = Identifier.of_ident cstr.cm_name in
            let tag = Identifier.to_string id |> String.lowercase_ascii in
            (* NOTE: more idiomatically, we would like to allow pattern matching
               on the BEAM empty list construct rather than an atom.
            *)
            match tag with
            | "::" | "[]" -> Ir.pat_nil
            | _ -> Ir.pat_lit (Literal.atom tag))
      in
      Ir.case ~cond ~cases:[ (zero_pat, else_expr); (Ir.pat_ignore, then_expr) ]
  | None ->
      Ir.case ~cond
        ~cases:
          [
            (Ir.pat_lit (Literal.atom "true"), then_expr);
            (Ir.pat_ignore, else_expr);
          ]

and of_lambda_let ~unit _kind id expr body =
  (* NOTE: when dealing with modules that are included, we get Llet's that have
     for identifier "include", but their expressions themselves are lets.
  *)
  match (Ident.name id, expr) with
  | "include", Llet (kind, _, id, expr, body) ->
      of_lambda_let ~unit kind id expr body
  | _ ->
      Ir.let_ ~visibility:Private ~id:(Identifier.of_ident id)
        ~expr:(ir_of_lambda ~unit expr) ~body:(ir_of_lambda ~unit body)

and of_lambda_letrec ~unit bindings body =
  let bindings =
    List.map
      (fun (id, expr) ->
        (Ir.Private, Identifier.of_ident id, ir_of_lambda ~unit expr))
      bindings
  in
  Ir.letrec ~bindings ~body:(ir_of_lambda ~unit body)

and of_lambda_primitive ~unit prim args =
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

and of_lambda_switch ~unit e1 { sw_blocks; sw_consts; sw_metadata; _ } =
  let cond = ir_of_lambda ~unit e1 in
  match sw_metadata with
  | Some cstrs ->
      (* lets build a table to quickly look up the constructors by index *)
      let cstr_tbl =
        let tbl = Hashtbl.create 128 in
        List.iteri (Hashtbl.add tbl) cstrs;
        tbl
      in

      let cases =
        List.map
          (fun (idx, exp) ->
            let cstr = Hashtbl.find cstr_tbl idx in
            let id = Identifier.of_ident cstr.cm_name in
            let parts =
              List.init (cstr.cm_arity + 1) (function
                | 0 ->
                    Identifier.to_string id |> String.lowercase_ascii
                    |> Literal.atom |> Ir.pat_lit
                | 1 -> Ir.pat_ignore
                | i -> Ir.pat_bind ("_" ^ Int.to_string i))
            in
            (Ir.pat_tuple parts, ir_of_lambda ~unit exp))
          (sw_consts @ sw_blocks)
      in

      Ir.case ~cond ~cases
  | _ ->
      let cases =
        List.map
          (fun (idx, exp) ->
            (Ir.pat_lit (Literal.int idx), ir_of_lambda ~unit exp))
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
