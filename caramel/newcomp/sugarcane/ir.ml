open Sexplib.Std

type pattern = P_ignore | P_lit of Literal.t [@@deriving sexp]

type t =
  | Ir_apply of t * t list
  | Ir_case of t * (pattern * t) list
  | Ir_catch of t * t
  | Ir_ext_call of (string * string) * t list
  | Ir_field of int * t
  | Ir_fun of Identifier.t list * t
  | Ir_let of Identifier.t * t * t
  | Ir_letrec of (Identifier.t * t) list * t
  | Ir_lit of Literal.t
  | Ir_module of Identifier.t * t
  | Ir_program of t list
  | Ir_record of (int * t) list
  | Ir_seq of t * t
  | Ir_throw of int * t list
  | Ir_var of Identifier.t
[@@deriving sexp]

(*******************************************************************************)

let pp ppf t =
  let sexp = sexp_of_t t in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

(*******************************************************************************)

let apply ~fn ~args = Ir_apply (fn, args)

let case ~cond ~cases = Ir_case (cond, cases)

let catch e1 e2 = Ir_catch (e1, e2)

let empty = Ir_program []

let ext_call ~name ~args = Ir_ext_call (name, args)

let field ~idx ~expr = Ir_field (idx, expr)

let fun_ ~args ~body = Ir_fun (args, body)

let if_ ~cond ~then_expr ~else_expr =
  case ~cond
    ~cases:[ (P_lit Literal.t, then_expr); (P_lit Literal.f, else_expr) ]

let let_ ~id ~expr ~body = Ir_let (id, expr, body)

let letrec ~bindings ~body = Ir_letrec (bindings, body)

let lit l = Ir_lit l

let module_ ~name ~expr = Ir_module (name, expr)

let pat_ignore = P_ignore

let pat_lit lit = P_lit lit

let program mods = Ir_program mods

let record ~fields = Ir_record fields

let seq e1 e2 = Ir_seq (e1, e2)

let throw ~code ~args = Ir_throw (code, args)

let var id = Ir_var id
