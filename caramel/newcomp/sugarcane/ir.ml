open Sexplib.Std

type pattern =
  | P_tuple of pattern list
  | P_bind of string
  | P_nil
  | P_ignore
  | P_lit of Literal.t
[@@deriving sexp]

type visibility = Private | Exported [@@deriving sexp]

type record_kind = List | Record | Tuple [@@deriving sexp]

type t =
  | Ir_apply of t * t list
  | Ir_case of t * (pattern * t) list
  | Ir_catch of t * t
  | Ir_ext_call of (string * string) * t list
  | Ir_field of int * t option * t
  | Ir_fun of Identifier.t list * t
  | Ir_let of visibility * Identifier.t * t * t
  | Ir_letrec of (visibility * Identifier.t * t) list * t
  | Ir_lit of Literal.t
  | Ir_nil
  | Ir_cons of t * t
  | Ir_module of Identifier.t * t
  | Ir_program of t list
  | Ir_record of { fields : (t * t) list }
  | Ir_seq of t * t
  | Ir_throw of int * t list
  | Ir_tuple of t list
  | Ir_var of Identifier.t
  | Ir_fn_name of Identifier.t * int
[@@deriving sexp]

(*******************************************************************************)

let pp ppf t =
  let sexp = sexp_of_t t in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

(*******************************************************************************)

let apply ~fn ~args = Ir_apply (fn, args)

let case ~cond ~cases = Ir_case (cond, cases)

let catch e1 e2 = Ir_catch (e1, e2)

let cons head tail = Ir_cons (head, tail)

let empty = Ir_program []

let ext_call ~name ~args = Ir_ext_call (name, args)

let field ~idx ~field ~expr = Ir_field (idx, field, expr)

let fun_ ~args ~body = Ir_fun (args, body)

let if_ ~cond ~then_expr ~else_expr =
  case ~cond ~cases:[ (P_lit Literal.t, then_expr); (P_ignore, else_expr) ]

let let_ ~visibility ~id ~expr ~body = Ir_let (visibility, id, expr, body)

let letrec ~bindings ~body = Ir_letrec (bindings, body)

let lit l = Ir_lit l

let module_ ~name ~expr = Ir_module (name, expr)

let nil = Ir_nil

let pat_bind var = P_bind var

let pat_ignore = P_ignore

let pat_nil = P_nil

let pat_lit lit = P_lit lit

let pat_tuple parts = P_tuple parts

let program mods = Ir_program mods

let record ~fields = Ir_record { fields }

let tuple ~parts = Ir_tuple parts

let seq e1 e2 = Ir_seq (e1, e2)

let throw ~code ~args = Ir_throw (code, args)

let var id = Ir_var id

let fn_name ~name ~arity = Ir_fn_name (name, arity)
