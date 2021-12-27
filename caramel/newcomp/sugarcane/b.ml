open Sexplib.Std

type fn_name = string * int [@@deriving sexp]

type fn = { args : string list; body : t } [@@deriving sexp]

and pattern =
  | Pat_ignore
  | Pat_atom of string
  | Pat_nil
  | Pat_cons of pattern * pattern
  | Pat_bind of string
  | Pat_tuple of pattern list
  | Pat_int of string
  | Pat_float of string
  | Pat_char of char
  | Pat_binary of string
[@@deriving sexp]

and literal =
  | Lit_atom of string
  | Lit_char of char
  | Lit_cons of literal * literal
  | Lit_float of string
  | Lit_int of string
  | Lit_nil
  | Lit_tuple of literal list
[@@deriving sexp]

and def = { df_name : fn_name; df_body : t } [@@deriving sexp]

and t =
  | Apply of { fn : t; args : t list }
  | Binary of string
  | Call of { mod_ : string; fun_ : string; args : t list }
  | Case of { cond : t; cases : (pattern * t) list }
  | Catch of t
  | Fun of fn
  | Fun_ref of fn_name
  | Let of { value_list : string list; expr : t; body : t }
  | Let_rec of { bindings : (fn_name * fn) list; body : t }
  | List of t * t
  | Literal of literal
  | Map of (t * t) list
  | Module of { name : string; defs : def list; exports : fn_name list }
  | Prim_op of { name : string; args : t list }
  | Receive of { cases : (pattern * t) list; after_cond : t; after_body : t }
  | Seq of t * t
  | Try of {
      expr : t;
      try_value_list : string list;
      body : t;
      catch_value_list : string list;
      catch_expr : t;
    }
  | Tuple of t list
  | Value_list of string list
  | Var of string
[@@deriving sexp]

(*******************************************************************************)

let pp ppf t =
  let sexp = sexp_of_t t in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

(*******************************************************************************)

let apply ~fn ~args = Apply { fn; args }

let atom atom = Literal (Lit_atom atom)

let binary part = Binary part

let call ~mod_ ~fun_ ~args = Call { mod_; fun_; args }

let case ~cond ~cases = Case { cond; cases }

let catch e1 e2 =
  Try
    {
      expr = e1;
      try_value_list = [ "Result" ];
      body = Var "Result";
      catch_value_list = [ "_a"; "_b"; "_c" ];
      catch_expr = e2;
    }

let char char = Literal (Lit_char char)

let cons ~head ~tail = List (head, tail)

let def ~name ~arity ~body = { df_name = (name, arity); df_body = body }

let float float = Literal (Lit_float float)

let fun_ ~args ~body = Fun { args; body }

let fun_name ~name ~arity = (name, arity)

let fun_ref fn_name = Fun_ref fn_name

let int i = Literal (Lit_int i)

let let_ ~value_list ~expr ~body = Let { value_list; expr; body }

let map ~fields = Map fields

let module_ ~name ~defs ~exports = Module { name; defs; exports }

let nil = Literal Lit_nil

let pat_atom atom = Pat_atom atom

let pat_binary str = Pat_binary str

let pat_char char = Pat_char char

let pat_float float = Pat_float float

let pat_ignore = Pat_ignore

let pat_int int = Pat_int int

let pat_tuple parts = Pat_tuple parts

let pat_var name = Pat_bind name

let pat_nil = Pat_nil

let seq a b = Seq (a, b)

let tuple ~parts = Tuple parts

let var name = Var name
