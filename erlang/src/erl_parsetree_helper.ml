open Erl_parsetree

(* Helpers to work with Locations *)
module Loc = struct
  let empty = { txt = ""; pos = (0, 0) }

  let mk ~txt ~pos = { txt; pos }
end

module Term = struct
  let atom atom = Term_atom atom

  let int ~ctx int = Term_integer (ctx, int)

  let float ~ctx float = Term_float (ctx, float)
end

(* Helpers to work with Atoms *)
module Atom = struct
  let quote str = "'" ^ str ^ "'"

  let safe_quote str =
    match str.[0] with
    | 'a' .. 'z' -> if Erl_syntax.is_keyword str then quote str else str
    | _ -> quote str

  let unquote a =
    match a.[0] with '\'' -> String.sub a 1 (String.length a - 2) | _ -> a

  let mk ~ctx str = Atom (ctx, safe_quote str)

  let to_string (Atom (_, str)) = unquote str

  let lowercase (Atom (_, str)) = mk (String.lowercase_ascii (unquote str))

  let equal (Atom (_, a)) (Atom (_, b)) = String.equal a b

  let concat (Atom (ctx, a)) (Atom (_, b)) str =
    mk ~ctx (unquote a ^ str ^ unquote b)
end

module Name = struct
  let var ~ctx ~name = Name_var (ctx, name)

  let mf ~ctx ~m ~f =
    Name_qualified_name { qn_ctx = ctx; qn_mod = m; qn_fun = f }
end

module Pat = struct
  let ignore = Pat_ignore

  let bind ~name = Pat_var name

  let tuple ~parts =
    Pat_tuple { ptup_size = List.length parts; ptup_elements = parts }

  let empty_tuple = tuple ~parts:[]
end

module Expr = struct
  let term ~term = Expr_term term

  let atom a = term ~term:(Term_atom a)

  let var ~name = Expr_var name
end

module Case = struct
  let mk ~lhs ~rhs = { c_lhs = lhs; c_rhs = rhs }
end

module Fun_decl = struct
  let mk ~name ~arity ~cases =
    { fd_name = name; fd_arity = arity; fd_cases = cases }
end

module Attr = struct
  let export_type ~name ~arity =
    Export_type { attr_type_name = name; attr_type_arity = arity }

  let export ~name ~arity =
    Export_fun { attr_fun_name = name; attr_fun_arity = arity }
end

module Mod = struct
  let mk ~mod_ctx ~ctx ~attributes ~behaviours ~functions ~file_name
      ~module_name =
    { attributes; behaviours; functions; file_name; module_name; ctx; mod_ctx }
end

let ok = Expr.atom (Atom.mk ~ctx:Loc.empty "ok")
