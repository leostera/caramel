open Erl_parsetree

(* Helpers to work with Locations *)
module Loc = struct
  let empty = { txt = ""; pos = (0, 0) }

  let mk ~txt ~pos = { txt; pos }
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

module Mod = struct
  let mk ~mod_ctx ~ctx ~attributes ~behaviours ~functions ~file_name
      ~module_name =
    { attributes; behaviours; functions; file_name; module_name; ctx; mod_ctx }
end
