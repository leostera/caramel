open Erl_parsetree

module Pp_utils = struct
  let sep_by char ppf () = Format.fprintf ppf char

  let newline = sep_by "\n"

  let comma = sep_by ","

  let wrap l r ppf fn =
    Format.fprintf ppf "%s" l;
    fn ppf;
    Format.fprintf ppf "%s" r
end

module Atom = struct
  let pp ppf (Atom (_ctx, atom)) = Format.fprintf ppf "%s" atom
end

module Name = struct
  let pp ppf name =
    match name with
    | Name_var (_ctx, var) -> Format.fprintf ppf "%s" var
    | Name_atom atom -> Atom.pp ppf atom
end

module Attr = struct
  let pp ppf attr =
    match attr with
    | Export_type { attr_type_name; attr_type_arity } ->
        Format.fprintf ppf "-export_type([%a/%d])." Atom.pp attr_type_name
          attr_type_arity
    | Export_fun { attr_fun_name; attr_fun_arity } ->
        Format.fprintf ppf "-export([%a/%d])." Atom.pp attr_fun_name
          attr_fun_arity
    | Custom { attr_name = _; attr_value = _ } -> ()
end

module Pattern = struct
  let rec pp ppf pattern =
    match pattern with
    | Pat_ignore -> Format.fprintf ppf "_"
    | Pat_atom atom -> Atom.pp ppf atom
    | Pat_float (_, n) | Pat_integer (_, n) -> Format.fprintf ppf "%s" n
    | Pat_tuple { ptup_elements; _ } ->
        Pp_utils.wrap "{" "}" ppf (fun ppf ->
            Format.pp_print_list ~pp_sep:Pp_utils.comma pp ppf ptup_elements)
    | Pat_binary_string (_, _)
    | Pat_char (_, _)
    | Pat_list _ | Pat_map _ | Pat_record _ | Pat_string _ | Pat_var _ ->
        ()
end

module Term = struct
  let pp ppf term =
    match term with
    | Term_atom atom -> Atom.pp ppf atom
    | Term_char (_ctx, char) -> Format.fprintf ppf "'%c'" char
    | Term_integer (_ctx, int) -> Format.fprintf ppf "%s" int
    | Term_binary_string (_ctx, _) | Term_float (_ctx, _) -> ()
    | Term_list _ | Term_tuple _ | Term_map _ | Term_record _ | Term_string _ ->
        ()
end

module Expr = struct
  let pp ppf expr =
    match expr with
    | Expr_var name -> Name.pp ppf name
    | Expr_term term -> Term.pp ppf term
    | Expr_case (_, _)
    | Expr_macro_app _ | Expr_fun_call _ | Expr_lambda _ | Expr_match _ ->
        ()
end

module Case = struct
  let pp ~name ppf case =
    Format.fprintf ppf "%a(" Atom.pp name;
    Format.pp_print_list ~pp_sep:Pp_utils.comma Pattern.pp ppf case.c_lhs;
    Format.fprintf ppf ") -> ";
    Format.fprintf ppf "%a" Expr.pp case.c_rhs
end

module Fun_decl = struct
  let pp ppf ({ fd_name; fd_cases; _ } as _fun_decl) =
    Format.pp_print_list ~pp_sep:(Pp_utils.sep_by ";\n") (Case.pp ~name:fd_name)
      ppf fd_cases;
    Format.fprintf ppf ".\n"
end

module Mod = struct
  let pp ppf { module_name; attributes; functions; _ } =
    Format.fprintf ppf "%% Source code generated with Caramel.\n";
    Format.fprintf ppf "-module(%a).\n" Atom.pp module_name;
    Format.fprintf ppf "%% Module attributes:\n";
    Format.pp_print_list ~pp_sep:Pp_utils.newline Attr.pp ppf attributes;
    Format.fprintf ppf "\n\n";
    Format.fprintf ppf "%% Function declarations:\n";
    Format.pp_print_list ~pp_sep:Pp_utils.newline Fun_decl.pp ppf functions;
    Format.fprintf ppf "\n\n%!"
end
