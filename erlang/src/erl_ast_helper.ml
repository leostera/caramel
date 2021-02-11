open Erl_ast

(* Helpers to work with Atoms *)
module Atom = struct
  let is_keyword str = match str with "and" | "or" -> true | _ -> false

  let quote str = "'" ^ str ^ "'"

  let safe_quote str =
    match str.[0] with
    | 'a' .. 'z' -> if is_keyword str then quote str else str
    | _ -> quote str

  let unquote a =
    match a.[0] with '\'' -> String.sub a 1 (String.length a - 2) | _ -> a

  let mk str = Atom (safe_quote str)

  let to_string (Atom str) = unquote str

  let lowercase (Atom str) = mk (String.lowercase_ascii (unquote str))

  let equal (Atom a) (Atom b) = String.equal a b

  let concat (Atom a) (Atom b) str = mk (unquote a ^ str ^ unquote b)
end

(* Helpers to work with Names *)
module Name = struct
  let var str = Var_name (String.capitalize_ascii str)

  let atom a = Atom_name a

  let qualified ~m:n_mod ~f:n_name = Qualified_name { n_mod; n_name }

  let rec to_string n =
    match n with
    | Atom_name a -> Atom.to_string a
    | Var_name n -> n
    | Qualified_name { n_mod; n_name } ->
        Printf.sprintf "%s:%s" (to_string n_mod) (to_string n_name)
end

(* Helpers to work with Literals *)
module Const = struct
  let integer str = Lit_integer str

  let char str = Lit_char str

  let binary str = Lit_binary str

  let string str = Lit_string str

  let float str =
    match str.[String.length str - 1] == '.' with
    | false -> Lit_float str
    | true -> Lit_float (str ^ "0")

  let atom a = Lit_atom a
end

(* Helpers to work with Expressions *)
module Expr = struct
  let apply fa_name fa_args = Expr_apply { fa_name; fa_args }

  let case expr cases = Expr_case (expr, cases)

  let cons head tail = Expr_cons (head, tail)

  let const lit = Expr_literal lit

  let fun_ ~cases = Expr_fun cases

  (* FIXME: not yet fixed modelling arity in functino references *)
  let fun_ref name ~arity =
    Expr_fun_ref { fref_name = name; fref_arity = arity }

  let ident name = Expr_name name

  let let_ binding body = Expr_let (binding, body)

  let let_bind lb_lhs lb_rhs = { lb_lhs; lb_rhs }

  let list xs = Expr_list xs

  let map_field key value = { mf_name = key; mf_value = value }

  let map kvs = Expr_map kvs

  let map_update m kvs = Expr_map_update (m, kvs)

  let nil = Expr_nil

  let recv ~cases ~after = Expr_recv { rcv_cases = cases; rcv_after = after }

  let tuple parts = Expr_tuple parts

  let comment c e = Expr_comment (c, e)

  let try_ try_expr ~catch ~after =
    Expr_try { try_expr; try_catch = catch; try_after = after }

  let catch expr = Expr_catch expr

  let if_ ~clauses = Expr_if clauses

  let macro name = Expr_macro name
end

(* Helpers to work with Patterns *)
module Pat = struct
  let any = Pattern_ignore

  let bind name = Pattern_binding name

  let tuple parts = Pattern_tuple parts

  let list parts = Pattern_list parts

  let cons init tail = Pattern_cons (init, tail)

  let map kvs = Pattern_map kvs

  let with_name p1 p2 = Pattern_with_name (p1, p2)

  let const literal = Pattern_match literal

  let catch ?(class_ = None) ?(stacktrace = None) pat =
    Pattern_catch (class_, pat, stacktrace)
end

(* Helpers to work with Functions *)
module FunDecl = struct
  let mk ~name ?(spec = None) ~cases =
    let main_case = List.hd cases in
    {
      fd_name = name;
      fd_arity = List.length main_case.c_lhs;
      fd_cases = cases;
      fd_spec = spec;
    }

  let case ~lhs ?(guard = None) ~rhs =
    { c_lhs = lhs; c_guard = guard; c_rhs = rhs }
end

(* Helpers to work with Types *)
module Type = struct
  let mk ?(kind = Type) ?(params = []) ~name ~expr =
    { typ_expr = expr; typ_name = name; typ_kind = kind; typ_params = params }

  let apply ?(args = []) ~name = Type_constr { tc_name = name; tc_args = args }

  let fun_ ?(args = []) ~return =
    Type_function { tyfun_args = args; tyfun_return = return }

  let rec all_named_vars typ_expr =
    match typ_expr with
    | Type_const _ -> []
    | Type_variable (Var_name n) -> [ n ]
    | Type_variable _ -> []
    | Type_constr { tc_name = _; tc_args = args } ->
        List.concat_map all_named_vars args
    | Type_list p -> all_named_vars p
    | Type_tuple parts -> List.concat_map all_named_vars parts
    | Type_map fields -> List.concat_map all_named_vars_map_field fields
    | Type_record (_, fields) ->
        List.concat_map all_named_vars_record_field fields
    | Type_variant tks -> List.concat_map all_named_vars tks
    | Type_function { tyfun_args = args; tyfun_return = return } ->
        List.concat_map all_named_vars (return :: args)

  and all_named_vars_map_field
      { tmf_name = e1; tmf_presence = _; tmf_value = e2 } =
    List.concat_map all_named_vars [ e1; e2 ]

  and all_named_vars_record_field { rf_name = _; rf_type = t } =
    all_named_vars t

  let rec clean_vars occurs typ_expr =
    match typ_expr with
    | Type_const l -> Type_const l
    | Type_variable (Var_name n)
      when List.length (List.find_all (( = ) n) occurs) < 2 ->
        Type_variable (Var_name "_")
    | Type_variable n -> Type_variable n
    | Type_constr { tc_name = n; tc_args = args } ->
        Type_constr { tc_name = n; tc_args = List.map (clean_vars occurs) args }
    | Type_list p -> Type_list (clean_vars occurs p)
    | Type_tuple parts -> Type_tuple (List.map (clean_vars occurs) parts)
    | Type_map fields ->
        Type_map (List.map (clean_vars_map_field occurs) fields)
    | Type_record (n, fields) ->
        Type_record (n, List.map (clean_vars_record_field occurs) fields)
    | Type_variant tks -> Type_variant (List.map (clean_vars occurs) tks)
    | Type_function { tyfun_args = args; tyfun_return = return } ->
        Type_function
          {
            tyfun_args = List.map (clean_vars occurs) args;
            tyfun_return = clean_vars occurs return;
          }

  and clean_vars_map_field occurs
      { tmf_name = n; tmf_presence = p; tmf_value = t } =
    {
      tmf_name = clean_vars occurs n;
      tmf_presence = p;
      tmf_value = clean_vars occurs t;
    }

  and clean_vars_record_field occurs { rf_name = n; rf_type = t } =
    { rf_name = n; rf_type = clean_vars occurs t }

  let clean_unbound_named_vars typ_expr =
    clean_vars (all_named_vars typ_expr) typ_expr

  let record name fields = Type_record (name, fields)

  let map_field ?(presence = Mandatory) tmf_name tmf_value =
    { tmf_presence = presence; tmf_name; tmf_value }

  let map fields = Type_map fields

  let var name = Type_variable name

  let const lit = Type_const lit

  let variant types = Type_variant types

  let tuple parts = Type_tuple parts

  let list t = Type_list t

  let field rf_name rf_type = { rf_name; rf_type }

  let opaque = Opaque

  let type_ = Type

  let spec = Spec

  let callback = Callback

  let any = apply ~args:[] ~name:(Name.atom (Atom.mk "any"))
end

module Export = struct
  let mk ~kind ~name ~arity =
    { exp_type = kind; exp_name = name; exp_arity = arity }

  let fun_ name ~arity = mk ~kind:Export_function ~name ~arity

  let type_ name ~arity = mk ~kind:Export_type ~name ~arity
end

(* Helpers to work with Modules *)
module Mod = struct
  let empty =
    {
      file_name = "empty_module.erl";
      behaviours = [];
      module_name = Atom.mk "empty_module";
      attributes = [];
      exports = [];
      types = [];
      functions = [];
    }

  let mk ?(attributes = []) ?(behaviours = []) ?(exports = []) ?(functions = [])
      ?(types = []) module_name =
    {
      file_name = Atom.to_string module_name ^ ".erl";
      behaviours;
      module_name;
      attributes;
      exports;
      types;
      functions;
    }

  let update x acc =
    match x with
    | Module_comment _ -> acc
    | Module_attribute
        {
          atr_name = Atom "module";
          atr_value = Expr_literal (Lit_atom module_name);
        } ->
        let file_name = Atom.to_string module_name ^ ".erl" in
        { acc with module_name; file_name }
    | Module_attribute
        {
          atr_name = Atom "behavior";
          atr_value = Expr_literal (Lit_atom behavior);
        }
    | Module_attribute
        {
          atr_name = Atom "behaviour";
          atr_value = Expr_literal (Lit_atom behavior);
        } ->
        { acc with behaviours = behavior :: acc.behaviours }
    | Module_attribute { atr_name = Atom "export"; atr_value = Expr_list attrs }
      ->
        {
          acc with
          exports =
            (attrs
            |> List.filter_map (function
                 | Expr_tuple
                     [
                       Expr_literal (Lit_atom name);
                       Expr_literal (Lit_integer arity);
                     ] ->
                     let arity = int_of_string arity in
                     Some (Export.fun_ name ~arity)
                 | _ -> None))
            @ acc.exports;
        }
    | Module_attribute
        { atr_name = Atom "export_type"; atr_value = Expr_list attrs } ->
        {
          acc with
          exports =
            (attrs
            |> List.filter_map (function
                 | Expr_tuple
                     [
                       Expr_literal (Lit_atom name);
                       Expr_literal (Lit_integer arity);
                     ] ->
                     let arity = int_of_string arity in
                     Some (Export.type_ name ~arity)
                 | _ -> None))
            @ acc.exports;
        }
    | Module_attribute atr -> { acc with attributes = atr :: acc.attributes }
    | Type_decl td -> { acc with types = td :: acc.types }
    | Function_decl fd -> { acc with functions = fd :: acc.functions }

  let rec item_list_to_module items acc =
    match items with
    | [] -> acc
    | x :: xs -> item_list_to_module xs (update x acc)

  let of_structure items =
    match List.rev items with
    | [
     Module_attribute
       {
         atr_name = Atom "module";
         atr_value = Expr_literal (Lit_atom module_name);
       };
    ] ->
        mk module_name
    | xs -> item_list_to_module xs empty
end

let find_fun_by_name mod_ name =
  mod_.functions |> List.find_opt (fun { fd_name; _ } -> fd_name = name)
