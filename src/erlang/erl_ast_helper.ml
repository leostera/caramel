open Erl_ast

(* Helpers to work with Atoms *)
module Atom = struct
  let quote str = "'" ^ str ^ "'"

  let unquote a =
    match a.[0] with
    | '\'' -> String.sub a 1 ((String.length a) - 2)
    | _ -> a

  let mk str =
    let atom = match str.[0] with
      | 'a' .. 'z' -> String.lowercase_ascii str
      | _ -> quote str
    in
    Atom atom

  let to_string (Atom str) = unquote str

  let equal (Atom a) (Atom b) = String.equal a b

  let concat (Atom a) (Atom b) str =
    mk ((unquote a) ^ str ^ (unquote b))
end

(* Helpers to work with Names *)
module Name = struct
  let var str = Var_name (String.capitalize_ascii str)

  let atom str = Atom_name (Atom.mk str)

  let qualified ~module_name:n_mod n_name = Qualified_name { n_mod; n_name }

  let to_string n =
    match n with
    | Atom_name a -> Atom.to_string a
    | Var_name n -> n
    | Qualified_name { n_mod; n_name } ->
        Printf.sprintf "%s:%s" (Atom.to_string n_mod) (Atom.to_string n_name)

  let ignore = var "_"
end

(* Helpers to work with Literals *)
module Const = struct
  let integer str = Lit_integer str

  let char str = Lit_char str

  let binary str = Lit_binary str

  let float str = Lit_float str

  let atom a = Lit_atom a
end

(* Helpers to work with Expressions *)
module Expr = struct
  let apply fa_name fa_args = Expr_apply { fa_name; fa_args }

  let case expr cases = Expr_case (expr, cases)

  let cons head tail = Expr_cons (head, tail)

  let const lit = Expr_literal lit

  let field key value = { mf_name = key; mf_value = value }

  let fun_ ~cases = Expr_fun cases

  (* FIXME: not yet fixed modelling arity in functino references *)
  let fun_ref name ~arity =
    Expr_fun_ref { fref_name = name; fref_arity = arity }

  let ident name = Expr_name name

  let let_ binding body = Expr_let (binding, body)

  let let_bind lb_lhs lb_rhs = { lb_lhs; lb_rhs }

  let list xs = Expr_list xs

  let map kvs = Expr_map kvs

  let nil = Expr_nil

  let recv ~cases ~after = Expr_recv { rcv_cases = cases; rcv_after = after }

  let tuple parts = Expr_tuple parts
end

(* Helpers to work with Patterns *)
module Pat = struct
  let any = Pattern_ignore

  let bind name = Pattern_binding name

  let tuple parts = Pattern_tuple parts

  let list parts = Pattern_list parts

  let cons init tail = Pattern_cons (init, tail)

  let map kvs = Pattern_map kvs

  let const literal = Pattern_match literal
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
  let mk ?(visibility = Visible) ?(params = []) ~name ~kind =
    {
      typ_kind = kind;
      typ_name = name;
      typ_visibility = visibility;
      typ_params = params;
    }

  let apply ?(args = []) ~name = Type_constr { tc_name = name; tc_args = args }

  let fun_ ?(args = []) ~return =
    Type_function { tyfun_args = args; tyfun_return = return }

  let record tyrec_fields = Type_record { tyrec_fields }

  let var name = Type_variable name

  let variant tyvar_constructors = Type_variant { tyvar_constructors }

  let tuple parts = Type_tuple parts

  let constr ?(args = []) ~name = Constructor { tc_name = name; tc_args = args }

  let extension type_kind = Extension type_kind

  let field rf_name rf_type = { rf_name; rf_type }

  let opaque = Opaque

  let visible = Visible

  let any = apply ~args:[] ~name:(Name.atom "any")
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
      file_name = "caramel_empty_module.erl";
      behaviours = [];
      module_name = Atom.mk "caramel_empty_module";
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
            ( attrs
            |> List.filter_map (function
                 | Expr_tuple
                     [
                       Expr_literal (Lit_atom name);
                       Expr_literal (Lit_integer arity);
                     ] ->
                     let arity = int_of_string arity in
                     Some (Export.fun_ name ~arity)
                 | _ -> None) )
            @ acc.exports;
        }
    | Module_attribute
        { atr_name = Atom "export_type"; atr_value = Expr_list attrs } ->
        {
          acc with
          exports =
            ( attrs
            |> List.filter_map (function
                 | Expr_tuple
                     [
                       Expr_literal (Lit_atom name);
                       Expr_literal (Lit_integer arity);
                     ] ->
                     let arity = int_of_string arity in
                     Some (Export.type_ name ~arity)
                 | _ -> None) )
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
    match (List.rev items) with
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
