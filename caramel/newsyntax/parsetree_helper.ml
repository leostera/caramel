open Parsetree

let id str = Id (String.split_on_char '.' str)

let id_of_path parts = Id parts

let id_kind (Id path) =
  let first = List.nth path 0 in
  let last = List.nth path (List.length path - 1) in

  let first_is_lower =
    match String.get first 0 with 'a' .. 'z' -> true | _ -> false
  in
  let last_is_upper =
    match String.get last 0 with 'A' .. 'Z' -> true | _ -> false
  in

  match path with
  (* path = ["hello", _] *)
  | id :: path :: rest when first_is_lower -> `field_access (id, path, rest)
  (* path = [_, "World"] *)
  | _ when last_is_upper -> `constructor
  (* path = ["hello"] *)
  | _ -> `value

module Visibility = struct
  let pub = Public

  let priv = Private
end

module Expr = struct
  let nil = Expr_nil

  let let_ ~pat ~body ~expr = Expr_let (pat, body, expr)

  let lambda ~args ~body = Expr_lambda (args, body)

  let var id = Expr_var id

  let tuple ~parts = Expr_tuple parts

  let field ~name ~expr = (name, expr)

  let field_access ~expr ~field = Expr_field (expr, field)

  let parse_field_access name field path =
    let rec consume_path path =
      match path with
      | [] -> field_access ~expr:(var (id name)) ~field:(id field)
      | f :: rest -> field_access ~expr:(consume_path rest) ~field:(id f)
    in
    consume_path path

  let record ~fields = Expr_record fields

  let constructor_record ~name ~fields =
    Expr_constructor (name, Ctr_record fields)

  let constructor_tuple ~name ~parts = Expr_constructor (name, Ctr_tuple parts)

  let case ~lhs ~rhs = { cs_lhs = lhs; cs_rhs = rhs }

  let match_ ~expr ~cases = Expr_match (expr, cases)

  let list ~head ~tail = Expr_cons (head, tail)

  let lit_str s = Expr_literal (Lit_string s)

  let lit_atom a = Expr_literal (Lit_atom a)

  let lit_int i = Expr_literal (Lit_integer i)

  let call ~name ~args = Expr_call (name, args)

  let seq a b = Expr_seq (a, b)

  let open_ ~mod_name ~expr = Expr_open (mod_name, expr)
end

module Typ = struct
  let id name = Type_name name

  let var name = Type_var name

  let arrow a b = Type_arrow (a, b)

  let tuple ~parts = Type_tuple parts

  let apply ~id ~args = Type_apply (id, args)
end

module Type = struct
  let abstract = Type_abstract

  let variant ~constructors = Type_variant { tyk_constructors = constructors }

  let record ~labels = Type_record { tyk_labels = labels }

  let alias ~name = Type_alias name

  let variant_constructor ~name ~args ~annot =
    { ctr_name = name; ctr_args = args; ctr_annot = annot }

  let variant_record_args ~labels = Record labels

  let variant_tuple_args ~parts = Tuple parts

  let label_decl ~name ~type_ ~annot =
    { lbl_name = name; lbl_type = type_; lbl_annot = annot }

  let mk ~name ~args ~desc ~annot =
    { typ_name = name; typ_args = args; typ_desc = desc; typ_annot = annot }
end

module Pat = struct
  let any = Pat_any

  let nil = Pat_nil

  let bind name = Pat_bind name

  let tuple ~parts = Pat_tuple parts

  let field ~name ~pattern = (name, pattern)

  let record ~fields = Pat_record fields

  let constructor_tuple ~name ~parts = Pat_constructor (name, Ctp_tuple parts)

  let constructor_record ~name ~fields =
    Pat_constructor (name, Ctp_record fields)

  let list ~head ~tail = Pat_cons (head, tail)

  let lit_atom a = Pat_literal (Lit_atom a)

  let lit_str s = Pat_literal (Lit_string s)
end

module Annot = struct
  let field ~name ~value = (name, value)

  let map ~fields = Map fields

  let mk ~name ~desc = { ann_name = name; ann_desc = desc }
end

module Fun_decl = struct
  let mk ~name ~visibility ~annot ~args ~body =
    {
      fn_name = name;
      fn_visibility = visibility;
      fn_annot = annot;
      fn_args = args;
      fn_arity = List.length args;
      fn_body = body;
    }
end

module Ext = struct
  let mk ~name ~type_sig ~symbol ~annot ~visibility =
    {
      ext_name = name;
      ext_type = type_sig;
      ext_symbol = symbol;
      ext_annot = annot;
      ext_visibility = visibility;
    }
end

module Macro = struct
  let quoted_expr expr = Expr_quote (Quoted_expr expr)

  let quoted_str_item item = Expr_quote (Quoted_str item)

  let unquoted ~expr = Expr_unquote expr
end

module Mod = struct
  let decl ~name ~items ~annot ~visibility =
    Mod_decl
      {
        mod_name = name;
        mod_items = items;
        mod_annot = annot;
        mod_visibility = visibility;
      }

  let open_ ~mod_name = Mod_open mod_name
end

module Str = struct
  let type_ t = Str_type t

  let fun_ fn = Str_fun fn

  let extern e = Str_extern e

  let macro fn = Str_macro fn

  let mod_expr m = Str_mod_expr m

  let comment c = Str_comment c

  let annotations item =
    match item with
    | Str_type t -> t.typ_annot
    | Str_fun f -> f.fn_annot
    | Str_extern e -> e.ext_annot
    | _ -> []
end

module Mapper = struct
  let map_str ?typ ?fn ?ext ?macro ?module_ ?comment str =
    List.map
      (fun item ->
        match item with
        | Str_type i -> Str_type (match typ with Some f -> f i | _ -> i)
        | Str_fun i -> Str_fun (match fn with Some f -> f i | _ -> i)
        | Str_extern i -> Str_extern (match ext with Some f -> f i | _ -> i)
        | Str_macro i -> Str_macro (match macro with Some f -> f i | _ -> i)
        | Str_mod_expr i ->
            Str_mod_expr (match module_ with Some f -> f i | _ -> i)
        | Str_comment i ->
            Str_comment (match comment with Some f -> f i | _ -> i))
      str

  (*
     TODO(leandro): figure out how we want to use this

  let rec map_expr ?record ?constructor ?lambda ?open_ ?call ?let_ ?cons
      ?literal ?match_ ?nil ?seq ?tuple ?var ?quote ?unquote expr =
    let record = match record with Some f -> f | _ -> fun x -> x in
    let constructor = match constructor with Some f -> f | _ -> fun x -> x in
    let lambda = match lambda with Some f -> f | _ -> fun x -> x in
    let open_ = match open_ with Some f -> f | _ -> fun x -> x in
    let call = match call with Some f -> f | _ -> fun x -> x in
    let let_ = match let_ with Some f -> f | _ -> fun x -> x in
    let cons = match cons with Some f -> f | _ -> fun x -> x in
    let literal = match literal with Some f -> f | _ -> fun x -> x in
    let match_ = match match_ with Some f -> f | _ -> fun x -> x in
    let nil = match nil with Some f -> f | _ -> fun x -> x in
    let seq = match seq with Some f -> f | _ -> fun x -> x in
    let tuple = match tuple with Some f -> f | _ -> fun x -> x in
    let var = match var with Some f -> f | _ -> fun x -> x in
    let quote = match quote with Some f -> f | _ -> fun x -> x in
    let unquote = match unquote with Some f -> f | _ -> fun x -> x in

    let self expr =
      map_expr ~record ~constructor ~lambda ~open_ ~call ~let_ ~cons ~literal
        ~match_ ~nil ~seq ~tuple ~var ~quote ~unquote expr
    in

    match expr with
    | Expr_lambda (args, body) -> Expr.lambda ~args ~body:(lambda body)
    | Expr_call  ->
        let (name, args) = self fn in
        let args = List.map self args in
        Expr.call ~name ~args
    | Expr_cons (a, b) -> Expr.list ~head:(self a) ~tail:(self b)
    | Expr_quote (Quoted_expr expr) -> Macro.quoted_expr (self expr)
    | Expr_quote _ -> expr
    | Expr_unquote expr -> Macro.unquoted ~expr:(self expr)
    | Expr_match (expr, cases) ->
        Expr.match_ ~expr:(self expr)
          ~cases:
            (List.map
               (fun { cs_lhs; cs_rhs } -> { cs_lhs; cs_rhs = self cs_rhs })
               cases)
    | Expr_let (pat, body, expr) ->
        Expr.let_ ~pat ~body:(self body) ~expr:(self expr)
    | Expr_seq (a, b) -> Expr.seq (self a) (self b)
    | Expr_tuple parts -> Expr.tuple ~parts:(List.map self parts)
    | Expr_constructor (name, Ctr_tuple parts) ->
        Expr.constructor_tuple ~name ~parts:(List.map self parts)
    | Expr_constructor (name, Ctr_record fields) ->
        Expr.constructor_record ~name
          ~fields:(List.map (fun (k, v) -> (k, self v)) fields)
    | Expr_record fields ->
        Expr.record ~fields:(List.map (fun (k, v) -> (k, self v)) fields)
    | Expr_open (mod_name, expr) ->
        let mod_name, expr = open_ (mod_name, expr) in
        Expr.open_ ~mod_name ~expr
    | Expr_literal lit -> Expr_literal (literal lit)
    | Expr_var id -> Expr_var (var id)
    | Expr_nil -> Expr_nil
    *)
end
