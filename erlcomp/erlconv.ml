open Typedtree
open Types

exception Function_without_body
exception Unsupported_feature

let rec varname_of_string s =
  let name = s |> String.capitalize_ascii in
  match String.get name 0, name with
  | '_', name when name <> "_" ->
      let name = name |> String.to_seq |> List.of_seq |> List.tl |> List.to_seq |> String.of_seq in
      "_" ^ (varname_of_string name)
  | _, _ -> name

let atom_of_string = String.lowercase_ascii

let atom_of_ident i = i |> Ident.name |> atom_of_string
let varname_of_ident i = i |> Ident.name |> varname_of_string


(** Build the actual functions of an Erlang module
 *)
let build_functions: Typedtree.structure -> Erlast.fun_decl list =
  fun typedtree ->
    let rec build_expression exp =
      match exp.exp_desc with
      | Texp_ident (_, {txt}, _) ->
          Some (Erlast.Exp_name (Longident.last txt |> varname_of_string))


      | Texp_construct ({ txt }, _, _expr)  when Longident.last txt = "[]" ->
          Some (Erlast.Exp_list [])

      | Texp_construct ({ txt }, _, _expr)  when Longident.last txt = "()" ->
          Some (Erlast.Exp_tuple [])

      (* NOTE: use `extended_expression` to provide map overrides *)
      | Texp_record { fields; } ->
          Some (Erlast.Exp_map (fields |> Array.to_list |> List.map (fun (field, value) ->
            let value = match value with
            | Kept _ -> raise Unsupported_feature
            | Overridden (_, exp) -> begin match build_expression exp with
                | None -> raise Unsupported_feature
                | Some v -> v
                end
            in
            Erlast.{ mf_name = field.lbl_name; mf_value = value }
          )))

      | Texp_tuple exprs ->
          Some (Erlast.Exp_tuple (exprs |> List.filter_map build_expression))

      | _ -> None

      (*
  | Texp_constant of constant -> Some (Erlast.Exp_literal )
        (** 1, 'a', "true", 1.0, 1l, 1L, 1n *)

  | Texp_let of rec_flag * value_binding list * expression
        (** let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
            let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
         *)
  | Texp_function of { arg_label : arg_label; param : Ident.t;
      cases : value case list; partial : partial; }
        (** [Pexp_fun] and [Pexp_function] both translate to [Texp_function].
            See {!Parsetree} for more details.

            [param] is the identifier that is to be used to name the
            parameter of the function.

            partial =
              [Partial] if the pattern match is partial
              [Total] otherwise.
         *)
  | Texp_apply of expression * (arg_label * expression option) list
        (** E0 ~l1:E1 ... ~ln:En

            The expression can be None if the expression is abstracted over
            this argument. It currently appears when a label is applied.

            For example:
            let f x ~y = x + y in
            f ~y:3

            The resulting typedtree for the application is:
            Texp_apply (Texp_ident "f/1037",
                        [(Nolabel, None);
                         (Labelled "y", Some (Texp_constant Const_int 3))
                        ])
         *)
  | Texp_match of expression * computation case list * partial
        (** match E0 with
            | P1 -> E1
            | P2 | exception P3 -> E2
            | exception P4 -> E3

            [Texp_match (E0, [(P1, E1); (P2 | exception P3, E2);
                              (exception P4, E3)], _)]
         *)
  | Texp_try of expression * value case list
        (** try E with P1 -> E1 | ... | PN -> EN *)
  | Texp_construct of
      Longident.t loc * Types.constructor_description * expression list
        (** C                []
            C E              [E]
            C (E1, ..., En)  [E1;...;En]
         *)
  | Texp_variant of label * expression option
  | Texp_field of expression * Longident.t loc * Types.label_description
  | Texp_setfield of
      expression * Longident.t loc * Types.label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * Parsetree.pattern * expression * expression * direction_flag *
        expression
  | Texp_send of expression * meth * expression option
  | Texp_new of Path.t * Longident.t loc * Types.class_declaration
  | Texp_instvar of Path.t * Path.t * string loc
  | Texp_setinstvar of Path.t * Path.t * string loc * expression
  | Texp_override of Path.t * (Path.t * string loc * expression) list
  | Texp_letmodule of
      Ident.t option * string option loc * Types.module_presence * module_expr *
        expression
  | Texp_letexception of extension_constructor * expression
  | Texp_assert of expression
  | Texp_lazy of expression
  | Texp_object of class_structure * string list
  | Texp_pack of module_expr
  | Texp_letop of {
      let_ : binding_op;
      ands : binding_op list;
      param : Ident.t;
      body : value case;
      partial : partial;
    }
  | Texp_unreachable
  | Texp_extension_constructor of Longident.t loc * Path.t
  | Texp_open of open_declaration * expression
        (** let open[!] M in e *)


        *)


    in

    let rec build_pattern pat =
      match pat.pat_desc with
      | Tpat_var (id, _) ->
          let var_name = id |> varname_of_ident in
          Erlast.Pattern_binding var_name

      | Tpat_tuple tuples ->
          Erlast.Pattern_tuple (List.map build_pattern tuples)

      | Tpat_record (fields, _) ->
          Erlast.Pattern_map (fields |> List.map (fun (Asttypes.{txt}, _, pattern) ->
            (atom_of_string (Longident.last txt), build_pattern pattern)
          ))

      | Tpat_construct ({ txt }, _, patterns) when Longident.last txt = "::" ->
          Erlast.Pattern_list (List.map build_pattern patterns)

      | Tpat_construct ({ txt }, _, _patterns) ->
          Erlast.Pattern_match (Longident.last txt |> atom_of_string)

      (* NOTE: here's where the translation of pattern
       * matching at the function level should happen. *)
      | _ ->
          Erlast.Pattern_ignore
    in

    let build_value vb =
      match vb.vb_pat.pat_desc, vb.vb_expr.exp_desc with
      | Tpat_var (id, _), Texp_function { cases; } ->
          let rec params c acc =
            let param = build_pattern c.c_lhs in
            let acc' = param :: acc in
            match c.c_rhs.exp_desc with
            | Texp_function { cases = [c']; } -> params c' acc'
            | _ -> acc' |> List.rev
          in

          let fd_name = id |> atom_of_ident in
          let fd_arity = match cases with
            | [] -> 0
            | c :: _ -> (params c []) |> List.length
          in
          let fd_cases = cases |> List.map (fun case ->
            (* NOTE: we'll just traverse all the expressions in this case and
             * make sure we collapse as many top-level arguments for this function.
             *)
            let rec body c =
              match c.c_rhs.exp_desc with
              | Texp_function { cases = [c']; } -> body c'
              | _ -> begin match build_expression c.c_rhs with
                | Some exp -> exp
                | _ -> raise Function_without_body
              end
            in

            let fc_lhs = params case [] in
            let fc_rhs = body case in
            let fc_guards = [] in
            Erlast.{ fc_lhs; fc_guards; fc_rhs }
          ) in Some Erlast.{ fd_name; fd_arity; fd_cases }

      | _ -> None
    in

    typedtree.str_items
    |> (List.fold_left (fun acc item  ->
        match item.str_desc with
        | Tstr_value (_, vb) ->
            (List.filter_map build_value vb) @ acc
        | _ -> acc
    ) [])
    |> List.rev

(** Build the types of an Erlang module.
 *)
let build_types:
  Typedtree.structure
  -> Erlast.type_decl list =
  fun typedtree ->
    let rec build_type_kind core_type =
      match core_type.ctyp_desc with
      | Ttyp_any -> Some (Erlast.type_any)

      | Ttyp_var var_name -> Some (Erlast.Type_variable (var_name |> varname_of_string))

      (* NOTE: OCaml works with functions from one type to another, and supports
       * multiargument functions via currying or tuples.
       *
       * Erlang doesn't, so we'll squash the function type into a single function
       * with multiple arguments instead.
       *)
      | Ttyp_arrow (_, param, out) ->
          let rec args t acc = match t.ctyp_desc with
            | Ttyp_arrow (_, p, t') -> args t' (p :: acc)
            | _ -> (t :: acc) |> List.rev
          in
          let args = (args out [param]) |> List.filter_map build_type_kind in
          Some (Erlast.Type_function args)


      (* NOTE: this allows us to export type aliases that may have been made
       * opaque, such as `type opaque = string`, as `-type opaque() :: string().`
       *
       * It is also used for application of type constructors: `type a = string list`
       * gets compiled to `-type a() :: list(string()).`
       *)
      | Ttyp_constr (_, { txt; }, args) ->
          let tc_name = Longident.last txt |> atom_of_string in
          let tc_args = args |> List.filter_map build_type_kind in
          Some (Erlast.Type_constr { tc_name; tc_args})

      | Ttyp_tuple els ->
          let parts = (els |> List.filter_map build_type_kind) in
          Some (Erlast.Type_tuple parts)

      | Ttyp_variant  (rows, _closed, _labels) ->
          let rec all_rows rs acc =  match rs with
            | [] -> acc |> List.rev
            | r :: rs' -> match r.rf_desc with
              | Tinherit _ctype -> all_rows rs' acc
              | Ttag ( {txt}, _, core_types) ->
                  let vc_name = txt |> atom_of_string in
                  let vc_args = (core_types |> List.filter_map build_type_kind) in
                  let variant = Erlast.{ vc_name; vc_args } in
                  all_rows rs' (variant :: acc)
          in
          let constructors = all_rows rows [] in
          Some (Erlast.Type_variant {constructors})

      (* NOTE: these are two core type constructors that are essentially "links"
       * to follow.
       *
       * The second one `Ttyp_poly (strings, core_typ)` seemed to appear in records.
       *)
      | Ttyp_alias (follow, _)
      | Ttyp_poly (_, follow) -> build_type_kind follow

      | Ttyp_object _
      | Ttyp_class _
      | Ttyp_package _ -> None
    in

    let build_record labels =
      let fields = labels |> List.map(fun Typedtree.{ ld_id; ld_type } ->
        let rf_name = atom_of_ident ld_id in
        let rf_type = match build_type_kind ld_type with
          | Some t -> t
          | None -> Erlast.type_any
        in Erlast.{ rf_name; rf_type })
      in Erlast.Type_record {fields}
    in

    let build_abstract name params _type_decl core_type =
      match build_type_kind core_type with
      | Some kind -> Some (Erlast.make_named_type name params kind)
      | None -> None
    in

    let build_type_params params =
      params
      |> List.filter_map (fun (core_type, _) ->
          match core_type.ctyp_desc with
          | Ttyp_var name -> Some (name |> varname_of_string)
          | _ -> None
      )
    in

    let build_type type_decl =
      let name = (atom_of_ident type_decl.typ_id) in
      let params = build_type_params type_decl.typ_params in
      match type_decl.typ_kind with

      (* NOTE: turns out that "abstract" here means "only structure, no names!"
       * so this branch will generate the appropriate types for tuples, aliases
       * and actual abstract types.
       * *)
      | Ttype_abstract  ->
          begin match type_decl.typ_manifest with
          | Some abs -> (build_abstract name params type_decl abs)
          | None ->
              let ref = (Erlast.Type_constr { tc_name="ref"; tc_args=[]}) in
              Some (Erlast.make_named_type name params ref)
          end

      | Ttype_record labels ->
          let record = build_record labels in
          Some (Erlast.make_named_type name params (record))

      | Ttype_variant constructors ->
          let constructors = constructors
          |> List.map(fun Typedtree.{ cd_id; cd_args } ->
              let vc_args = match cd_args with
                | Cstr_tuple core_types -> core_types |> List.filter_map build_type_kind
                | Cstr_record labels -> [build_record labels]
              in
              Erlast.{ vc_name = atom_of_ident cd_id; vc_args } )
          in Some (Erlast.make_named_type name params (Erlast.Type_variant {constructors}))

      | _ -> None
    in
    typedtree.str_items
    |> (List.concat_map (fun item  ->
        match item.str_desc with
        | Tstr_type (_, tys)  -> tys
        | _ -> []
    ))
    |> List.filter_map build_type

(** Build the exports table of an Erlang module

    This will look for the signature of the module to determine what to export.

    The type-chain looks like this:
      * Typedtree.structure
      * Types.signature
      * types.signature_item
 *)
let build_exports:
  name:string
  -> Typedtree.structure
  -> Types.signature option
  -> Erlast.export list =
  fun ~name:_ typedtree signature ->
    let rec value_arity = fun value count ->
      match value.desc with
      | Tarrow (_, _, next, _) -> value_arity next (count + 1)
      | Tlink t -> value_arity t count
      | _ -> count
    in

    let signature = match signature with
      | None -> typedtree.str_type
      | Some x -> x
    in

    signature |> (List.filter_map (fun sig_item ->
      match sig_item  with
      | Sig_value (name, { val_type }, Exported) ->
          Some (Erlast.make_fn_export (atom_of_ident name) (value_arity val_type 0))
      | Sig_type (name, { type_arity }, _, Exported) ->
          Some (Erlast.make_type_export (atom_of_ident name) type_arity)
      | _  -> None
    ))

(** Build a single Erlang module from a Typedtree.structure, and an optionally
    constraining Types.signature.
 *)
let build_module: name:string -> Typedtree.structure -> Types.signature option -> Erlast.t =
  fun ~name typedtree signature ->
    let exports = build_exports ~name typedtree signature in
    let types = build_types typedtree in
    let functions = build_functions typedtree in
    Erlast.make ~name ~exports ~types ~functions

(** Navigate a [Typedtree.structure] and recursively collect all module definitions,
    building up the right prefixed names.

    The type-chain looks like this:
      * Typedtree.structure
      * Typedtree.structure_item
      * Typedtree.structure_item_desc
      * Typedtree.module_binding
      * Typedtree.module_expr
      * Typedtree.module_expr_desc
      * Typedtree.structure -> back to the top again
 *)
let rec find_modules:
  prefix:string
  -> Typedtree.structure
  -> (string * Typedtree.structure * (Types.signature option)) list =
  fun ~prefix typedtree ->
    let module_name prefix mb_id = (match mb_id with
          | Some x -> prefix ^ "_" ^ (atom_of_ident x)
          | None -> prefix) |> String.lowercase_ascii
    in
    typedtree.str_items
    |> (List.fold_left (fun acc struct_item ->
        let mbs = (match struct_item.str_desc with
            | Tstr_module mb -> [mb]
            | Tstr_recmodule mbs ->  mbs
            | _ -> [])
            |> (List.concat_map (fun mb ->
                let prefix = module_name prefix mb.mb_id in
                match mb.mb_expr.mod_desc with
                  | Tmod_constraint ({ mod_desc = Tmod_structure typedtree },
                                     Mty_signature signature,
                                     _mod_type_constr,
                                     _mod_type_coerc) ->
                      (prefix, typedtree, Some signature) :: (find_modules ~prefix typedtree)
                  | Tmod_structure typedtree -> (prefix, typedtree, None) :: (find_modules ~prefix typedtree)
                  | _ -> []
                )) in
        List.concat [mbs; acc]
    ) [])


(** Turn an OCaml Typedtree into a list of Erlang ASTs that can be compiled to
    sources.
*)
let from_typedtree:
  name:string
  -> Typedtree.structure
  -> (Types.signature option)
  -> Erlast.t list =
  fun ~name typedtree signature ->
    let name = atom_of_string name in
    [
      (find_modules ~prefix:name typedtree)
      |> List.map( fun (name, impl, sign) -> build_module ~name impl sign );

      [ build_module ~name typedtree signature ];
    ] |> List.concat
