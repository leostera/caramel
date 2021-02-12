open Import

module Kind = struct
  type t =
    | Intf
    | Impl

  type ('intf, 'impl) pair =
    { intf : 'intf
    ; impl : 'impl
    }

  module Map = struct
    type 'a t = ('a, 'a) pair

    let get { intf; impl } = function
      | Impl -> impl
      | Intf -> intf

    let make_both a = { intf = a; impl = a }

    let iter { intf; impl } ~f =
      f intf;
      f impl

    let map { intf; impl } ~f = { intf = f intf; impl = f impl }

    let both (type a b) (x : a t) (y : b t) : (a * b) t =
      { intf = (x.intf, y.intf); impl = (x.impl, y.impl) }
  end
end

module Arg = struct
  type 'e t =
    | Unnamed of 'e
    | Labeled of string * 'e
    | Optional of string * 'e
end

module Type = struct
  [@@@warning "-30"]

  let ident = function
    | "type" -> "type_"
    | "method" -> "method_"
    | "end" -> "end_"
    | s -> s

  type prim =
    | Unit
    | String
    | Int
    | Bool

  type attr =
    | Option
    | Key of string
    | Omitted of string

  type t =
    | Named of string
    | Var of string
    | Prim of prim
    | Tuple of t list
    | Optional of t
    | List of t
    | Poly_variant of constr list
    | Assoc of t * t
    | App of t * t list
    | Fun of t Arg.t * t

  and constr =
    { name : string
    ; args : t list
    }

  and field =
    { name : string
    ; typ : t
    ; attrs : attr list
    }

  type decl =
    | Alias of t
    | Record of field list
    | Variant of constr list

  class virtual ['env, 'm] mapreduce =
    object (self : 'self)
      method virtual empty : 'm

      method virtual plus : 'm -> 'm -> 'm

      method poly_variant env constrs =
        let r, s = self#fold_left_map constrs ~f:(fun c -> self#constr env c) in
        (Poly_variant r, s)

      method tuple (env : 'env) t =
        let (r : t list), s =
          self#fold_left_map t ~f:(fun (t : t) -> self#t env t)
        in
        (Tuple r, s)

      method named _ n = (Named n, self#empty)

      method var _ n = (Var n, self#empty)

      method prim _ p = (Prim p, self#empty)

      method optional env p =
        let t, s = self#t env p in
        (Optional t, s)

      method list env t =
        let t, s = self#t env t in
        (List t, s)

      method assoc env k v =
        let k, s1 = self#t env k in
        let v, s2 = self#t env v in
        (Assoc (k, v), self#plus s1 s2)

      method app env f xs =
        let f, s1 = self#t env f in
        let xs, s2 = self#fold_left_map xs ~f:(fun x -> self#t env x) in
        (App (f, xs), self#plus s1 s2)

      method t env this =
        match (this : t) with
        | Named n -> self#named env n
        | Var v -> self#var env v
        | Prim p -> self#prim env p
        | Tuple t -> self#tuple env t
        | Optional t -> self#optional env t
        | List t -> self#list env t
        | Poly_variant t -> self#poly_variant env t
        | Assoc (k, v) -> self#assoc env k v
        | App (f, xs) -> self#app env f xs
        | Fun (_, _) -> assert false

      method alias env t =
        let r0, s0 = self#t env t in
        (Alias r0, s0)

      method constr env (constr : constr) =
        let args, s =
          self#fold_left_map constr.args ~f:(fun t -> self#t env t)
        in
        ({ constr with args }, s)

      method private fold_left_map
          : 'a. f:('a -> 'a * 'm) -> 'a list -> 'a list * 'm =
        fun ~f xs ->
          let accf, accm =
            List.fold_left xs ~init:([], self#empty) ~f:(fun (accf, accm) x ->
                let r, s = f x in
                (r :: accf, self#plus accm s))
          in
          (List.rev accf, accm)

      method field env f =
        let typ, s = self#t env f.typ in
        ({ f with typ }, s)

      method record env fields =
        let r, s = self#fold_left_map fields ~f:(fun f -> self#field env f) in
        (Record r, s)

      method variant env constrs =
        let v, s = self#fold_left_map constrs ~f:(fun f -> self#constr env f) in
        (Variant v, s)

      method decl env decl =
        match decl with
        | Alias a -> self#alias env a
        | Record fs -> self#record env fs
        | Variant v -> self#variant env v
    end

  let field typ ~name =
    let ident = ident name in
    let attrs =
      if ident = name then
        []
      else
        [ Key name ]
    in
    let attrs =
      match typ with
      | Optional _ -> Option :: attrs
      | _ -> attrs
    in
    { name = ident; typ; attrs }

  let fun_ args t =
    List.fold_right args ~init:t ~f:(fun arg acc -> Fun (arg, acc))

  let constr args ~name = { name; args }

  let list t = List t

  let assoc_list ~key ~data = Assoc (key, data)

  let t = Named "t"

  let module_t m = Named (String.capitalize_ascii m ^ ".t")

  let string = Prim String

  let name s = Named s

  let int = Prim Int

  let bool = Prim Bool

  let alpha = Var "a"

  let enum constrs =
    Variant (List.map constrs ~f:(fun constr -> { name = constr; args = [] }))

  let poly_enum constrs =
    Poly_variant
      (List.map constrs ~f:(fun constr -> { name = constr; args = [] }))

  let json = Named "Json.t"

  let unit = Prim Unit

  let void = Named "Json.Void.t"

  let kind_field ~literal =
    { name = "kind"; typ = void; attrs = [ Omitted literal ] }

  let get_kind f =
    match f.attrs with
    | [ Omitted l ] -> Some l
    | _ -> None

  module Type = W.Type

  let pp_prim (p : prim) : W.t =
    match p with
    | String -> Pp.verbatim "string"
    | Int -> Pp.verbatim "int"
    | Bool -> Pp.verbatim "bool"
    | Unit -> Pp.verbatim "unit"

  let rec pp (a : t) ~(kind : Kind.t) : W.t =
    match a with
    | Prim p -> pp_prim p
    | Var v -> Type.var v
    | Named v -> Type.name v
    | App (f, xs) -> Type.app (pp ~kind f) (List.map ~f:(pp ~kind) xs)
    | Tuple t -> Type.tuple (List.map ~f:(pp ~kind) t)
    | Optional t ->
      let name =
        match (kind, t) with
        | Impl, Named "Json.t"
        | Intf, _ ->
          "option"
        | Impl, _ -> "Json.Nullable_option.t"
      in
      pp ~kind (App (Named name, [ t ]))
    | List t -> pp ~kind (App (Named "list", [ t ]))
    | Poly_variant constrs ->
      List.map constrs ~f:(fun { name; args } ->
          (name, List.map args ~f:(pp ~kind)))
      |> Type.poly
    | Fun (a, r) -> (
      match a with
      | Arg.Unnamed t ->
        Pp.concat
          [ pp t ~kind; Pp.space; Pp.verbatim "->"; Pp.space; pp ~kind r ]
      | Arg.Labeled (l, t) ->
        Pp.concat
          [ Pp.textf "%s:" l
          ; pp t ~kind
          ; Pp.space
          ; Pp.verbatim "->"
          ; Pp.space
          ; pp ~kind r
          ]
      | Arg.Optional (l, t) ->
        Pp.concat
          [ Pp.textf "?%s:" l
          ; pp t ~kind
          ; Pp.space
          ; Pp.verbatim "->"
          ; Pp.space
          ; pp ~kind r
          ])
    | Assoc (k, v) -> (
      match kind with
      | Intf -> pp (List (Tuple [ k; v ])) ~kind
      | Impl -> pp (App (Named "Json.Assoc.t", [ k; v ])) ~kind)

  let pp_decl' ~(kind : Kind.t) (a : decl) =
    match a with
    | Alias a -> (
      let pp = pp ~kind a in
      match (a, kind) with
      | (List _ | Named _ | Prim _), Impl -> W.Type.deriving ~record:false pp
      | _, _ -> pp)
    | Record r -> (
      let r =
        List.filter_map r ~f:(fun { name; typ; attrs } ->
            let open Option.O in
            let+ attrs =
              match attrs with
              | [ Omitted _ ] -> None
              | a -> Some a
            in
            let def =
              let field = pp ~kind typ in
              let attrs =
                let attrs =
                  match kind with
                  | Intf -> []
                  | Impl -> attrs
                in
                List.concat_map attrs ~f:(function
                  | Omitted _ -> assert false
                  | Option ->
                    if typ = Optional json then
                      [ W.Attr.make "yojson.option" [] ]
                    else
                      [ W.Attr.make "default" [ Pp.verbatim "None" ]
                      ; W.Attr.make "yojson_drop_default"
                          [ Pp.verbatim "( = )" ]
                      ]
                  | Key s ->
                    [ W.Attr.make "key" [ Pp.verbatim (sprintf "%S" s) ] ])
              in
              Type.field_attrs ~field ~attrs
            in
            (name, def))
        |> Type.record
      in
      match kind with
      | Intf -> r
      | Impl -> W.Type.deriving r ~record:true)
    | Variant v ->
      List.map v ~f:(fun { name; args } -> (name, List.map ~f:(pp ~kind) args))
      |> Type.variant

  let pp_decl ~name ~kind (a : decl) : W.t =
    let body = pp_decl' ~kind a in
    Type.decl name body
end

module Expr = struct
  [@@@ocaml.warning "-30-32-37"]

  type expr =
    | Let of pat * expr * expr
    | Match of expr * (pat * expr) list
    | Fun of pat Arg.t list * expr
    | App of expr * expr Arg.t list
    | Create of expr prim
    | Assert_false

  and 'e prim =
    | Unit
    | Bool of bool
    | Int of int
    | String of string
    | Ident of string
    | Cons of 'e * 'e prim
    | List of 'e list
    | Tuple of 'e list
    | Record of 'e record_
    | Constr of 'e constr

  and pat =
    | Wildcard
    | Pat of pat prim

  and 'e record_ = (string * 'e) list

  and 'e constr =
    { tag : string
    ; poly : bool
    ; args : 'e list
    }

  type t = expr

  let assert_false_clause = (Wildcard, Assert_false)

  type toplevel =
    { pat : (string Arg.t * Type.t) list
    ; type_ : Type.t
    ; body : t
    }

  let constr ?(poly = false) ?(args = []) tag = { poly; args; tag }

  let pp_constr f { tag; poly; args } =
    let tag =
      let tag = String.capitalize tag in
      Pp.verbatim
        (if poly then
          "`" ^ tag
        else
          tag)
    in
    match args with
    | [] -> tag
    | args ->
      let sep = Pp.verbatim "," in
      let args = W.surround `Paren (Pp.concat_map ~sep ~f args) in
      Pp.concat [ tag; Pp.space; args ]

  let rec pp_pat = function
    | Wildcard -> Pp.verbatim "_"
    | Pat pat -> (
      match pat with
      | Unit -> Pp.verbatim "()"
      | Bool b -> Pp.textf "%b" b
      | Int i -> Pp.textf "%i" i
      | String s -> Pp.textf "%S" s
      | Ident s -> Pp.verbatim s
      | Cons _ -> assert false
      | List _ -> assert false
      | Tuple _ -> assert false
      | Record _ -> assert false
      | Constr c -> pp_constr pp_pat c)

  let rec pp_create : expr prim -> _ Pp.t = function
    | Unit -> Pp.verbatim "()"
    | Bool b -> Pp.textf "%b" b
    | Int i ->
      let pp = Pp.textf "%i" i in
      if i < 0 then
        W.surround `Paren pp
      else
        pp
    | String s -> Pp.textf "%S" s
    | Ident s -> Pp.verbatim s
    | Cons _ -> assert false
    | List xs ->
      let xs = Pp.concat_map xs ~sep:(Pp.verbatim ";") ~f:pp in
      W.surround `Square xs
    | Tuple _ -> assert false
    | Record fields ->
      let record =
        let open Pp.O in
        Pp.concat_map fields
          ~sep:(Pp.verbatim ";" ++ Pp.space)
          ~f:(fun (name, expr) ->
            if expr = Create (Ident name) then
              pp expr
            else
              Pp.verbatim name ++ Pp.space ++ Pp.verbatim "=" ++ pp expr)
      in
      W.surround `Curly record
    | Constr c -> pp_constr pp c

  and pp = function
    | Assert_false -> Pp.verbatim "assert false"
    | Match (expr, patterns) ->
      let with_ =
        Pp.concat
          [ Pp.verbatim "match"
          ; Pp.space
          ; pp expr
          ; Pp.space
          ; Pp.verbatim "with"
          ]
      in
      let clauses =
        Pp.concat_map patterns ~f:(fun (pat, expr) ->
            Pp.concat
              [ Pp.verbatim "| "
              ; pp_pat pat
              ; Pp.space
              ; Pp.verbatim "->"
              ; Pp.space
              ; Pp.verbatim "("
              ; pp expr
              ; Pp.verbatim ")"
              ])
      in
      Pp.concat [ with_; Pp.newline; clauses ]
    | Create c -> pp_create c
    | App (x, args) ->
      let args =
        Pp.concat_map args ~sep:Pp.space ~f:(fun arg ->
            match arg with
            | Unnamed e -> pp e
            | _ -> assert false)
      in
      Pp.concat [ pp x; Pp.space; args ]
    | Fun (pats, expr) ->
      W.surround `Paren
        (Pp.concat
           [ Pp.verbatim "fun"
           ; Pp.space
           ; Pp.concat_map pats ~sep:Pp.space ~f:(fun arg ->
                 match arg with
                 | Unnamed e -> pp_pat e
                 | _ -> assert false)
           ; Pp.space
           ; Pp.verbatim "->"
           ; pp expr
           ])
    | _ -> assert false

  let pp_toplevel ~kind name { pat; type_; body } =
    let pat =
      Pp.concat_map pat ~f:(fun (pat, typ) ->
          let typ = Type.pp ~kind typ in
          match pat with
          | Unnamed s ->
            Pp.concat
              [ Pp.verbatim "("
              ; Pp.verbatim s
              ; Pp.verbatim " : "
              ; typ
              ; Pp.verbatim ")"
              ]
          | Labeled (l, r) ->
            if l = r then
              Pp.concat [ Pp.textf "~(%s :" l; typ; Pp.verbatim ")" ]
            else
              assert false
          | Optional (l, r) ->
            if l = r then
              Pp.concat
                [ Pp.textf "?(%s :" l; typ; Pp.space; Pp.verbatim "option)" ]
            else
              assert false)
    in
    let body = pp body in
    let type_ = Type.pp type_ ~kind in
    Pp.concat
      [ Pp.textf "let %s" name
      ; pat
      ; Pp.textf " : "
      ; type_
      ; Pp.textf "="
      ; Pp.newline
      ; body
      ]
end

module Module = struct
  type 'a t =
    { name : string
    ; bindings : 'a Named.t list
    }

  let empty name = { name; bindings = [] }

  type sig_ =
    | Value of Type.t
    | Type_decl of Type.decl
    | Json_conv_sig

  type impl =
    | Type_decl of Type.decl
    | Value of Expr.toplevel

  let pp_sig { name; bindings } =
    let bindings =
      Pp.concat_map bindings ~sep:Pp.newline ~f:(fun { name; data } ->
          match (data : sig_) with
          | Value t ->
            Pp.concat
              [ Pp.textf "val %s :" name; Pp.space; Type.pp ~kind:Intf t ]
          | Type_decl t ->
            Pp.concat
              [ Pp.textf "type %s =" name
              ; Pp.space
              ; Type.pp_decl' ~kind:Intf t
              ]
          | Json_conv_sig ->
            Pp.textf "include Json.Jsonable.S with type t := %s" name)
    in
    W.Sig.module_ name bindings

  let pp_impl { name; bindings } =
    let bindings =
      Pp.concat_map bindings ~sep:Pp.newline ~f:(fun { name; data = v } ->
          match v with
          | Type_decl t ->
            let lhs = Pp.textf "type %s =" name in
            let rhs = Type.pp_decl' ~kind:Impl t in
            Pp.concat [ lhs; Pp.space; rhs ]
          | Value decl -> Expr.pp_toplevel ~kind:Impl name decl)
    in
    W.module_ name bindings
end
