open Import

module Literal = struct
  type t =
    | String of string
    | Int of int
    | Float of float
end

module Enum = struct
  type t = (string * Literal.t) list
end

module type S = sig
  type ident

  type field_def =
    | Single of
        { optional : bool
        ; typ : typ
        }
    | Pattern of
        { pat : typ
        ; typ : typ
        }

  and field = field_def Named.t

  and typ =
    | Literal of Literal.t
    | Ident of ident
    | Sum of typ list
    | List of typ
    | Record of field list
    | Tuple of typ list
    | App of typ * typ

  and interface =
    { extends : ident list
    ; fields : field list
    ; params : string list
    }

  and decl =
    | Interface of interface
    | Type of typ
    | Enum_anon of Enum.t

  and t = decl Named.t

  class map :
    object
      method typ : typ -> typ

      method interface : interface -> interface

      method field : field -> field

      method t : t -> t
    end

  class ['a] fold :
    object
      method field : field -> init:'a -> 'a

      method ident : ident -> init:'a -> 'a

      method t : t -> init:'a -> 'a

      method typ : typ -> init:'a -> 'a
    end
end

module Make (Ident : sig
  type t
end) =
struct
  type field_def =
    | Single of
        { optional : bool
        ; typ : typ
        }
    | Pattern of
        { pat : typ
        ; typ : typ
        }

  and field = field_def Named.t

  and typ =
    | Literal of Literal.t
    | Ident of Ident.t
    | Sum of typ list
    | List of typ
    | Record of field list
    | Tuple of typ list
    | App of typ * typ

  and interface =
    { extends : Ident.t list
    ; fields : field list
    ; params : string list
    }

  and decl =
    | Interface of interface
    | Type of typ
    | Enum_anon of Enum.t

  and t = decl Named.t

  class ['a] fold =
    object (self)
      method t (t : t) ~init =
        match t.data with
        | Interface (i : interface) ->
          let init =
            List.fold_left i.extends ~init ~f:(fun acc e ->
                self#ident e ~init:acc)
          in
          List.fold_left ~init i.fields ~f:(fun init f -> self#field f ~init)
        | Type (t : typ) -> self#typ t ~init
        | Enum_anon _ -> init

      method ident _ ~init = init

      method field (f : field) ~init : 'a =
        match f.data with
        | Single { optional = _; typ } -> self#typ ~init typ
        | Pattern { pat; typ } ->
          let init = self#typ ~init pat in
          self#typ ~init typ

      method typ (t : typ) ~init =
        match t with
        | Literal _ -> init
        | Ident i -> self#ident i ~init
        | App (t1, t2) ->
          let init = self#typ t1 ~init in
          self#typ t2 ~init
        | List t -> self#typ t ~init
        | Tuple typs
        | Sum typs ->
          List.fold_left typs ~init ~f:(fun init f -> self#typ f ~init)
        | Record fs ->
          List.fold_left fs ~init ~f:(fun init f -> self#field f ~init)
    end

  class map =
    object (self)
      method field (f : field) =
        let data =
          match f.data with
          | Single s ->
            let typ = self#typ s.typ in
            Single { s with typ }
          | Pattern { pat; typ } ->
            let pat = self#typ pat in
            let typ = self#typ typ in
            Pattern { pat; typ }
        in
        { f with data }

      method interface (i : interface) =
        let fields = List.map ~f:self#field i.fields in
        { i with fields }

      method typ (t : typ) =
        match t with
        | Literal i -> Literal i
        | Ident i -> Ident i
        | App (x, y) ->
          let x = self#typ x
          and y = self#typ y in
          App (x, y)
        | List t -> List (self#typ t)
        | Tuple ts -> Tuple (List.map ts ~f:self#typ)
        | Sum ts -> Sum (List.map ts ~f:self#typ)
        | Record ts -> Record (List.map ts ~f:self#field)

      method t (t : t) =
        let data =
          match t.data with
          | Interface i -> Interface (self#interface i)
          | Type t -> Type (self#typ t)
          | Enum_anon _ -> t.data
        in
        { t with data }
    end
end

module Unresolved = struct
  include Make (String)

  let enum ~name ~constrs : Enum.t Named.t = { Named.name; data = constrs }

  let interface ~name ~extends ~fields ~params : interface Named.t =
    { Named.name; data = { extends; fields; params } }

  let pattern_field ~name ~pat ~typ =
    { Named.name; data = Pattern { pat; typ } }

  let named_field ?(optional = false) typ name =
    { Named.name; data = Single { optional; typ } }
end

module type Prim_intf = sig
  type resolved

  type t =
    | Null
    | String
    | Bool
    | Number
    | Any
    | Object
    | List
    | Self
    | Resolved of resolved

  val of_string : string -> resolve:(string -> t) -> t
end

module Prim_make (Resolved : sig
  type t
end) =
struct
  type t =
    | Null
    | String
    | Bool
    | Number
    | Any
    | Object
    | List
    | Self
    | Resolved of Resolved.t

  let of_string s ~resolve =
    match String.lowercase_ascii s with
    | "null" -> Null
    | "string" -> String
    | "boolean" -> Bool
    | "number" -> Number
    | "any" -> Any
    | "array" -> List
    | "object" -> Object
    | _ -> resolve s
end

module rec Resolved : (S with type ident := Prim.t) = Make (Prim)
and Prim : (Prim_intf with type resolved := Resolved.t) = Prim_make (Resolved)

let subst unresolved =
  object
    val params = String.Map.empty

    val inside = None

    method inside s = {<inside = Some s>}

    method resolve n =
      match String.Map.find params n with
      | Some [] -> assert false
      | Some (x :: _) -> `Resolved x
      | None ->
        if inside = Some n then
          `Self
        else
          `Unresolved (String.Map.find_exn unresolved n)

    method push x y =
      let params =
        String.Map.update params x ~f:(function
          | None -> Some [ y ]
          | Some [] -> assert false
          | Some (y' :: xs) ->
            if y = y' then
              Some xs
            else
              Some (y :: y' :: xs))
      in
      {<params>}

    method pop x =
      let params =
        String.Map.update params x ~f:(function
          | None ->
            ignore (String.Map.find_exn params x);
            None
          | Some [] -> assert false
          | Some (_ :: xs) -> Some xs)
      in
      {<params>}
  end

let rec resolve_all ts ~(names : Unresolved.t String.Map.t) : Resolved.t list =
  let names = subst names in
  List.map ts ~f:(resolve ~names)

and resolve (t : Unresolved.t) ~names : Resolved.t =
  let data : Resolved.decl =
    match t.data with
    | Interface i -> Interface (resolve_interface { t with data = i } ~names)
    | Type t -> Type (resolve_type t ~names)
    | Enum_anon a -> Enum_anon a
  in
  { t with Named.data }

and resolve_ident i ~names =
  Prim.of_string i ~resolve:(fun s ->
      match names#resolve s with
      | `Resolved s -> s
      | `Self -> Self
      | `Unresolved s -> Resolved (resolve s ~names))

and resolve_type t ~names : Resolved.typ =
  match t with
  | Literal l -> Literal l
  | Ident i -> Ident (resolve_ident ~names i)
  | Sum l -> Sum (List.map ~f:(resolve_type ~names) l)
  | Tuple l -> Tuple (List.map ~f:(resolve_type ~names) l)
  | App (f, x) -> App (resolve_type ~names f, resolve_type ~names x)
  | List t -> List (resolve_type t ~names)
  | Record fields -> Record (List.map ~f:(resolve_field ~names) fields)

and resolve_interface i ~names : Resolved.interface =
  let names = names#inside i.name in
  let i = i.data in
  { extends = List.map ~f:(resolve_ident ~names) i.extends
  ; params = i.params
  ; fields =
      (let names =
         List.fold_left ~init:names i.params ~f:(fun acc x ->
             acc#push x Prim.Any)
       in
       List.map ~f:(resolve_field ~names) i.fields)
  }

and resolve_field f ~names : Resolved.field =
  let data : Resolved.field_def =
    match f.data with
    | Single { optional; typ } ->
      let typ = resolve_type ~names typ in
      Single { optional; typ }
    | Pattern { pat; typ } ->
      let typ = resolve_type ~names typ in
      let pat = resolve_type ~names pat in
      Pattern { pat; typ }
  in
  { f with Named.data }
