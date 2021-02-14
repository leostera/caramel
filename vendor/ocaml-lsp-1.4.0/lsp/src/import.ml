open Stdune
module List = Stdune.List
module Hashtbl = Stdune.Hashtbl
module Option = Stdune.Option
module Either = Stdune.Either
module Int = Stdune.Int
module Dyn = Stdune.Dyn
module Ordering = Stdune.Ordering
module Exn = Stdune.Exn
module Code_error = Code_error
module Or_exn = Or_exn
module Table = Table
module Id = Id
module Result = Stdune.Result
module Exn_with_backtrace = Exn_with_backtrace
module Queue = Queue

module String = struct
  include Stdune.String

  let first_double_underscore_end s =
    let len = String.length s in
    let rec aux i =
      if i > len - 2 then
        raise Not_found
      else if s.[i] = '_' && s.[i + 1] = '_' then
        i + 1
      else
        aux (i + 1)
    in
    aux 0

  let no_double_underscore s =
    try
      ignore (first_double_underscore_end s);
      false
    with Not_found -> true

  let trim = function
    | "" -> ""
    | str ->
      let l = String.length str in
      let is_space = function
        | ' '
        | '\n'
        | '\t'
        | '\r' ->
          true
        | _ -> false
      in
      let r0 = ref 0
      and rl = ref l in
      while !r0 < l && is_space str.[!r0] do
        incr r0
      done;
      let r0 = !r0 in
      while !rl > r0 && is_space str.[!rl - 1] do
        decr rl
      done;
      let rl = !rl in
      if r0 = 0 && rl = l then
        str
      else
        sub str ~pos:r0 ~len:(rl - r0)

  let print () s = Printf.sprintf "%S" s

  let next_occurrence ~pattern text from =
    let plen = String.length pattern in
    let last = String.length text - plen in
    let i = ref from
    and j = ref 0 in
    while !i <= last && !j < plen do
      if text.[!i + !j] <> pattern.[!j] then (
        incr i;
        j := 0
      ) else
        incr j
    done;
    if !j < plen then
      raise Not_found
    else
      !i

  let replace_all ~pattern ~with_ text =
    if pattern = "" then
      text
    else
      match next_occurrence ~pattern text 0 with
      | exception Not_found -> text
      | j0 ->
        let buffer = Buffer.create (String.length text) in
        let rec aux i j =
          Buffer.add_substring buffer text i (j - i);
          Buffer.add_string buffer with_;
          let i' = j + String.length pattern in
          match next_occurrence ~pattern text i' with
          | exception Not_found ->
            Buffer.add_substring buffer text i' (String.length text - i')
          | j' -> aux i' j'
        in
        aux 0 j0;
        Buffer.contents buffer
end

let let_ref r v f =
  let v' = !r in
  r := v;
  match f () with
  | result ->
    r := v';
    result
  | exception exn ->
    r := v';
    raise exn

module Json = struct
  type t = Ppx_yojson_conv_lib.Yojson.Safe.t

  let to_pretty_string (t : t) = Yojson.Safe.pretty_to_string ~std:false t

  let to_string t = Yojson.Safe.to_string t

  let of_string s = Yojson.Safe.from_string s

  let yojson_of_t x = x

  let t_of_yojson x = x

  let error = Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error

  let yojson_of_list = Ppx_yojson_conv_lib.Yojson_conv.yojson_of_list

  let pp ppf (t : t) = Yojson.Safe.pretty_print ppf t

  module Jsonable = Ppx_yojson_conv_lib.Yojsonable

  let field fields name conv = List.assoc_opt name fields |> Option.map ~f:conv

  let field_exn fields name conv =
    match field fields name conv with
    | Some f -> f
    | None -> error "Jsonrpc.Result.t: missing field" (`Assoc fields)

  let rec of_dyn (t : Dyn.t) : t =
    match t with
    | Opaque -> `String "<opaque>"
    | Unit -> `String "()"
    | Int i -> `Int i
    | Int64 i -> `Int (Int64.to_int i)
    | Bool b -> `Bool b
    | String s -> `String s
    | Bytes s -> `String (Bytes.to_string s)
    | Char c -> `String (String.of_list [ c ])
    | Float f -> `Float f
    | Option None -> `String "<none>"
    | Option (Some s) -> of_dyn s
    | List xs -> `List (List.map ~f:of_dyn xs)
    | Array xs -> `List (List.map ~f:of_dyn (Array.to_list xs))
    | Tuple xs -> `List (List.map ~f:of_dyn xs)
    | Record r -> `Assoc (List.map r ~f:(fun (k, v) -> (k, of_dyn v)))
    | Variant (name, args) -> `Assoc [ (name, of_dyn (List args)) ]
    | Set xs -> `List (List.map ~f:of_dyn xs)
    | Map map ->
      `List (List.map map ~f:(fun (k, v) -> `List [ of_dyn k; of_dyn v ]))

  module Conv = struct
    include Ppx_yojson_conv_lib.Yojson_conv
  end

  module O = struct
    let ( <|> ) c1 c2 json =
      match c1 json with
      | s -> s
      | exception Conv.Of_yojson_error (_, _) -> c2 json
  end

  module Option = struct
    type 'a t = 'a option

    let yojson_of_t f = function
      | None -> `Null
      | Some x -> f x

    let t_of_yojson f = function
      | `Null -> None
      | json -> Some (f json)
  end

  module Of = struct
    let list = Ppx_yojson_conv_lib.Yojson_conv.list_of_yojson

    let pair f g json =
      match json with
      | `List [ x; y ] -> (f x, g y)
      | json -> error "pair" json

    let int_pair =
      let int = Ppx_yojson_conv_lib.Yojson_conv.int_of_yojson in
      pair int int

    let untagged_union (type a) name (xs : (t -> a) list) (json : t) : a =
      match
        List.find_map xs ~f:(fun conv ->
            try Some (conv json)
            with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (_, _) ->
              None)
      with
      | None -> error name json
      | Some x -> x

    let literal_field (type a) (name : string) (k : string) (v : string)
        (f : t -> a) (json : t) : a =
      match json with
      | `Assoc xs -> (
        let ks, xs =
          List.partition_map xs ~f:(fun (k', v') ->
              if k = k' then
                if `String v = v' then
                  Left k
                else
                  error (sprintf "%s: incorrect key %s" name k) json
              else
                Right (k', v'))
        in
        match ks with
        | [] -> error (sprintf "%s: key %s not found" name k) json
        | [ _ ] -> f (`Assoc xs)
        | _ :: _ -> error (sprintf "%s: multiple keys %s" name k) json)
      | _ -> error (sprintf "%s: not a record (key: %s)" name k) json
  end

  module To = struct
    let list f xs = `List (List.map ~f xs)

    let literal_field (type a) (k : string) (v : string) (f : a -> t) (t : a) :
        t =
      match f t with
      | `Assoc xs -> `Assoc ((k, `String v) :: xs)
      | _ -> Code_error.raise "To.literal_field" []

    let int_pair (x, y) = `List [ `Int x; `Int y ]
  end

  module Nullable_option = struct
    type 'a t = 'a option

    let t_of_yojson f = function
      | `Null -> None
      | json -> Some (f json)

    let yojson_of_t f = function
      | None -> assert false
      | Some s -> f s
  end

  module Assoc = struct
    type ('a, 'b) t = ('a * 'b) list constraint 'a = string

    let yojson_of_t f g xs =
      let f k =
        match f k with
        | `String s -> s
        | json -> error "Json.Assoc.yojson_of_t not a string key" json
      in
      `Assoc (List.map xs ~f:(fun (k, v) -> (f k, g v)))

    let t_of_yojson f g json =
      let f s = f (`String s) in
      match json with
      | `Assoc xs -> List.map xs ~f:(fun (k, v) -> (f k, g v))
      | _ -> error "Json.Assoc.t_of_yojson: not an object" json
  end

  module Void = struct
    type t

    let t_of_yojson = error "Void.t"

    let yojson_of_t (_ : t) = assert false
  end
end

module Log = struct
  let level : (string option -> bool) ref = ref (fun _ -> false)

  let out = ref Format.err_formatter

  type message =
    { message : string
    ; payload : (string * Json.t) list
    }

  let msg message payload = { message; payload }

  let log ?section k =
    if !level section then (
      let message = k () in
      (match section with
      | None -> Format.fprintf !out "%s@." message.message
      | Some section -> Format.fprintf !out "[%s] %s@." section message.message);
      (match message.payload with
      | [] -> ()
      | fields -> Format.fprintf !out "%a@." Json.pp (`Assoc fields));
      Format.pp_print_flush !out ()
    )
end

let sprintf = Stdune.sprintf
