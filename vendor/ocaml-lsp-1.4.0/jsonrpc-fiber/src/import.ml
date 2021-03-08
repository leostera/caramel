open Stdune
module List = Stdune.List
module Hashtbl = Stdune.Hashtbl
module Option = Stdune.Option
module Either = Stdune.Either
module Int = Stdune.Int
module Dyn = Stdune.Dyn
module Ordering = Stdune.Ordering
module Exn = Stdune.Exn
module Result = Stdune.Result
module Code_error = Code_error
module Or_exn = Or_exn
module Table = Table
module Id = Id
module Exn_with_backtrace = Exn_with_backtrace
module Queue = Queue
include Fiber_unix

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
            try Some (conv json) with
            | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (_, _) -> None)
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

module Fiber = struct
  include Fiber

  module Result = struct
    type nonrec ('a, 'e) t = ('a, 'e) result Fiber.t

    let lift x = Fiber.map x ~f:(fun x -> Ok x)

    let return x = Fiber.return (Ok x)

    let ( >>= ) x f =
      Fiber.bind
        ~f:(function
          | Error _ as e -> Fiber.return e
          | Ok x -> f x)
        x

    module O = struct
      let ( let+ ) x f = Fiber.map ~f:(Result.map ~f) x

      let ( let* ) x f = x >>= f
    end
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
