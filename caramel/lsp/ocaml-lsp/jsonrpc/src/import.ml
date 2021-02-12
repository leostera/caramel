module Option = struct
  let map t ~f =
    match t with
    | None -> None
    | Some x -> Some (f x)
end

module Json = struct
  type t = Yojson.Safe.t

  let error = Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error

  module Jsonable = Ppx_yojson_conv_lib.Yojsonable

  let field fields name conv = List.assoc_opt name fields |> Option.map ~f:conv

  let field_exn fields name conv =
    match field fields name conv with
    | Some f -> f
    | None -> error "Jsonrpc.Result.t: missing field" (`Assoc fields)

  module Conv = Ppx_yojson_conv_lib.Yojson_conv
end
