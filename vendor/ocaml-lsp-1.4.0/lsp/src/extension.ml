open Import
open Json.Conv

module DebugEcho = struct
  module T = struct
    type t = { message : string } [@@deriving_inline yojson]

    let _ = fun (_ : t) -> ()

    let t_of_yojson =
      (let _tp_loc = "lsp/src/extension.ml.DebugEcho.T.t" in
       function
       | `Assoc field_yojsons as yojson -> (
         let message_field = ref None
         and duplicates = ref []
         and extra = ref [] in
         let rec iter = function
           | (field_name, _field_yojson) :: tail ->
             (match field_name with
             | "message" -> (
               match Ppx_yojson_conv_lib.( ! ) message_field with
               | None ->
                 let fvalue = string_of_yojson _field_yojson in
                 message_field := Some fvalue
               | Some _ ->
                 duplicates :=
                   field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
             | _ ->
               if
                 Ppx_yojson_conv_lib.( ! )
                   Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
               then
                 extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
               else
                 ());
             iter tail
           | [] -> ()
         in
         iter field_yojsons;
         match Ppx_yojson_conv_lib.( ! ) duplicates with
         | _ :: _ ->
           Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
             (Ppx_yojson_conv_lib.( ! ) duplicates)
             yojson
         | [] -> (
           match Ppx_yojson_conv_lib.( ! ) extra with
           | _ :: _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
               (Ppx_yojson_conv_lib.( ! ) extra)
               yojson
           | [] -> (
             match Ppx_yojson_conv_lib.( ! ) message_field with
             | Some message_value -> { message = message_value }
             | _ ->
               Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                 _tp_loc yojson
                 [ ( Ppx_yojson_conv_lib.poly_equal
                       (Ppx_yojson_conv_lib.( ! ) message_field)
                       None
                   , "message" )
                 ])))
       | _ as yojson ->
         Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
           yojson
        : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

    let _ = t_of_yojson

    let yojson_of_t =
      (function
       | { message = v_message } ->
         let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
         let bnds =
           let arg = yojson_of_string v_message in
           ("message", arg) :: bnds
         in
         `Assoc bnds
        : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

    let _ = yojson_of_t

    [@@@deriving.end]
  end

  module Params = T
  module Result = T
end

module DebugTextDocumentGet = struct
  module Params = Types.TextDocumentPositionParams

  module Result = struct
    type t = string option

    let yojson_of_t = function
      | None -> `Null
      | Some s -> `String s

    let t_of_yojson = function
      | `Null -> None
      | `String s -> Some s
      | json -> Json.error "DebugTextDocumentGet" json
  end
end
