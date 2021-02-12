open! Import
open Json.Conv

module MarkedString = struct
  type t =
    { value : string
    ; language : string option
    }

  let yojson_of_t { value; language } =
    match language with
    | None -> `String value
    | Some language ->
      `Assoc [ ("value", `String value); ("language", `String language) ]

  let t_of_yojson json =
    match json with
    | `String value -> { value; language = None }
    | `Assoc fields ->
      let value = Json.field_exn fields "value" Json.Conv.string_of_yojson in
      let language =
        Json.field_exn fields "language" Json.Conv.string_of_yojson
      in
      { value; language = Some language }
    | _ -> Json.error "invalid MarkedString" json
end

(*$ Lsp_gen.print_ml () *)

module DeleteFileOptions = struct
  type t =
    { recursive : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; ignoreIfNotExists : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DeleteFileOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let recursive_field = ref None
       and ignoreIfNotExists_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "recursive" -> (
             match Ppx_yojson_conv_lib.( ! ) recursive_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               recursive_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "ignoreIfNotExists" -> (
             match Ppx_yojson_conv_lib.( ! ) ignoreIfNotExists_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               ignoreIfNotExists_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let recursive_value, ignoreIfNotExists_value =
             ( Ppx_yojson_conv_lib.( ! ) recursive_field
             , Ppx_yojson_conv_lib.( ! ) ignoreIfNotExists_field )
           in
           { recursive =
               (match recursive_value with
               | None -> None
               | Some v -> v)
           ; ignoreIfNotExists =
               (match ignoreIfNotExists_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { recursive = v_recursive; ignoreIfNotExists = v_ignoreIfNotExists } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_ignoreIfNotExists then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_ignoreIfNotExists
           in
           let bnd = ("ignoreIfNotExists", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_recursive then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_recursive
           in
           let bnd = ("recursive", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(recursive : bool option) ?(ignoreIfNotExists : bool option)
      (() : unit) : t =
    { recursive; ignoreIfNotExists }
end

module DocumentUri = struct
  type t = string [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson = (string_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t = (yojson_of_string : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]
end

module DeleteFile = struct
  type t =
    { uri : DocumentUri.t
    ; options : DeleteFileOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DeleteFile.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let uri_field = ref None
       and options_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "uri" -> (
             match Ppx_yojson_conv_lib.( ! ) uri_field with
             | None ->
               let fvalue = DocumentUri.t_of_yojson _field_yojson in
               uri_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "options" -> (
             match Ppx_yojson_conv_lib.( ! ) options_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DeleteFileOptions.t_of_yojson
                   _field_yojson
               in
               options_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) uri_field
             , Ppx_yojson_conv_lib.( ! ) options_field )
           with
           | Some uri_value, options_value ->
             { uri = uri_value
             ; options =
                 (match options_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) uri_field)
                     None
                 , "uri" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { uri = v_uri; options = v_options } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_options then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DeleteFileOptions.yojson_of_t)
               v_options
           in
           let bnd = ("options", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = DocumentUri.yojson_of_t v_uri in
         ("uri", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(uri : DocumentUri.t) ?(options : DeleteFileOptions.t option)
      (() : unit) : t =
    { uri; options }

  let yojson_of_t (t : t) : Json.t =
    Json.To.literal_field "kind" "delete" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "delete" t_of_yojson json
end

module RenameFileOptions = struct
  type t =
    { overwrite : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; ignoreIfExists : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.RenameFileOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let overwrite_field = ref None
       and ignoreIfExists_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "overwrite" -> (
             match Ppx_yojson_conv_lib.( ! ) overwrite_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               overwrite_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "ignoreIfExists" -> (
             match Ppx_yojson_conv_lib.( ! ) ignoreIfExists_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               ignoreIfExists_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let overwrite_value, ignoreIfExists_value =
             ( Ppx_yojson_conv_lib.( ! ) overwrite_field
             , Ppx_yojson_conv_lib.( ! ) ignoreIfExists_field )
           in
           { overwrite =
               (match overwrite_value with
               | None -> None
               | Some v -> v)
           ; ignoreIfExists =
               (match ignoreIfExists_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { overwrite = v_overwrite; ignoreIfExists = v_ignoreIfExists } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_ignoreIfExists then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_ignoreIfExists
           in
           let bnd = ("ignoreIfExists", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_overwrite then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_overwrite
           in
           let bnd = ("overwrite", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(overwrite : bool option) ?(ignoreIfExists : bool option)
      (() : unit) : t =
    { overwrite; ignoreIfExists }
end

module RenameFile = struct
  type t =
    { oldUri : DocumentUri.t
    ; newUri : DocumentUri.t
    ; options : RenameFileOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.RenameFile.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let oldUri_field = ref None
       and newUri_field = ref None
       and options_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "oldUri" -> (
             match Ppx_yojson_conv_lib.( ! ) oldUri_field with
             | None ->
               let fvalue = DocumentUri.t_of_yojson _field_yojson in
               oldUri_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "newUri" -> (
             match Ppx_yojson_conv_lib.( ! ) newUri_field with
             | None ->
               let fvalue = DocumentUri.t_of_yojson _field_yojson in
               newUri_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "options" -> (
             match Ppx_yojson_conv_lib.( ! ) options_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson RenameFileOptions.t_of_yojson
                   _field_yojson
               in
               options_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) oldUri_field
             , Ppx_yojson_conv_lib.( ! ) newUri_field
             , Ppx_yojson_conv_lib.( ! ) options_field )
           with
           | Some oldUri_value, Some newUri_value, options_value ->
             { oldUri = oldUri_value
             ; newUri = newUri_value
             ; options =
                 (match options_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) oldUri_field)
                     None
                 , "oldUri" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) newUri_field)
                     None
                 , "newUri" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { oldUri = v_oldUri; newUri = v_newUri; options = v_options } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_options then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t RenameFileOptions.yojson_of_t)
               v_options
           in
           let bnd = ("options", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = DocumentUri.yojson_of_t v_newUri in
         ("newUri", arg) :: bnds
       in
       let bnds =
         let arg = DocumentUri.yojson_of_t v_oldUri in
         ("oldUri", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(oldUri : DocumentUri.t) ~(newUri : DocumentUri.t)
      ?(options : RenameFileOptions.t option) (() : unit) : t =
    { oldUri; newUri; options }

  let yojson_of_t (t : t) : Json.t =
    Json.To.literal_field "kind" "rename" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "rename" t_of_yojson json
end

module CreateFileOptions = struct
  type t =
    { overwrite : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; ignoreIfExists : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CreateFileOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let overwrite_field = ref None
       and ignoreIfExists_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "overwrite" -> (
             match Ppx_yojson_conv_lib.( ! ) overwrite_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               overwrite_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "ignoreIfExists" -> (
             match Ppx_yojson_conv_lib.( ! ) ignoreIfExists_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               ignoreIfExists_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let overwrite_value, ignoreIfExists_value =
             ( Ppx_yojson_conv_lib.( ! ) overwrite_field
             , Ppx_yojson_conv_lib.( ! ) ignoreIfExists_field )
           in
           { overwrite =
               (match overwrite_value with
               | None -> None
               | Some v -> v)
           ; ignoreIfExists =
               (match ignoreIfExists_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { overwrite = v_overwrite; ignoreIfExists = v_ignoreIfExists } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_ignoreIfExists then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_ignoreIfExists
           in
           let bnd = ("ignoreIfExists", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_overwrite then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_overwrite
           in
           let bnd = ("overwrite", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(overwrite : bool option) ?(ignoreIfExists : bool option)
      (() : unit) : t =
    { overwrite; ignoreIfExists }
end

module CreateFile = struct
  type t =
    { uri : DocumentUri.t
    ; options : CreateFileOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CreateFile.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let uri_field = ref None
       and options_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "uri" -> (
             match Ppx_yojson_conv_lib.( ! ) uri_field with
             | None ->
               let fvalue = DocumentUri.t_of_yojson _field_yojson in
               uri_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "options" -> (
             match Ppx_yojson_conv_lib.( ! ) options_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson CreateFileOptions.t_of_yojson
                   _field_yojson
               in
               options_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) uri_field
             , Ppx_yojson_conv_lib.( ! ) options_field )
           with
           | Some uri_value, options_value ->
             { uri = uri_value
             ; options =
                 (match options_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) uri_field)
                     None
                 , "uri" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { uri = v_uri; options = v_options } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_options then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t CreateFileOptions.yojson_of_t)
               v_options
           in
           let bnd = ("options", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = DocumentUri.yojson_of_t v_uri in
         ("uri", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(uri : DocumentUri.t) ?(options : CreateFileOptions.t option)
      (() : unit) : t =
    { uri; options }

  let yojson_of_t (t : t) : Json.t =
    Json.To.literal_field "kind" "create" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "create" t_of_yojson json
end

module Position = struct
  type t =
    { line : int
    ; character : int
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.Position.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let line_field = ref None
       and character_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "line" -> (
             match Ppx_yojson_conv_lib.( ! ) line_field with
             | None ->
               let fvalue = int_of_yojson _field_yojson in
               line_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "character" -> (
             match Ppx_yojson_conv_lib.( ! ) character_field with
             | None ->
               let fvalue = int_of_yojson _field_yojson in
               character_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) line_field
             , Ppx_yojson_conv_lib.( ! ) character_field )
           with
           | Some line_value, Some character_value ->
             { line = line_value; character = character_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) line_field)
                     None
                 , "line" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) character_field)
                     None
                 , "character" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { line = v_line; character = v_character } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_int v_character in
         ("character", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_int v_line in
         ("line", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(line : int) ~(character : int) : t = { line; character }
end

module Range = struct
  type t =
    { start : Position.t
    ; end_ : Position.t [@key "end"]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.Range.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let start_field = ref None
       and end__field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "start" -> (
             match Ppx_yojson_conv_lib.( ! ) start_field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               start_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "end" -> (
             match Ppx_yojson_conv_lib.( ! ) end__field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               end__field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) start_field
             , Ppx_yojson_conv_lib.( ! ) end__field )
           with
           | Some start_value, Some end__value ->
             { start = start_value; end_ = end__value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) start_field)
                     None
                 , "start" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) end__field)
                     None
                 , "end_" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { start = v_start; end_ = v_end_ } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Position.yojson_of_t v_end_ in
         ("end", arg) :: bnds
       in
       let bnds =
         let arg = Position.yojson_of_t v_start in
         ("start", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(start : Position.t) ~(end_ : Position.t) : t = { start; end_ }
end

module TextEdit = struct
  type t =
    { range : Range.t
    ; newText : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TextEdit.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let range_field = ref None
       and newText_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "range" -> (
             match Ppx_yojson_conv_lib.( ! ) range_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               range_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "newText" -> (
             match Ppx_yojson_conv_lib.( ! ) newText_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               newText_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) range_field
             , Ppx_yojson_conv_lib.( ! ) newText_field )
           with
           | Some range_value, Some newText_value ->
             { range = range_value; newText = newText_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) range_field)
                     None
                 , "range" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) newText_field)
                     None
                 , "newText" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { range = v_range; newText = v_newText } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_newText in
         ("newText", arg) :: bnds
       in
       let bnds =
         let arg = Range.yojson_of_t v_range in
         ("range", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(range : Range.t) ~(newText : string) : t = { range; newText }
end

module TextDocumentIdentifier = struct
  type t = { uri : DocumentUri.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TextDocumentIdentifier.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let uri_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "uri" -> (
             match Ppx_yojson_conv_lib.( ! ) uri_field with
             | None ->
               let fvalue = DocumentUri.t_of_yojson _field_yojson in
               uri_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) uri_field with
           | Some uri_value -> { uri = uri_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) uri_field)
                     None
                 , "uri" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { uri = v_uri } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = DocumentUri.yojson_of_t v_uri in
         ("uri", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(uri : DocumentUri.t) : t = { uri }
end

module VersionedTextDocumentIdentifier = struct
  type t =
    { uri : DocumentUri.t
    ; version : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.VersionedTextDocumentIdentifier.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let uri_field = ref None
       and version_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "uri" -> (
             match Ppx_yojson_conv_lib.( ! ) uri_field with
             | None ->
               let fvalue = DocumentUri.t_of_yojson _field_yojson in
               uri_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "version" -> (
             match Ppx_yojson_conv_lib.( ! ) version_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson int_of_yojson _field_yojson
               in
               version_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) uri_field
             , Ppx_yojson_conv_lib.( ! ) version_field )
           with
           | Some uri_value, version_value ->
             { uri = uri_value
             ; version =
                 (match version_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) uri_field)
                     None
                 , "uri" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { uri = v_uri; version = v_version } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_version then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_int) v_version
           in
           let bnd = ("version", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = DocumentUri.yojson_of_t v_uri in
         ("uri", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(uri : DocumentUri.t) ?(version : int option) (() : unit) : t =
    { uri; version }
end

module TextDocumentEdit = struct
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; edits : TextEdit.t list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TextDocumentEdit.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and edits_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue =
                 VersionedTextDocumentIdentifier.t_of_yojson _field_yojson
               in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "edits" -> (
             match Ppx_yojson_conv_lib.( ! ) edits_field with
             | None ->
               let fvalue = list_of_yojson TextEdit.t_of_yojson _field_yojson in
               edits_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) edits_field )
           with
           | Some textDocument_value, Some edits_value ->
             { textDocument = textDocument_value; edits = edits_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) edits_field)
                     None
                 , "edits" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; edits = v_edits } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list TextEdit.yojson_of_t v_edits in
         ("edits", arg) :: bnds
       in
       let bnds =
         let arg = VersionedTextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : VersionedTextDocumentIdentifier.t)
      ~(edits : TextEdit.t list) : t =
    { textDocument; edits }
end

module WorkspaceEdit = struct
  type documentChanges =
    [ `TextDocumentEdit of TextDocumentEdit.t
    | `CreateFile of CreateFile.t
    | `RenameFile of RenameFile.t
    | `DeleteFile of DeleteFile.t
    ]

  let documentChanges_of_yojson (json : Json.t) : documentChanges =
    Json.Of.untagged_union "documentChanges"
      [ (fun json -> `TextDocumentEdit (TextDocumentEdit.t_of_yojson json))
      ; (fun json -> `CreateFile (CreateFile.t_of_yojson json))
      ; (fun json -> `RenameFile (RenameFile.t_of_yojson json))
      ; (fun json -> `DeleteFile (DeleteFile.t_of_yojson json))
      ]
      json

  let yojson_of_documentChanges (documentChanges : documentChanges) : Json.t =
    match documentChanges with
    | `TextDocumentEdit s -> TextDocumentEdit.yojson_of_t s
    | `CreateFile s -> CreateFile.yojson_of_t s
    | `RenameFile s -> RenameFile.yojson_of_t s
    | `DeleteFile s -> DeleteFile.yojson_of_t s

  type t =
    { changes :
        (DocumentUri.t, TextEdit.t list) Json.Assoc.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentChanges : documentChanges list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkspaceEdit.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let changes_field = ref None
       and documentChanges_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "changes" -> (
             match Ppx_yojson_conv_lib.( ! ) changes_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (Json.Assoc.t_of_yojson DocumentUri.t_of_yojson
                      (list_of_yojson TextEdit.t_of_yojson))
                   _field_yojson
               in
               changes_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentChanges" -> (
             match Ppx_yojson_conv_lib.( ! ) documentChanges_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson documentChanges_of_yojson)
                   _field_yojson
               in
               documentChanges_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let changes_value, documentChanges_value =
             ( Ppx_yojson_conv_lib.( ! ) changes_field
             , Ppx_yojson_conv_lib.( ! ) documentChanges_field )
           in
           { changes =
               (match changes_value with
               | None -> None
               | Some v -> v)
           ; documentChanges =
               (match documentChanges_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { changes = v_changes; documentChanges = v_documentChanges } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_documentChanges then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list yojson_of_documentChanges))
               v_documentChanges
           in
           let bnd = ("documentChanges", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_changes then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (Json.Assoc.yojson_of_t DocumentUri.yojson_of_t
                   (yojson_of_list TextEdit.yojson_of_t)))
               v_changes
           in
           let bnd = ("changes", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(changes : (DocumentUri.t, TextEdit.t list) Json.Assoc.t option)
      ?(documentChanges : documentChanges list option) (() : unit) : t =
    { changes; documentChanges }
end

module ApplyWorkspaceEditParams = struct
  type t =
    { label : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; edit : WorkspaceEdit.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ApplyWorkspaceEditParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let label_field = ref None
       and edit_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "label" -> (
             match Ppx_yojson_conv_lib.( ! ) label_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               label_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "edit" -> (
             match Ppx_yojson_conv_lib.( ! ) edit_field with
             | None ->
               let fvalue = WorkspaceEdit.t_of_yojson _field_yojson in
               edit_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) label_field
             , Ppx_yojson_conv_lib.( ! ) edit_field )
           with
           | label_value, Some edit_value ->
             { label =
                 (match label_value with
                 | None -> None
                 | Some v -> v)
             ; edit = edit_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) edit_field)
                     None
                 , "edit" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { label = v_label; edit = v_edit } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = WorkspaceEdit.yojson_of_t v_edit in
         ("edit", arg) :: bnds
       in
       let bnds =
         if None = v_label then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_label
           in
           let bnd = ("label", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(label : string option) ~(edit : WorkspaceEdit.t) (() : unit) : t
      =
    { label; edit }
end

module ApplyWorkspaceEditResponse = struct
  type t =
    { applied : bool
    ; failureReason : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ApplyWorkspaceEditResponse.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let applied_field = ref None
       and failureReason_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "applied" -> (
             match Ppx_yojson_conv_lib.( ! ) applied_field with
             | None ->
               let fvalue = bool_of_yojson _field_yojson in
               applied_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "failureReason" -> (
             match Ppx_yojson_conv_lib.( ! ) failureReason_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               failureReason_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) applied_field
             , Ppx_yojson_conv_lib.( ! ) failureReason_field )
           with
           | Some applied_value, failureReason_value ->
             { applied = applied_value
             ; failureReason =
                 (match failureReason_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) applied_field)
                     None
                 , "applied" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { applied = v_applied; failureReason = v_failureReason } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_failureReason then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_failureReason
           in
           let bnd = ("failureReason", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_bool v_applied in
         ("applied", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(applied : bool) ?(failureReason : string option) (() : unit) : t
      =
    { applied; failureReason }
end

module CancelParams = struct
  type t = { id : Jsonrpc.Id.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CancelParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let id_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "id" -> (
             match Ppx_yojson_conv_lib.( ! ) id_field with
             | None ->
               let fvalue = Jsonrpc.Id.t_of_yojson _field_yojson in
               id_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) id_field with
           | Some id_value -> { id = id_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) id_field)
                     None
                 , "id" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { id = v_id } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Jsonrpc.Id.yojson_of_t v_id in
         ("id", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(id : Jsonrpc.Id.t) : t = { id }
end

module SelectionRangeClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.SelectionRangeClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value =
             Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
end

module FoldingRangeClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; rangeLimit : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; lineFoldingOnly : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.FoldingRangeClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and rangeLimit_field = ref None
       and lineFoldingOnly_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "rangeLimit" -> (
             match Ppx_yojson_conv_lib.( ! ) rangeLimit_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson int_of_yojson _field_yojson
               in
               rangeLimit_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "lineFoldingOnly" -> (
             match Ppx_yojson_conv_lib.( ! ) lineFoldingOnly_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               lineFoldingOnly_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( dynamicRegistration_value
               , rangeLimit_value
               , lineFoldingOnly_value ) =
             ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
             , Ppx_yojson_conv_lib.( ! ) rangeLimit_field
             , Ppx_yojson_conv_lib.( ! ) lineFoldingOnly_field )
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           ; rangeLimit =
               (match rangeLimit_value with
               | None -> None
               | Some v -> v)
           ; lineFoldingOnly =
               (match lineFoldingOnly_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration
       ; rangeLimit = v_rangeLimit
       ; lineFoldingOnly = v_lineFoldingOnly
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_lineFoldingOnly then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_lineFoldingOnly
           in
           let bnd = ("lineFoldingOnly", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_rangeLimit then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_int) v_rangeLimit
           in
           let bnd = ("rangeLimit", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) ?(rangeLimit : int option)
      ?(lineFoldingOnly : bool option) (() : unit) : t =
    { dynamicRegistration; rangeLimit; lineFoldingOnly }
end

module DiagnosticTag = struct
  type t =
    | Unnecessary
    | Deprecated

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Unnecessary -> `Int 1
    | Deprecated -> `Int 2

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Unnecessary
    | `Int 2 -> Deprecated
    | _ -> Json.error "t" json
end

module PublishDiagnosticsClientCapabilities = struct
  type tagSupport = { valueSet : DiagnosticTag.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : tagSupport) -> ()

  let tagSupport_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.PublishDiagnosticsClientCapabilities.tagSupport"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let valueSet_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "valueSet" -> (
             match Ppx_yojson_conv_lib.( ! ) valueSet_field with
             | None ->
               let fvalue =
                 list_of_yojson DiagnosticTag.t_of_yojson _field_yojson
               in
               valueSet_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) valueSet_field with
           | Some valueSet_value -> { valueSet = valueSet_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) valueSet_field)
                     None
                 , "valueSet" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> tagSupport)

  let _ = tagSupport_of_yojson

  let yojson_of_tagSupport =
    (function
     | { valueSet = v_valueSet } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list DiagnosticTag.yojson_of_t v_valueSet in
         ("valueSet", arg) :: bnds
       in
       `Assoc bnds
      : tagSupport -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_tagSupport

  [@@@end]

  let create_tagSupport ~(valueSet : DiagnosticTag.t list) : tagSupport =
    { valueSet }

  type t =
    { relatedInformation : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; tagSupport : tagSupport Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; versionSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.PublishDiagnosticsClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let relatedInformation_field = ref None
       and tagSupport_field = ref None
       and versionSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "relatedInformation" -> (
             match Ppx_yojson_conv_lib.( ! ) relatedInformation_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               relatedInformation_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "tagSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) tagSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson tagSupport_of_yojson
                   _field_yojson
               in
               tagSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "versionSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) versionSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               versionSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let relatedInformation_value, tagSupport_value, versionSupport_value
               =
             ( Ppx_yojson_conv_lib.( ! ) relatedInformation_field
             , Ppx_yojson_conv_lib.( ! ) tagSupport_field
             , Ppx_yojson_conv_lib.( ! ) versionSupport_field )
           in
           { relatedInformation =
               (match relatedInformation_value with
               | None -> None
               | Some v -> v)
           ; tagSupport =
               (match tagSupport_value with
               | None -> None
               | Some v -> v)
           ; versionSupport =
               (match versionSupport_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { relatedInformation = v_relatedInformation
       ; tagSupport = v_tagSupport
       ; versionSupport = v_versionSupport
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_versionSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_versionSupport
           in
           let bnd = ("versionSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_tagSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_tagSupport)
               v_tagSupport
           in
           let bnd = ("tagSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_relatedInformation then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_relatedInformation
           in
           let bnd = ("relatedInformation", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(relatedInformation : bool option)
      ?(tagSupport : tagSupport option) ?(versionSupport : bool option)
      (() : unit) : t =
    { relatedInformation; tagSupport; versionSupport }
end

module RenameClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; prepareSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.RenameClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and prepareSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "prepareSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) prepareSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               prepareSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value, prepareSupport_value =
             ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
             , Ppx_yojson_conv_lib.( ! ) prepareSupport_field )
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           ; prepareSupport =
               (match prepareSupport_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration
       ; prepareSupport = v_prepareSupport
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_prepareSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_prepareSupport
           in
           let bnd = ("prepareSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option)
      ?(prepareSupport : bool option) (() : unit) : t =
    { dynamicRegistration; prepareSupport }
end

module DocumentOnTypeFormattingClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.DocumentOnTypeFormattingClientCapabilities.t"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value =
             Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
end

module DocumentRangeFormattingClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.DocumentRangeFormattingClientCapabilities.t"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value =
             Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
end

module DocumentFormattingClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentFormattingClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value =
             Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
end

module DocumentColorClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentColorClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value =
             Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
end

module DocumentLinkClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; tooltipSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentLinkClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and tooltipSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "tooltipSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) tooltipSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               tooltipSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value, tooltipSupport_value =
             ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
             , Ppx_yojson_conv_lib.( ! ) tooltipSupport_field )
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           ; tooltipSupport =
               (match tooltipSupport_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration
       ; tooltipSupport = v_tooltipSupport
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_tooltipSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_tooltipSupport
           in
           let bnd = ("tooltipSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option)
      ?(tooltipSupport : bool option) (() : unit) : t =
    { dynamicRegistration; tooltipSupport }
end

module CodeLensClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CodeLensClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value =
             Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
end

module CodeActionKind = struct
  type t =
    | Empty
    | QuickFix
    | Refactor
    | RefactorExtract
    | RefactorInline
    | RefactorRewrite
    | Source
    | SourceOrganizeImports
    | Other of string

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Empty -> `String ""
    | QuickFix -> `String "quickfix"
    | Refactor -> `String "refactor"
    | RefactorExtract -> `String "refactor.extract"
    | RefactorInline -> `String "refactor.inline"
    | RefactorRewrite -> `String "refactor.rewrite"
    | Source -> `String "source"
    | SourceOrganizeImports -> `String "source.organizeImports"
    | Other s -> `String s

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "" -> Empty
    | `String "quickfix" -> QuickFix
    | `String "refactor" -> Refactor
    | `String "refactor.extract" -> RefactorExtract
    | `String "refactor.inline" -> RefactorInline
    | `String "refactor.rewrite" -> RefactorRewrite
    | `String "source" -> Source
    | `String "source.organizeImports" -> SourceOrganizeImports
    | `String s -> Other s
    | _ -> Json.error "t" json
end

module CodeActionClientCapabilities = struct
  type codeActionKind = { valueSet : CodeActionKind.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : codeActionKind) -> ()

  let codeActionKind_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.CodeActionClientCapabilities.codeActionKind"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let valueSet_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "valueSet" -> (
             match Ppx_yojson_conv_lib.( ! ) valueSet_field with
             | None ->
               let fvalue =
                 list_of_yojson CodeActionKind.t_of_yojson _field_yojson
               in
               valueSet_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) valueSet_field with
           | Some valueSet_value -> { valueSet = valueSet_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) valueSet_field)
                     None
                 , "valueSet" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> codeActionKind)

  let _ = codeActionKind_of_yojson

  let yojson_of_codeActionKind =
    (function
     | { valueSet = v_valueSet } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list CodeActionKind.yojson_of_t v_valueSet in
         ("valueSet", arg) :: bnds
       in
       `Assoc bnds
      : codeActionKind -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_codeActionKind

  [@@@end]

  let create_codeActionKind ~(valueSet : CodeActionKind.t list) : codeActionKind
      =
    { valueSet }

  type codeActionLiteralSupport = { codeActionKind : codeActionKind }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : codeActionLiteralSupport) -> ()

  let codeActionLiteralSupport_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.CodeActionClientCapabilities.codeActionLiteralSupport"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let codeActionKind_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "codeActionKind" -> (
             match Ppx_yojson_conv_lib.( ! ) codeActionKind_field with
             | None ->
               let fvalue = codeActionKind_of_yojson _field_yojson in
               codeActionKind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) codeActionKind_field with
           | Some codeActionKind_value ->
             { codeActionKind = codeActionKind_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) codeActionKind_field)
                     None
                 , "codeActionKind" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> codeActionLiteralSupport)

  let _ = codeActionLiteralSupport_of_yojson

  let yojson_of_codeActionLiteralSupport =
    (function
     | { codeActionKind = v_codeActionKind } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_codeActionKind v_codeActionKind in
         ("codeActionKind", arg) :: bnds
       in
       `Assoc bnds
      : codeActionLiteralSupport -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_codeActionLiteralSupport

  [@@@end]

  let create_codeActionLiteralSupport ~(codeActionKind : codeActionKind) :
      codeActionLiteralSupport =
    { codeActionKind }

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeActionLiteralSupport : codeActionLiteralSupport Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; isPreferredSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CodeActionClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and codeActionLiteralSupport_field = ref None
       and isPreferredSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "codeActionLiteralSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) codeActionLiteralSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   codeActionLiteralSupport_of_yojson _field_yojson
               in
               codeActionLiteralSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "isPreferredSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) isPreferredSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               isPreferredSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( dynamicRegistration_value
               , codeActionLiteralSupport_value
               , isPreferredSupport_value ) =
             ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
             , Ppx_yojson_conv_lib.( ! ) codeActionLiteralSupport_field
             , Ppx_yojson_conv_lib.( ! ) isPreferredSupport_field )
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           ; codeActionLiteralSupport =
               (match codeActionLiteralSupport_value with
               | None -> None
               | Some v -> v)
           ; isPreferredSupport =
               (match isPreferredSupport_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration
       ; codeActionLiteralSupport = v_codeActionLiteralSupport
       ; isPreferredSupport = v_isPreferredSupport
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_isPreferredSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_isPreferredSupport
           in
           let bnd = ("isPreferredSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_codeActionLiteralSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                yojson_of_codeActionLiteralSupport)
               v_codeActionLiteralSupport
           in
           let bnd = ("codeActionLiteralSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option)
      ?(codeActionLiteralSupport : codeActionLiteralSupport option)
      ?(isPreferredSupport : bool option) (() : unit) : t =
    { dynamicRegistration; codeActionLiteralSupport; isPreferredSupport }
end

module SymbolKind = struct
  type t =
    | File
    | Module
    | Namespace
    | Package
    | Class
    | Method
    | Property
    | Field
    | Constructor
    | Enum
    | Interface
    | Function
    | Variable
    | Constant
    | String
    | Number
    | Boolean
    | Array
    | Object
    | Key
    | Null
    | EnumMember
    | Struct
    | Event
    | Operator
    | TypeParameter

  let yojson_of_t (t : t) : Json.t =
    match t with
    | File -> `Int 1
    | Module -> `Int 2
    | Namespace -> `Int 3
    | Package -> `Int 4
    | Class -> `Int 5
    | Method -> `Int 6
    | Property -> `Int 7
    | Field -> `Int 8
    | Constructor -> `Int 9
    | Enum -> `Int 10
    | Interface -> `Int 11
    | Function -> `Int 12
    | Variable -> `Int 13
    | Constant -> `Int 14
    | String -> `Int 15
    | Number -> `Int 16
    | Boolean -> `Int 17
    | Array -> `Int 18
    | Object -> `Int 19
    | Key -> `Int 20
    | Null -> `Int 21
    | EnumMember -> `Int 22
    | Struct -> `Int 23
    | Event -> `Int 24
    | Operator -> `Int 25
    | TypeParameter -> `Int 26

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> File
    | `Int 2 -> Module
    | `Int 3 -> Namespace
    | `Int 4 -> Package
    | `Int 5 -> Class
    | `Int 6 -> Method
    | `Int 7 -> Property
    | `Int 8 -> Field
    | `Int 9 -> Constructor
    | `Int 10 -> Enum
    | `Int 11 -> Interface
    | `Int 12 -> Function
    | `Int 13 -> Variable
    | `Int 14 -> Constant
    | `Int 15 -> String
    | `Int 16 -> Number
    | `Int 17 -> Boolean
    | `Int 18 -> Array
    | `Int 19 -> Object
    | `Int 20 -> Key
    | `Int 21 -> Null
    | `Int 22 -> EnumMember
    | `Int 23 -> Struct
    | `Int 24 -> Event
    | `Int 25 -> Operator
    | `Int 26 -> TypeParameter
    | _ -> Json.error "t" json
end

module DocumentSymbolClientCapabilities = struct
  type symbolKind =
    { valueSet : SymbolKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : symbolKind) -> ()

  let symbolKind_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.DocumentSymbolClientCapabilities.symbolKind"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let valueSet_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "valueSet" -> (
             match Ppx_yojson_conv_lib.( ! ) valueSet_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson SymbolKind.t_of_yojson)
                   _field_yojson
               in
               valueSet_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let valueSet_value = Ppx_yojson_conv_lib.( ! ) valueSet_field in
           { valueSet =
               (match valueSet_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> symbolKind)

  let _ = symbolKind_of_yojson

  let yojson_of_symbolKind =
    (function
     | { valueSet = v_valueSet } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_valueSet then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list SymbolKind.yojson_of_t))
               v_valueSet
           in
           let bnd = ("valueSet", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : symbolKind -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_symbolKind

  [@@@end]

  let create_symbolKind ?(valueSet : SymbolKind.t list option) (() : unit) :
      symbolKind =
    { valueSet }

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; symbolKind : symbolKind Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; hierarchicalDocumentSymbolSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentSymbolClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and symbolKind_field = ref None
       and hierarchicalDocumentSymbolSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "symbolKind" -> (
             match Ppx_yojson_conv_lib.( ! ) symbolKind_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson symbolKind_of_yojson
                   _field_yojson
               in
               symbolKind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "hierarchicalDocumentSymbolSupport" -> (
             match
               Ppx_yojson_conv_lib.( ! ) hierarchicalDocumentSymbolSupport_field
             with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               hierarchicalDocumentSymbolSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( dynamicRegistration_value
               , symbolKind_value
               , hierarchicalDocumentSymbolSupport_value ) =
             ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
             , Ppx_yojson_conv_lib.( ! ) symbolKind_field
             , Ppx_yojson_conv_lib.( ! ) hierarchicalDocumentSymbolSupport_field
             )
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           ; symbolKind =
               (match symbolKind_value with
               | None -> None
               | Some v -> v)
           ; hierarchicalDocumentSymbolSupport =
               (match hierarchicalDocumentSymbolSupport_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration
       ; symbolKind = v_symbolKind
       ; hierarchicalDocumentSymbolSupport = v_hierarchicalDocumentSymbolSupport
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_hierarchicalDocumentSymbolSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_hierarchicalDocumentSymbolSupport
           in
           let bnd = ("hierarchicalDocumentSymbolSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_symbolKind then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_symbolKind)
               v_symbolKind
           in
           let bnd = ("symbolKind", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option)
      ?(symbolKind : symbolKind option)
      ?(hierarchicalDocumentSymbolSupport : bool option) (() : unit) : t =
    { dynamicRegistration; symbolKind; hierarchicalDocumentSymbolSupport }
end

module DocumentHighlightClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentHighlightClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value =
             Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
end

module ReferenceClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ReferenceClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value =
             Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
end

module ImplementationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ImplementationClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and linkSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "linkSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) linkSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               linkSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value, linkSupport_value =
             ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
             , Ppx_yojson_conv_lib.( ! ) linkSupport_field )
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           ; linkSupport =
               (match linkSupport_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration
       ; linkSupport = v_linkSupport
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_linkSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_linkSupport
           in
           let bnd = ("linkSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) ?(linkSupport : bool option)
      (() : unit) : t =
    { dynamicRegistration; linkSupport }
end

module TypeDefinitionClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TypeDefinitionClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and linkSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "linkSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) linkSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               linkSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value, linkSupport_value =
             ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
             , Ppx_yojson_conv_lib.( ! ) linkSupport_field )
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           ; linkSupport =
               (match linkSupport_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration
       ; linkSupport = v_linkSupport
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_linkSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_linkSupport
           in
           let bnd = ("linkSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) ?(linkSupport : bool option)
      (() : unit) : t =
    { dynamicRegistration; linkSupport }
end

module DefinitionClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DefinitionClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and linkSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "linkSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) linkSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               linkSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value, linkSupport_value =
             ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
             , Ppx_yojson_conv_lib.( ! ) linkSupport_field )
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           ; linkSupport =
               (match linkSupport_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration
       ; linkSupport = v_linkSupport
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_linkSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_linkSupport
           in
           let bnd = ("linkSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) ?(linkSupport : bool option)
      (() : unit) : t =
    { dynamicRegistration; linkSupport }
end

module DeclarationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; linkSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DeclarationClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and linkSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "linkSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) linkSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               linkSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value, linkSupport_value =
             ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
             , Ppx_yojson_conv_lib.( ! ) linkSupport_field )
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           ; linkSupport =
               (match linkSupport_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration
       ; linkSupport = v_linkSupport
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_linkSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_linkSupport
           in
           let bnd = ("linkSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) ?(linkSupport : bool option)
      (() : unit) : t =
    { dynamicRegistration; linkSupport }
end

module MarkupKind = struct
  type t =
    | PlainText
    | Markdown

  let yojson_of_t (t : t) : Json.t =
    match t with
    | PlainText -> `String "plaintext"
    | Markdown -> `String "markdown"

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "plaintext" -> PlainText
    | `String "markdown" -> Markdown
    | _ -> Json.error "t" json
end

module SignatureHelpClientCapabilities = struct
  type parameterInformation =
    { labelOffsetSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : parameterInformation) -> ()

  let parameterInformation_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.SignatureHelpClientCapabilities.parameterInformation"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let labelOffsetSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "labelOffsetSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) labelOffsetSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               labelOffsetSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let labelOffsetSupport_value =
             Ppx_yojson_conv_lib.( ! ) labelOffsetSupport_field
           in
           { labelOffsetSupport =
               (match labelOffsetSupport_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> parameterInformation)

  let _ = parameterInformation_of_yojson

  let yojson_of_parameterInformation =
    (function
     | { labelOffsetSupport = v_labelOffsetSupport } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_labelOffsetSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_labelOffsetSupport
           in
           let bnd = ("labelOffsetSupport", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : parameterInformation -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_parameterInformation

  [@@@end]

  let create_parameterInformation ?(labelOffsetSupport : bool option)
      (() : unit) : parameterInformation =
    { labelOffsetSupport }

  type signatureInformation =
    { documentationFormat : MarkupKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; parameterInformation : parameterInformation Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : signatureInformation) -> ()

  let signatureInformation_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.SignatureHelpClientCapabilities.signatureInformation"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentationFormat_field = ref None
       and parameterInformation_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentationFormat" -> (
             match Ppx_yojson_conv_lib.( ! ) documentationFormat_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson MarkupKind.t_of_yojson)
                   _field_yojson
               in
               documentationFormat_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "parameterInformation" -> (
             match Ppx_yojson_conv_lib.( ! ) parameterInformation_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson parameterInformation_of_yojson
                   _field_yojson
               in
               parameterInformation_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let documentationFormat_value, parameterInformation_value =
             ( Ppx_yojson_conv_lib.( ! ) documentationFormat_field
             , Ppx_yojson_conv_lib.( ! ) parameterInformation_field )
           in
           { documentationFormat =
               (match documentationFormat_value with
               | None -> None
               | Some v -> v)
           ; parameterInformation =
               (match parameterInformation_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> signatureInformation)

  let _ = signatureInformation_of_yojson

  let yojson_of_signatureInformation =
    (function
     | { documentationFormat = v_documentationFormat
       ; parameterInformation = v_parameterInformation
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_parameterInformation then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_parameterInformation)
               v_parameterInformation
           in
           let bnd = ("parameterInformation", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentationFormat then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list MarkupKind.yojson_of_t))
               v_documentationFormat
           in
           let bnd = ("documentationFormat", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : signatureInformation -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_signatureInformation

  [@@@end]

  let create_signatureInformation
      ?(documentationFormat : MarkupKind.t list option)
      ?(parameterInformation : parameterInformation option) (() : unit) :
      signatureInformation =
    { documentationFormat; parameterInformation }

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; signatureInformation : signatureInformation Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; contextSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.SignatureHelpClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and signatureInformation_field = ref None
       and contextSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "signatureInformation" -> (
             match Ppx_yojson_conv_lib.( ! ) signatureInformation_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson signatureInformation_of_yojson
                   _field_yojson
               in
               signatureInformation_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "contextSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) contextSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               contextSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( dynamicRegistration_value
               , signatureInformation_value
               , contextSupport_value ) =
             ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
             , Ppx_yojson_conv_lib.( ! ) signatureInformation_field
             , Ppx_yojson_conv_lib.( ! ) contextSupport_field )
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           ; signatureInformation =
               (match signatureInformation_value with
               | None -> None
               | Some v -> v)
           ; contextSupport =
               (match contextSupport_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration
       ; signatureInformation = v_signatureInformation
       ; contextSupport = v_contextSupport
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_contextSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_contextSupport
           in
           let bnd = ("contextSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_signatureInformation then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_signatureInformation)
               v_signatureInformation
           in
           let bnd = ("signatureInformation", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option)
      ?(signatureInformation : signatureInformation option)
      ?(contextSupport : bool option) (() : unit) : t =
    { dynamicRegistration; signatureInformation; contextSupport }
end

module HoverClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; contentFormat : MarkupKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.HoverClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and contentFormat_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "contentFormat" -> (
             match Ppx_yojson_conv_lib.( ! ) contentFormat_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson MarkupKind.t_of_yojson)
                   _field_yojson
               in
               contentFormat_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value, contentFormat_value =
             ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
             , Ppx_yojson_conv_lib.( ! ) contentFormat_field )
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           ; contentFormat =
               (match contentFormat_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration
       ; contentFormat = v_contentFormat
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_contentFormat then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list MarkupKind.yojson_of_t))
               v_contentFormat
           in
           let bnd = ("contentFormat", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option)
      ?(contentFormat : MarkupKind.t list option) (() : unit) : t =
    { dynamicRegistration; contentFormat }
end

module CompletionItemKind = struct
  type t =
    | Text
    | Method
    | Function
    | Constructor
    | Field
    | Variable
    | Class
    | Interface
    | Module
    | Property
    | Unit
    | Value
    | Enum
    | Keyword
    | Snippet
    | Color
    | File
    | Reference
    | Folder
    | EnumMember
    | Constant
    | Struct
    | Event
    | Operator
    | TypeParameter

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Text -> `Int 1
    | Method -> `Int 2
    | Function -> `Int 3
    | Constructor -> `Int 4
    | Field -> `Int 5
    | Variable -> `Int 6
    | Class -> `Int 7
    | Interface -> `Int 8
    | Module -> `Int 9
    | Property -> `Int 10
    | Unit -> `Int 11
    | Value -> `Int 12
    | Enum -> `Int 13
    | Keyword -> `Int 14
    | Snippet -> `Int 15
    | Color -> `Int 16
    | File -> `Int 17
    | Reference -> `Int 18
    | Folder -> `Int 19
    | EnumMember -> `Int 20
    | Constant -> `Int 21
    | Struct -> `Int 22
    | Event -> `Int 23
    | Operator -> `Int 24
    | TypeParameter -> `Int 25

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Text
    | `Int 2 -> Method
    | `Int 3 -> Function
    | `Int 4 -> Constructor
    | `Int 5 -> Field
    | `Int 6 -> Variable
    | `Int 7 -> Class
    | `Int 8 -> Interface
    | `Int 9 -> Module
    | `Int 10 -> Property
    | `Int 11 -> Unit
    | `Int 12 -> Value
    | `Int 13 -> Enum
    | `Int 14 -> Keyword
    | `Int 15 -> Snippet
    | `Int 16 -> Color
    | `Int 17 -> File
    | `Int 18 -> Reference
    | `Int 19 -> Folder
    | `Int 20 -> EnumMember
    | `Int 21 -> Constant
    | `Int 22 -> Struct
    | `Int 23 -> Event
    | `Int 24 -> Operator
    | `Int 25 -> TypeParameter
    | _ -> Json.error "t" json
end

module CompletionItemTag = struct
  type t = Deprecated

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Deprecated -> `Int 1

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Deprecated
    | _ -> Json.error "t" json
end

module CompletionClientCapabilities = struct
  type completionItemKind =
    { valueSet : CompletionItemKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : completionItemKind) -> ()

  let completionItemKind_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.CompletionClientCapabilities.completionItemKind"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let valueSet_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "valueSet" -> (
             match Ppx_yojson_conv_lib.( ! ) valueSet_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson CompletionItemKind.t_of_yojson)
                   _field_yojson
               in
               valueSet_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let valueSet_value = Ppx_yojson_conv_lib.( ! ) valueSet_field in
           { valueSet =
               (match valueSet_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> completionItemKind)

  let _ = completionItemKind_of_yojson

  let yojson_of_completionItemKind =
    (function
     | { valueSet = v_valueSet } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_valueSet then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list CompletionItemKind.yojson_of_t))
               v_valueSet
           in
           let bnd = ("valueSet", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : completionItemKind -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_completionItemKind

  [@@@end]

  let create_completionItemKind ?(valueSet : CompletionItemKind.t list option)
      (() : unit) : completionItemKind =
    { valueSet }

  type tagSupport = { valueSet : CompletionItemTag.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : tagSupport) -> ()

  let tagSupport_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CompletionClientCapabilities.tagSupport" in
     function
     | `Assoc field_yojsons as yojson -> (
       let valueSet_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "valueSet" -> (
             match Ppx_yojson_conv_lib.( ! ) valueSet_field with
             | None ->
               let fvalue =
                 list_of_yojson CompletionItemTag.t_of_yojson _field_yojson
               in
               valueSet_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) valueSet_field with
           | Some valueSet_value -> { valueSet = valueSet_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) valueSet_field)
                     None
                 , "valueSet" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> tagSupport)

  let _ = tagSupport_of_yojson

  let yojson_of_tagSupport =
    (function
     | { valueSet = v_valueSet } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list CompletionItemTag.yojson_of_t v_valueSet in
         ("valueSet", arg) :: bnds
       in
       `Assoc bnds
      : tagSupport -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_tagSupport

  [@@@end]

  let create_tagSupport ~(valueSet : CompletionItemTag.t list) : tagSupport =
    { valueSet }

  type completionItem =
    { snippetSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; commitCharactersSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentationFormat : MarkupKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; deprecatedSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; preselectSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; tagSupport : tagSupport Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : completionItem) -> ()

  let completionItem_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.CompletionClientCapabilities.completionItem"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let snippetSupport_field = ref None
       and commitCharactersSupport_field = ref None
       and documentationFormat_field = ref None
       and deprecatedSupport_field = ref None
       and preselectSupport_field = ref None
       and tagSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "snippetSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) snippetSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               snippetSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "commitCharactersSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) commitCharactersSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               commitCharactersSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentationFormat" -> (
             match Ppx_yojson_conv_lib.( ! ) documentationFormat_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson MarkupKind.t_of_yojson)
                   _field_yojson
               in
               documentationFormat_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "deprecatedSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) deprecatedSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               deprecatedSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "preselectSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) preselectSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               preselectSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "tagSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) tagSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson tagSupport_of_yojson
                   _field_yojson
               in
               tagSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( snippetSupport_value
               , commitCharactersSupport_value
               , documentationFormat_value
               , deprecatedSupport_value
               , preselectSupport_value
               , tagSupport_value ) =
             ( Ppx_yojson_conv_lib.( ! ) snippetSupport_field
             , Ppx_yojson_conv_lib.( ! ) commitCharactersSupport_field
             , Ppx_yojson_conv_lib.( ! ) documentationFormat_field
             , Ppx_yojson_conv_lib.( ! ) deprecatedSupport_field
             , Ppx_yojson_conv_lib.( ! ) preselectSupport_field
             , Ppx_yojson_conv_lib.( ! ) tagSupport_field )
           in
           { snippetSupport =
               (match snippetSupport_value with
               | None -> None
               | Some v -> v)
           ; commitCharactersSupport =
               (match commitCharactersSupport_value with
               | None -> None
               | Some v -> v)
           ; documentationFormat =
               (match documentationFormat_value with
               | None -> None
               | Some v -> v)
           ; deprecatedSupport =
               (match deprecatedSupport_value with
               | None -> None
               | Some v -> v)
           ; preselectSupport =
               (match preselectSupport_value with
               | None -> None
               | Some v -> v)
           ; tagSupport =
               (match tagSupport_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> completionItem)

  let _ = completionItem_of_yojson

  let yojson_of_completionItem =
    (function
     | { snippetSupport = v_snippetSupport
       ; commitCharactersSupport = v_commitCharactersSupport
       ; documentationFormat = v_documentationFormat
       ; deprecatedSupport = v_deprecatedSupport
       ; preselectSupport = v_preselectSupport
       ; tagSupport = v_tagSupport
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_tagSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_tagSupport)
               v_tagSupport
           in
           let bnd = ("tagSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_preselectSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_preselectSupport
           in
           let bnd = ("preselectSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_deprecatedSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_deprecatedSupport
           in
           let bnd = ("deprecatedSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentationFormat then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list MarkupKind.yojson_of_t))
               v_documentationFormat
           in
           let bnd = ("documentationFormat", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_commitCharactersSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_commitCharactersSupport
           in
           let bnd = ("commitCharactersSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_snippetSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_snippetSupport
           in
           let bnd = ("snippetSupport", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : completionItem -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_completionItem

  [@@@end]

  let create_completionItem ?(snippetSupport : bool option)
      ?(commitCharactersSupport : bool option)
      ?(documentationFormat : MarkupKind.t list option)
      ?(deprecatedSupport : bool option) ?(preselectSupport : bool option)
      ?(tagSupport : tagSupport option) (() : unit) : completionItem =
    { snippetSupport
    ; commitCharactersSupport
    ; documentationFormat
    ; deprecatedSupport
    ; preselectSupport
    ; tagSupport
    }

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; completionItem : completionItem Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; completionItemKind : completionItemKind Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; contextSupport : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CompletionClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and completionItem_field = ref None
       and completionItemKind_field = ref None
       and contextSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "completionItem" -> (
             match Ppx_yojson_conv_lib.( ! ) completionItem_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson completionItem_of_yojson
                   _field_yojson
               in
               completionItem_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "completionItemKind" -> (
             match Ppx_yojson_conv_lib.( ! ) completionItemKind_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson completionItemKind_of_yojson
                   _field_yojson
               in
               completionItemKind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "contextSupport" -> (
             match Ppx_yojson_conv_lib.( ! ) contextSupport_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               contextSupport_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( dynamicRegistration_value
               , completionItem_value
               , completionItemKind_value
               , contextSupport_value ) =
             ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
             , Ppx_yojson_conv_lib.( ! ) completionItem_field
             , Ppx_yojson_conv_lib.( ! ) completionItemKind_field
             , Ppx_yojson_conv_lib.( ! ) contextSupport_field )
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           ; completionItem =
               (match completionItem_value with
               | None -> None
               | Some v -> v)
           ; completionItemKind =
               (match completionItemKind_value with
               | None -> None
               | Some v -> v)
           ; contextSupport =
               (match contextSupport_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration
       ; completionItem = v_completionItem
       ; completionItemKind = v_completionItemKind
       ; contextSupport = v_contextSupport
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_contextSupport then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_contextSupport
           in
           let bnd = ("contextSupport", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_completionItemKind then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_completionItemKind)
               v_completionItemKind
           in
           let bnd = ("completionItemKind", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_completionItem then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_completionItem)
               v_completionItem
           in
           let bnd = ("completionItem", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option)
      ?(completionItem : completionItem option)
      ?(completionItemKind : completionItemKind option)
      ?(contextSupport : bool option) (() : unit) : t =
    { dynamicRegistration; completionItem; completionItemKind; contextSupport }
end

module TextDocumentSyncClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; willSave : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; willSaveWaitUntil : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; didSave : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TextDocumentSyncClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and willSave_field = ref None
       and willSaveWaitUntil_field = ref None
       and didSave_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "willSave" -> (
             match Ppx_yojson_conv_lib.( ! ) willSave_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               willSave_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "willSaveWaitUntil" -> (
             match Ppx_yojson_conv_lib.( ! ) willSaveWaitUntil_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               willSaveWaitUntil_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "didSave" -> (
             match Ppx_yojson_conv_lib.( ! ) didSave_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               didSave_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( dynamicRegistration_value
               , willSave_value
               , willSaveWaitUntil_value
               , didSave_value ) =
             ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
             , Ppx_yojson_conv_lib.( ! ) willSave_field
             , Ppx_yojson_conv_lib.( ! ) willSaveWaitUntil_field
             , Ppx_yojson_conv_lib.( ! ) didSave_field )
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           ; willSave =
               (match willSave_value with
               | None -> None
               | Some v -> v)
           ; willSaveWaitUntil =
               (match willSaveWaitUntil_value with
               | None -> None
               | Some v -> v)
           ; didSave =
               (match didSave_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration
       ; willSave = v_willSave
       ; willSaveWaitUntil = v_willSaveWaitUntil
       ; didSave = v_didSave
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_didSave then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_didSave
           in
           let bnd = ("didSave", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_willSaveWaitUntil then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_willSaveWaitUntil
           in
           let bnd = ("willSaveWaitUntil", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_willSave then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_willSave
           in
           let bnd = ("willSave", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) ?(willSave : bool option)
      ?(willSaveWaitUntil : bool option) ?(didSave : bool option) (() : unit) :
      t =
    { dynamicRegistration; willSave; willSaveWaitUntil; didSave }
end

module TextDocumentClientCapabilities = struct
  type t =
    { synchronization :
        TextDocumentSyncClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; completion : CompletionClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; hover : HoverClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; signatureHelp : SignatureHelpClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; declaration : DeclarationClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; definition : DefinitionClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; typeDefinition : TypeDefinitionClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; implementation : ImplementationClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; references : ReferenceClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentHighlight :
        DocumentHighlightClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentSymbol : DocumentSymbolClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeAction : CodeActionClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeLens : CodeLensClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentLink : DocumentLinkClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; colorProvider : DocumentColorClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; formatting : DocumentFormattingClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; rangeFormatting :
        DocumentRangeFormattingClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; onTypeFormatting :
        DocumentOnTypeFormattingClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; rename : RenameClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; publishDiagnostics :
        PublishDiagnosticsClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; foldingRange : FoldingRangeClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; selectionRange : SelectionRangeClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TextDocumentClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let synchronization_field = ref None
       and completion_field = ref None
       and hover_field = ref None
       and signatureHelp_field = ref None
       and declaration_field = ref None
       and definition_field = ref None
       and typeDefinition_field = ref None
       and implementation_field = ref None
       and references_field = ref None
       and documentHighlight_field = ref None
       and documentSymbol_field = ref None
       and codeAction_field = ref None
       and codeLens_field = ref None
       and documentLink_field = ref None
       and colorProvider_field = ref None
       and formatting_field = ref None
       and rangeFormatting_field = ref None
       and onTypeFormatting_field = ref None
       and rename_field = ref None
       and publishDiagnostics_field = ref None
       and foldingRange_field = ref None
       and selectionRange_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "synchronization" -> (
             match Ppx_yojson_conv_lib.( ! ) synchronization_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   TextDocumentSyncClientCapabilities.t_of_yojson _field_yojson
               in
               synchronization_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "completion" -> (
             match Ppx_yojson_conv_lib.( ! ) completion_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   CompletionClientCapabilities.t_of_yojson _field_yojson
               in
               completion_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "hover" -> (
             match Ppx_yojson_conv_lib.( ! ) hover_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   HoverClientCapabilities.t_of_yojson _field_yojson
               in
               hover_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "signatureHelp" -> (
             match Ppx_yojson_conv_lib.( ! ) signatureHelp_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   SignatureHelpClientCapabilities.t_of_yojson _field_yojson
               in
               signatureHelp_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "declaration" -> (
             match Ppx_yojson_conv_lib.( ! ) declaration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DeclarationClientCapabilities.t_of_yojson _field_yojson
               in
               declaration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "definition" -> (
             match Ppx_yojson_conv_lib.( ! ) definition_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DefinitionClientCapabilities.t_of_yojson _field_yojson
               in
               definition_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "typeDefinition" -> (
             match Ppx_yojson_conv_lib.( ! ) typeDefinition_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   TypeDefinitionClientCapabilities.t_of_yojson _field_yojson
               in
               typeDefinition_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "implementation" -> (
             match Ppx_yojson_conv_lib.( ! ) implementation_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   ImplementationClientCapabilities.t_of_yojson _field_yojson
               in
               implementation_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "references" -> (
             match Ppx_yojson_conv_lib.( ! ) references_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   ReferenceClientCapabilities.t_of_yojson _field_yojson
               in
               references_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentHighlight" -> (
             match Ppx_yojson_conv_lib.( ! ) documentHighlight_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DocumentHighlightClientCapabilities.t_of_yojson _field_yojson
               in
               documentHighlight_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentSymbol" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSymbol_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DocumentSymbolClientCapabilities.t_of_yojson _field_yojson
               in
               documentSymbol_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "codeAction" -> (
             match Ppx_yojson_conv_lib.( ! ) codeAction_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   CodeActionClientCapabilities.t_of_yojson _field_yojson
               in
               codeAction_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "codeLens" -> (
             match Ppx_yojson_conv_lib.( ! ) codeLens_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   CodeLensClientCapabilities.t_of_yojson _field_yojson
               in
               codeLens_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentLink" -> (
             match Ppx_yojson_conv_lib.( ! ) documentLink_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DocumentLinkClientCapabilities.t_of_yojson _field_yojson
               in
               documentLink_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "colorProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) colorProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DocumentColorClientCapabilities.t_of_yojson _field_yojson
               in
               colorProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "formatting" -> (
             match Ppx_yojson_conv_lib.( ! ) formatting_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DocumentFormattingClientCapabilities.t_of_yojson
                   _field_yojson
               in
               formatting_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "rangeFormatting" -> (
             match Ppx_yojson_conv_lib.( ! ) rangeFormatting_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DocumentRangeFormattingClientCapabilities.t_of_yojson
                   _field_yojson
               in
               rangeFormatting_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "onTypeFormatting" -> (
             match Ppx_yojson_conv_lib.( ! ) onTypeFormatting_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DocumentOnTypeFormattingClientCapabilities.t_of_yojson
                   _field_yojson
               in
               onTypeFormatting_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "rename" -> (
             match Ppx_yojson_conv_lib.( ! ) rename_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   RenameClientCapabilities.t_of_yojson _field_yojson
               in
               rename_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "publishDiagnostics" -> (
             match Ppx_yojson_conv_lib.( ! ) publishDiagnostics_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   PublishDiagnosticsClientCapabilities.t_of_yojson
                   _field_yojson
               in
               publishDiagnostics_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "foldingRange" -> (
             match Ppx_yojson_conv_lib.( ! ) foldingRange_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   FoldingRangeClientCapabilities.t_of_yojson _field_yojson
               in
               foldingRange_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "selectionRange" -> (
             match Ppx_yojson_conv_lib.( ! ) selectionRange_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   SelectionRangeClientCapabilities.t_of_yojson _field_yojson
               in
               selectionRange_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( synchronization_value
               , completion_value
               , hover_value
               , signatureHelp_value
               , declaration_value
               , definition_value
               , typeDefinition_value
               , implementation_value
               , references_value
               , documentHighlight_value
               , documentSymbol_value
               , codeAction_value
               , codeLens_value
               , documentLink_value
               , colorProvider_value
               , formatting_value
               , rangeFormatting_value
               , onTypeFormatting_value
               , rename_value
               , publishDiagnostics_value
               , foldingRange_value
               , selectionRange_value ) =
             ( Ppx_yojson_conv_lib.( ! ) synchronization_field
             , Ppx_yojson_conv_lib.( ! ) completion_field
             , Ppx_yojson_conv_lib.( ! ) hover_field
             , Ppx_yojson_conv_lib.( ! ) signatureHelp_field
             , Ppx_yojson_conv_lib.( ! ) declaration_field
             , Ppx_yojson_conv_lib.( ! ) definition_field
             , Ppx_yojson_conv_lib.( ! ) typeDefinition_field
             , Ppx_yojson_conv_lib.( ! ) implementation_field
             , Ppx_yojson_conv_lib.( ! ) references_field
             , Ppx_yojson_conv_lib.( ! ) documentHighlight_field
             , Ppx_yojson_conv_lib.( ! ) documentSymbol_field
             , Ppx_yojson_conv_lib.( ! ) codeAction_field
             , Ppx_yojson_conv_lib.( ! ) codeLens_field
             , Ppx_yojson_conv_lib.( ! ) documentLink_field
             , Ppx_yojson_conv_lib.( ! ) colorProvider_field
             , Ppx_yojson_conv_lib.( ! ) formatting_field
             , Ppx_yojson_conv_lib.( ! ) rangeFormatting_field
             , Ppx_yojson_conv_lib.( ! ) onTypeFormatting_field
             , Ppx_yojson_conv_lib.( ! ) rename_field
             , Ppx_yojson_conv_lib.( ! ) publishDiagnostics_field
             , Ppx_yojson_conv_lib.( ! ) foldingRange_field
             , Ppx_yojson_conv_lib.( ! ) selectionRange_field )
           in
           { synchronization =
               (match synchronization_value with
               | None -> None
               | Some v -> v)
           ; completion =
               (match completion_value with
               | None -> None
               | Some v -> v)
           ; hover =
               (match hover_value with
               | None -> None
               | Some v -> v)
           ; signatureHelp =
               (match signatureHelp_value with
               | None -> None
               | Some v -> v)
           ; declaration =
               (match declaration_value with
               | None -> None
               | Some v -> v)
           ; definition =
               (match definition_value with
               | None -> None
               | Some v -> v)
           ; typeDefinition =
               (match typeDefinition_value with
               | None -> None
               | Some v -> v)
           ; implementation =
               (match implementation_value with
               | None -> None
               | Some v -> v)
           ; references =
               (match references_value with
               | None -> None
               | Some v -> v)
           ; documentHighlight =
               (match documentHighlight_value with
               | None -> None
               | Some v -> v)
           ; documentSymbol =
               (match documentSymbol_value with
               | None -> None
               | Some v -> v)
           ; codeAction =
               (match codeAction_value with
               | None -> None
               | Some v -> v)
           ; codeLens =
               (match codeLens_value with
               | None -> None
               | Some v -> v)
           ; documentLink =
               (match documentLink_value with
               | None -> None
               | Some v -> v)
           ; colorProvider =
               (match colorProvider_value with
               | None -> None
               | Some v -> v)
           ; formatting =
               (match formatting_value with
               | None -> None
               | Some v -> v)
           ; rangeFormatting =
               (match rangeFormatting_value with
               | None -> None
               | Some v -> v)
           ; onTypeFormatting =
               (match onTypeFormatting_value with
               | None -> None
               | Some v -> v)
           ; rename =
               (match rename_value with
               | None -> None
               | Some v -> v)
           ; publishDiagnostics =
               (match publishDiagnostics_value with
               | None -> None
               | Some v -> v)
           ; foldingRange =
               (match foldingRange_value with
               | None -> None
               | Some v -> v)
           ; selectionRange =
               (match selectionRange_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { synchronization = v_synchronization
       ; completion = v_completion
       ; hover = v_hover
       ; signatureHelp = v_signatureHelp
       ; declaration = v_declaration
       ; definition = v_definition
       ; typeDefinition = v_typeDefinition
       ; implementation = v_implementation
       ; references = v_references
       ; documentHighlight = v_documentHighlight
       ; documentSymbol = v_documentSymbol
       ; codeAction = v_codeAction
       ; codeLens = v_codeLens
       ; documentLink = v_documentLink
       ; colorProvider = v_colorProvider
       ; formatting = v_formatting
       ; rangeFormatting = v_rangeFormatting
       ; onTypeFormatting = v_onTypeFormatting
       ; rename = v_rename
       ; publishDiagnostics = v_publishDiagnostics
       ; foldingRange = v_foldingRange
       ; selectionRange = v_selectionRange
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_selectionRange then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                SelectionRangeClientCapabilities.yojson_of_t)
               v_selectionRange
           in
           let bnd = ("selectionRange", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_foldingRange then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                FoldingRangeClientCapabilities.yojson_of_t)
               v_foldingRange
           in
           let bnd = ("foldingRange", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_publishDiagnostics then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                PublishDiagnosticsClientCapabilities.yojson_of_t)
               v_publishDiagnostics
           in
           let bnd = ("publishDiagnostics", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_rename then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                RenameClientCapabilities.yojson_of_t)
               v_rename
           in
           let bnd = ("rename", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_onTypeFormatting then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                DocumentOnTypeFormattingClientCapabilities.yojson_of_t)
               v_onTypeFormatting
           in
           let bnd = ("onTypeFormatting", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_rangeFormatting then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                DocumentRangeFormattingClientCapabilities.yojson_of_t)
               v_rangeFormatting
           in
           let bnd = ("rangeFormatting", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_formatting then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                DocumentFormattingClientCapabilities.yojson_of_t)
               v_formatting
           in
           let bnd = ("formatting", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_colorProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                DocumentColorClientCapabilities.yojson_of_t)
               v_colorProvider
           in
           let bnd = ("colorProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentLink then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                DocumentLinkClientCapabilities.yojson_of_t)
               v_documentLink
           in
           let bnd = ("documentLink", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_codeLens then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                CodeLensClientCapabilities.yojson_of_t)
               v_codeLens
           in
           let bnd = ("codeLens", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_codeAction then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                CodeActionClientCapabilities.yojson_of_t)
               v_codeAction
           in
           let bnd = ("codeAction", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSymbol then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                DocumentSymbolClientCapabilities.yojson_of_t)
               v_documentSymbol
           in
           let bnd = ("documentSymbol", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentHighlight then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                DocumentHighlightClientCapabilities.yojson_of_t)
               v_documentHighlight
           in
           let bnd = ("documentHighlight", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_references then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                ReferenceClientCapabilities.yojson_of_t)
               v_references
           in
           let bnd = ("references", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_implementation then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                ImplementationClientCapabilities.yojson_of_t)
               v_implementation
           in
           let bnd = ("implementation", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_typeDefinition then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                TypeDefinitionClientCapabilities.yojson_of_t)
               v_typeDefinition
           in
           let bnd = ("typeDefinition", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_definition then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                DefinitionClientCapabilities.yojson_of_t)
               v_definition
           in
           let bnd = ("definition", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_declaration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                DeclarationClientCapabilities.yojson_of_t)
               v_declaration
           in
           let bnd = ("declaration", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_signatureHelp then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                SignatureHelpClientCapabilities.yojson_of_t)
               v_signatureHelp
           in
           let bnd = ("signatureHelp", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_hover then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                HoverClientCapabilities.yojson_of_t)
               v_hover
           in
           let bnd = ("hover", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_completion then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                CompletionClientCapabilities.yojson_of_t)
               v_completion
           in
           let bnd = ("completion", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_synchronization then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                TextDocumentSyncClientCapabilities.yojson_of_t)
               v_synchronization
           in
           let bnd = ("synchronization", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(synchronization : TextDocumentSyncClientCapabilities.t option)
      ?(completion : CompletionClientCapabilities.t option)
      ?(hover : HoverClientCapabilities.t option)
      ?(signatureHelp : SignatureHelpClientCapabilities.t option)
      ?(declaration : DeclarationClientCapabilities.t option)
      ?(definition : DefinitionClientCapabilities.t option)
      ?(typeDefinition : TypeDefinitionClientCapabilities.t option)
      ?(implementation : ImplementationClientCapabilities.t option)
      ?(references : ReferenceClientCapabilities.t option)
      ?(documentHighlight : DocumentHighlightClientCapabilities.t option)
      ?(documentSymbol : DocumentSymbolClientCapabilities.t option)
      ?(codeAction : CodeActionClientCapabilities.t option)
      ?(codeLens : CodeLensClientCapabilities.t option)
      ?(documentLink : DocumentLinkClientCapabilities.t option)
      ?(colorProvider : DocumentColorClientCapabilities.t option)
      ?(formatting : DocumentFormattingClientCapabilities.t option)
      ?(rangeFormatting : DocumentRangeFormattingClientCapabilities.t option)
      ?(onTypeFormatting : DocumentOnTypeFormattingClientCapabilities.t option)
      ?(rename : RenameClientCapabilities.t option)
      ?(publishDiagnostics : PublishDiagnosticsClientCapabilities.t option)
      ?(foldingRange : FoldingRangeClientCapabilities.t option)
      ?(selectionRange : SelectionRangeClientCapabilities.t option) (() : unit)
      : t =
    { synchronization
    ; completion
    ; hover
    ; signatureHelp
    ; declaration
    ; definition
    ; typeDefinition
    ; implementation
    ; references
    ; documentHighlight
    ; documentSymbol
    ; codeAction
    ; codeLens
    ; documentLink
    ; colorProvider
    ; formatting
    ; rangeFormatting
    ; onTypeFormatting
    ; rename
    ; publishDiagnostics
    ; foldingRange
    ; selectionRange
    }
end

module ExecuteCommandClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ExecuteCommandClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value =
             Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
end

module WorkspaceSymbolClientCapabilities = struct
  type symbolKind =
    { valueSet : SymbolKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : symbolKind) -> ()

  let symbolKind_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.WorkspaceSymbolClientCapabilities.symbolKind"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let valueSet_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "valueSet" -> (
             match Ppx_yojson_conv_lib.( ! ) valueSet_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson SymbolKind.t_of_yojson)
                   _field_yojson
               in
               valueSet_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let valueSet_value = Ppx_yojson_conv_lib.( ! ) valueSet_field in
           { valueSet =
               (match valueSet_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> symbolKind)

  let _ = symbolKind_of_yojson

  let yojson_of_symbolKind =
    (function
     | { valueSet = v_valueSet } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_valueSet then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list SymbolKind.yojson_of_t))
               v_valueSet
           in
           let bnd = ("valueSet", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : symbolKind -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_symbolKind

  [@@@end]

  let create_symbolKind ?(valueSet : SymbolKind.t list option) (() : unit) :
      symbolKind =
    { valueSet }

  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; symbolKind : symbolKind Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkspaceSymbolClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and symbolKind_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "symbolKind" -> (
             match Ppx_yojson_conv_lib.( ! ) symbolKind_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson symbolKind_of_yojson
                   _field_yojson
               in
               symbolKind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value, symbolKind_value =
             ( Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
             , Ppx_yojson_conv_lib.( ! ) symbolKind_field )
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           ; symbolKind =
               (match symbolKind_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration
       ; symbolKind = v_symbolKind
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_symbolKind then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_symbolKind)
               v_symbolKind
           in
           let bnd = ("symbolKind", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option)
      ?(symbolKind : symbolKind option) (() : unit) : t =
    { dynamicRegistration; symbolKind }
end

module DidChangeWatchedFilesClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.DidChangeWatchedFilesClientCapabilities.t"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value =
             Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
end

module DidChangeConfigurationClientCapabilities = struct
  type t =
    { dynamicRegistration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.DidChangeConfigurationClientCapabilities.t"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let dynamicRegistration_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "dynamicRegistration" -> (
             match Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               dynamicRegistration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let dynamicRegistration_value =
             Ppx_yojson_conv_lib.( ! ) dynamicRegistration_field
           in
           { dynamicRegistration =
               (match dynamicRegistration_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { dynamicRegistration = v_dynamicRegistration } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_dynamicRegistration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_dynamicRegistration
           in
           let bnd = ("dynamicRegistration", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(dynamicRegistration : bool option) (() : unit) : t =
    { dynamicRegistration }
end

module FailureHandlingKind = struct
  type t =
    | Abort
    | Transactional
    | TextOnlyTransactional
    | Undo

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Abort -> `String "abort"
    | Transactional -> `String "transactional"
    | TextOnlyTransactional -> `String "textOnlyTransactional"
    | Undo -> `String "undo"

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "abort" -> Abort
    | `String "transactional" -> Transactional
    | `String "textOnlyTransactional" -> TextOnlyTransactional
    | `String "undo" -> Undo
    | _ -> Json.error "t" json
end

module ResourceOperationKind = struct
  type t =
    | Create
    | Rename
    | Delete

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Create -> `String "create"
    | Rename -> `String "rename"
    | Delete -> `String "delete"

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "create" -> Create
    | `String "rename" -> Rename
    | `String "delete" -> Delete
    | _ -> Json.error "t" json
end

module WorkspaceEditClientCapabilities = struct
  type t =
    { documentChanges : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resourceOperations : ResourceOperationKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; failureHandling : FailureHandlingKind.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkspaceEditClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentChanges_field = ref None
       and resourceOperations_field = ref None
       and failureHandling_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentChanges" -> (
             match Ppx_yojson_conv_lib.( ! ) documentChanges_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               documentChanges_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "resourceOperations" -> (
             match Ppx_yojson_conv_lib.( ! ) resourceOperations_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson ResourceOperationKind.t_of_yojson)
                   _field_yojson
               in
               resourceOperations_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "failureHandling" -> (
             match Ppx_yojson_conv_lib.( ! ) failureHandling_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   FailureHandlingKind.t_of_yojson _field_yojson
               in
               failureHandling_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( documentChanges_value
               , resourceOperations_value
               , failureHandling_value ) =
             ( Ppx_yojson_conv_lib.( ! ) documentChanges_field
             , Ppx_yojson_conv_lib.( ! ) resourceOperations_field
             , Ppx_yojson_conv_lib.( ! ) failureHandling_field )
           in
           { documentChanges =
               (match documentChanges_value with
               | None -> None
               | Some v -> v)
           ; resourceOperations =
               (match resourceOperations_value with
               | None -> None
               | Some v -> v)
           ; failureHandling =
               (match failureHandling_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentChanges = v_documentChanges
       ; resourceOperations = v_resourceOperations
       ; failureHandling = v_failureHandling
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_failureHandling then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t FailureHandlingKind.yojson_of_t)
               v_failureHandling
           in
           let bnd = ("failureHandling", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_resourceOperations then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list ResourceOperationKind.yojson_of_t))
               v_resourceOperations
           in
           let bnd = ("resourceOperations", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentChanges then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_documentChanges
           in
           let bnd = ("documentChanges", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentChanges : bool option)
      ?(resourceOperations : ResourceOperationKind.t list option)
      ?(failureHandling : FailureHandlingKind.t option) (() : unit) : t =
    { documentChanges; resourceOperations; failureHandling }
end

module ClientCapabilities = struct
  type window =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : window) -> ()

  let window_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ClientCapabilities.window" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> window)

  let _ = window_of_yojson

  let yojson_of_window =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : window -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_window

  [@@@end]

  let create_window ?(workDoneProgress : bool option) (() : unit) : window =
    { workDoneProgress }

  type workspace =
    { applyEdit : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workspaceEdit : WorkspaceEditClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; didChangeConfiguration :
        DidChangeConfigurationClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; didChangeWatchedFiles :
        DidChangeWatchedFilesClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; symbol : WorkspaceSymbolClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; executeCommand : ExecuteCommandClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workspaceFolders : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; configuration : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : workspace) -> ()

  let workspace_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ClientCapabilities.workspace" in
     function
     | `Assoc field_yojsons as yojson -> (
       let applyEdit_field = ref None
       and workspaceEdit_field = ref None
       and didChangeConfiguration_field = ref None
       and didChangeWatchedFiles_field = ref None
       and symbol_field = ref None
       and executeCommand_field = ref None
       and workspaceFolders_field = ref None
       and configuration_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "applyEdit" -> (
             match Ppx_yojson_conv_lib.( ! ) applyEdit_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               applyEdit_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workspaceEdit" -> (
             match Ppx_yojson_conv_lib.( ! ) workspaceEdit_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   WorkspaceEditClientCapabilities.t_of_yojson _field_yojson
               in
               workspaceEdit_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "didChangeConfiguration" -> (
             match Ppx_yojson_conv_lib.( ! ) didChangeConfiguration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DidChangeConfigurationClientCapabilities.t_of_yojson
                   _field_yojson
               in
               didChangeConfiguration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "didChangeWatchedFiles" -> (
             match Ppx_yojson_conv_lib.( ! ) didChangeWatchedFiles_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DidChangeWatchedFilesClientCapabilities.t_of_yojson
                   _field_yojson
               in
               didChangeWatchedFiles_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "symbol" -> (
             match Ppx_yojson_conv_lib.( ! ) symbol_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   WorkspaceSymbolClientCapabilities.t_of_yojson _field_yojson
               in
               symbol_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "executeCommand" -> (
             match Ppx_yojson_conv_lib.( ! ) executeCommand_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   ExecuteCommandClientCapabilities.t_of_yojson _field_yojson
               in
               executeCommand_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workspaceFolders" -> (
             match Ppx_yojson_conv_lib.( ! ) workspaceFolders_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workspaceFolders_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "configuration" -> (
             match Ppx_yojson_conv_lib.( ! ) configuration_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               configuration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( applyEdit_value
               , workspaceEdit_value
               , didChangeConfiguration_value
               , didChangeWatchedFiles_value
               , symbol_value
               , executeCommand_value
               , workspaceFolders_value
               , configuration_value ) =
             ( Ppx_yojson_conv_lib.( ! ) applyEdit_field
             , Ppx_yojson_conv_lib.( ! ) workspaceEdit_field
             , Ppx_yojson_conv_lib.( ! ) didChangeConfiguration_field
             , Ppx_yojson_conv_lib.( ! ) didChangeWatchedFiles_field
             , Ppx_yojson_conv_lib.( ! ) symbol_field
             , Ppx_yojson_conv_lib.( ! ) executeCommand_field
             , Ppx_yojson_conv_lib.( ! ) workspaceFolders_field
             , Ppx_yojson_conv_lib.( ! ) configuration_field )
           in
           { applyEdit =
               (match applyEdit_value with
               | None -> None
               | Some v -> v)
           ; workspaceEdit =
               (match workspaceEdit_value with
               | None -> None
               | Some v -> v)
           ; didChangeConfiguration =
               (match didChangeConfiguration_value with
               | None -> None
               | Some v -> v)
           ; didChangeWatchedFiles =
               (match didChangeWatchedFiles_value with
               | None -> None
               | Some v -> v)
           ; symbol =
               (match symbol_value with
               | None -> None
               | Some v -> v)
           ; executeCommand =
               (match executeCommand_value with
               | None -> None
               | Some v -> v)
           ; workspaceFolders =
               (match workspaceFolders_value with
               | None -> None
               | Some v -> v)
           ; configuration =
               (match configuration_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> workspace)

  let _ = workspace_of_yojson

  let yojson_of_workspace =
    (function
     | { applyEdit = v_applyEdit
       ; workspaceEdit = v_workspaceEdit
       ; didChangeConfiguration = v_didChangeConfiguration
       ; didChangeWatchedFiles = v_didChangeWatchedFiles
       ; symbol = v_symbol
       ; executeCommand = v_executeCommand
       ; workspaceFolders = v_workspaceFolders
       ; configuration = v_configuration
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_configuration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_configuration
           in
           let bnd = ("configuration", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workspaceFolders then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workspaceFolders
           in
           let bnd = ("workspaceFolders", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_executeCommand then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                ExecuteCommandClientCapabilities.yojson_of_t)
               v_executeCommand
           in
           let bnd = ("executeCommand", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_symbol then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                WorkspaceSymbolClientCapabilities.yojson_of_t)
               v_symbol
           in
           let bnd = ("symbol", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_didChangeWatchedFiles then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                DidChangeWatchedFilesClientCapabilities.yojson_of_t)
               v_didChangeWatchedFiles
           in
           let bnd = ("didChangeWatchedFiles", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_didChangeConfiguration then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                DidChangeConfigurationClientCapabilities.yojson_of_t)
               v_didChangeConfiguration
           in
           let bnd = ("didChangeConfiguration", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workspaceEdit then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                WorkspaceEditClientCapabilities.yojson_of_t)
               v_workspaceEdit
           in
           let bnd = ("workspaceEdit", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_applyEdit then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_applyEdit
           in
           let bnd = ("applyEdit", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : workspace -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_workspace

  [@@@end]

  let create_workspace ?(applyEdit : bool option)
      ?(workspaceEdit : WorkspaceEditClientCapabilities.t option)
      ?(didChangeConfiguration :
         DidChangeConfigurationClientCapabilities.t option)
      ?(didChangeWatchedFiles :
         DidChangeWatchedFilesClientCapabilities.t option)
      ?(symbol : WorkspaceSymbolClientCapabilities.t option)
      ?(executeCommand : ExecuteCommandClientCapabilities.t option)
      ?(workspaceFolders : bool option) ?(configuration : bool option)
      (() : unit) : workspace =
    { applyEdit
    ; workspaceEdit
    ; didChangeConfiguration
    ; didChangeWatchedFiles
    ; symbol
    ; executeCommand
    ; workspaceFolders
    ; configuration
    }

  type t =
    { workspace : workspace Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; textDocument : TextDocumentClientCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; window : window Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; experimental : Json.t option [@yojson.option]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ClientCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workspace_field = ref None
       and textDocument_field = ref None
       and window_field = ref None
       and experimental_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workspace" -> (
             match Ppx_yojson_conv_lib.( ! ) workspace_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson workspace_of_yojson
                   _field_yojson
               in
               workspace_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   TextDocumentClientCapabilities.t_of_yojson _field_yojson
               in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "window" -> (
             match Ppx_yojson_conv_lib.( ! ) window_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson window_of_yojson _field_yojson
               in
               window_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "experimental" -> (
             match Ppx_yojson_conv_lib.( ! ) experimental_field with
             | None ->
               let fvalue = Json.t_of_yojson _field_yojson in
               experimental_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( workspace_value
               , textDocument_value
               , window_value
               , experimental_value ) =
             ( Ppx_yojson_conv_lib.( ! ) workspace_field
             , Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) window_field
             , Ppx_yojson_conv_lib.( ! ) experimental_field )
           in
           { workspace =
               (match workspace_value with
               | None -> None
               | Some v -> v)
           ; textDocument =
               (match textDocument_value with
               | None -> None
               | Some v -> v)
           ; window =
               (match window_value with
               | None -> None
               | Some v -> v)
           ; experimental = experimental_value
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workspace = v_workspace
       ; textDocument = v_textDocument
       ; window = v_window
       ; experimental = v_experimental
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         match v_experimental with
         | None -> bnds
         | Some v ->
           let arg = Json.yojson_of_t v in
           let bnd = ("experimental", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_window then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_window) v_window
           in
           let bnd = ("window", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_textDocument then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                TextDocumentClientCapabilities.yojson_of_t)
               v_textDocument
           in
           let bnd = ("textDocument", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workspace then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_workspace) v_workspace
           in
           let bnd = ("workspace", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workspace : workspace option)
      ?(textDocument : TextDocumentClientCapabilities.t option)
      ?(window : window option) ?(experimental : Json.t option) (() : unit) : t
      =
    { workspace; textDocument; window; experimental }
end

module Command = struct
  type t =
    { title : string
    ; command : string
    ; arguments : Json.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.Command.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let title_field = ref None
       and command_field = ref None
       and arguments_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "title" -> (
             match Ppx_yojson_conv_lib.( ! ) title_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               title_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "command" -> (
             match Ppx_yojson_conv_lib.( ! ) command_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               command_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "arguments" -> (
             match Ppx_yojson_conv_lib.( ! ) arguments_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson Json.t_of_yojson)
                   _field_yojson
               in
               arguments_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) title_field
             , Ppx_yojson_conv_lib.( ! ) command_field
             , Ppx_yojson_conv_lib.( ! ) arguments_field )
           with
           | Some title_value, Some command_value, arguments_value ->
             { title = title_value
             ; command = command_value
             ; arguments =
                 (match arguments_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) title_field)
                     None
                 , "title" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) command_field)
                     None
                 , "command" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { title = v_title; command = v_command; arguments = v_arguments } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_arguments then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list Json.yojson_of_t))
               v_arguments
           in
           let bnd = ("arguments", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_command in
         ("command", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_title in
         ("title", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(title : string) ~(command : string)
      ?(arguments : Json.t list option) (() : unit) : t =
    { title; command; arguments }
end

module Location = struct
  type t =
    { uri : DocumentUri.t
    ; range : Range.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.Location.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let uri_field = ref None
       and range_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "uri" -> (
             match Ppx_yojson_conv_lib.( ! ) uri_field with
             | None ->
               let fvalue = DocumentUri.t_of_yojson _field_yojson in
               uri_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "range" -> (
             match Ppx_yojson_conv_lib.( ! ) range_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               range_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) uri_field
             , Ppx_yojson_conv_lib.( ! ) range_field )
           with
           | Some uri_value, Some range_value ->
             { uri = uri_value; range = range_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) uri_field)
                     None
                 , "uri" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) range_field)
                     None
                 , "range" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { uri = v_uri; range = v_range } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Range.yojson_of_t v_range in
         ("range", arg) :: bnds
       in
       let bnds =
         let arg = DocumentUri.yojson_of_t v_uri in
         ("uri", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(uri : DocumentUri.t) ~(range : Range.t) : t = { uri; range }
end

module DiagnosticRelatedInformation = struct
  type t =
    { location : Location.t
    ; message : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DiagnosticRelatedInformation.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let location_field = ref None
       and message_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "location" -> (
             match Ppx_yojson_conv_lib.( ! ) location_field with
             | None ->
               let fvalue = Location.t_of_yojson _field_yojson in
               location_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "message" -> (
             match Ppx_yojson_conv_lib.( ! ) message_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               message_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) location_field
             , Ppx_yojson_conv_lib.( ! ) message_field )
           with
           | Some location_value, Some message_value ->
             { location = location_value; message = message_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) location_field)
                     None
                 , "location" )
               ; ( Ppx_yojson_conv_lib.poly_equal
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
     | { location = v_location; message = v_message } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_message in
         ("message", arg) :: bnds
       in
       let bnds =
         let arg = Location.yojson_of_t v_location in
         ("location", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(location : Location.t) ~(message : string) : t =
    { location; message }
end

module DiagnosticSeverity = struct
  type t =
    | Error
    | Warning
    | Information
    | Hint

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Error -> `Int 1
    | Warning -> `Int 2
    | Information -> `Int 3
    | Hint -> `Int 4

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Error
    | `Int 2 -> Warning
    | `Int 3 -> Information
    | `Int 4 -> Hint
    | _ -> Json.error "t" json
end

module Diagnostic = struct
  type t =
    { range : Range.t
    ; severity : DiagnosticSeverity.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; code : Jsonrpc.Id.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; source : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; message : string
    ; tags : DiagnosticTag.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; relatedInformation :
        DiagnosticRelatedInformation.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.Diagnostic.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let range_field = ref None
       and severity_field = ref None
       and code_field = ref None
       and source_field = ref None
       and message_field = ref None
       and tags_field = ref None
       and relatedInformation_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "range" -> (
             match Ppx_yojson_conv_lib.( ! ) range_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               range_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "severity" -> (
             match Ppx_yojson_conv_lib.( ! ) severity_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DiagnosticSeverity.t_of_yojson
                   _field_yojson
               in
               severity_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "code" -> (
             match Ppx_yojson_conv_lib.( ! ) code_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson Jsonrpc.Id.t_of_yojson
                   _field_yojson
               in
               code_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "source" -> (
             match Ppx_yojson_conv_lib.( ! ) source_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               source_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "message" -> (
             match Ppx_yojson_conv_lib.( ! ) message_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               message_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "tags" -> (
             match Ppx_yojson_conv_lib.( ! ) tags_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson DiagnosticTag.t_of_yojson)
                   _field_yojson
               in
               tags_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "relatedInformation" -> (
             match Ppx_yojson_conv_lib.( ! ) relatedInformation_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson DiagnosticRelatedInformation.t_of_yojson)
                   _field_yojson
               in
               relatedInformation_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) range_field
             , Ppx_yojson_conv_lib.( ! ) severity_field
             , Ppx_yojson_conv_lib.( ! ) code_field
             , Ppx_yojson_conv_lib.( ! ) source_field
             , Ppx_yojson_conv_lib.( ! ) message_field
             , Ppx_yojson_conv_lib.( ! ) tags_field
             , Ppx_yojson_conv_lib.( ! ) relatedInformation_field )
           with
           | ( Some range_value
             , severity_value
             , code_value
             , source_value
             , Some message_value
             , tags_value
             , relatedInformation_value ) ->
             { range = range_value
             ; severity =
                 (match severity_value with
                 | None -> None
                 | Some v -> v)
             ; code =
                 (match code_value with
                 | None -> None
                 | Some v -> v)
             ; source =
                 (match source_value with
                 | None -> None
                 | Some v -> v)
             ; message = message_value
             ; tags =
                 (match tags_value with
                 | None -> None
                 | Some v -> v)
             ; relatedInformation =
                 (match relatedInformation_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) range_field)
                     None
                 , "range" )
               ; ( Ppx_yojson_conv_lib.poly_equal
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
     | { range = v_range
       ; severity = v_severity
       ; code = v_code
       ; source = v_source
       ; message = v_message
       ; tags = v_tags
       ; relatedInformation = v_relatedInformation
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_relatedInformation then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list DiagnosticRelatedInformation.yojson_of_t))
               v_relatedInformation
           in
           let bnd = ("relatedInformation", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_tags then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list DiagnosticTag.yojson_of_t))
               v_tags
           in
           let bnd = ("tags", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_message in
         ("message", arg) :: bnds
       in
       let bnds =
         if None = v_source then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_source
           in
           let bnd = ("source", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_code then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t Jsonrpc.Id.yojson_of_t) v_code
           in
           let bnd = ("code", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_severity then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DiagnosticSeverity.yojson_of_t)
               v_severity
           in
           let bnd = ("severity", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = Range.yojson_of_t v_range in
         ("range", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(range : Range.t) ?(severity : DiagnosticSeverity.t option)
      ?(code : Jsonrpc.Id.t option) ?(source : string option)
      ~(message : string) ?(tags : DiagnosticTag.t list option)
      ?(relatedInformation : DiagnosticRelatedInformation.t list option)
      (() : unit) : t =
    { range; severity; code; source; message; tags; relatedInformation }
end

module CodeAction = struct
  type t =
    { title : string
    ; kind : CodeActionKind.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; diagnostics : Diagnostic.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; isPreferred : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; edit : WorkspaceEdit.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; command : Command.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CodeAction.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let title_field = ref None
       and kind_field = ref None
       and diagnostics_field = ref None
       and isPreferred_field = ref None
       and edit_field = ref None
       and command_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "title" -> (
             match Ppx_yojson_conv_lib.( ! ) title_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               title_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "kind" -> (
             match Ppx_yojson_conv_lib.( ! ) kind_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson CodeActionKind.t_of_yojson
                   _field_yojson
               in
               kind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "diagnostics" -> (
             match Ppx_yojson_conv_lib.( ! ) diagnostics_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson Diagnostic.t_of_yojson)
                   _field_yojson
               in
               diagnostics_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "isPreferred" -> (
             match Ppx_yojson_conv_lib.( ! ) isPreferred_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               isPreferred_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "edit" -> (
             match Ppx_yojson_conv_lib.( ! ) edit_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson WorkspaceEdit.t_of_yojson
                   _field_yojson
               in
               edit_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "command" -> (
             match Ppx_yojson_conv_lib.( ! ) command_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson Command.t_of_yojson
                   _field_yojson
               in
               command_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) title_field
             , Ppx_yojson_conv_lib.( ! ) kind_field
             , Ppx_yojson_conv_lib.( ! ) diagnostics_field
             , Ppx_yojson_conv_lib.( ! ) isPreferred_field
             , Ppx_yojson_conv_lib.( ! ) edit_field
             , Ppx_yojson_conv_lib.( ! ) command_field )
           with
           | ( Some title_value
             , kind_value
             , diagnostics_value
             , isPreferred_value
             , edit_value
             , command_value ) ->
             { title = title_value
             ; kind =
                 (match kind_value with
                 | None -> None
                 | Some v -> v)
             ; diagnostics =
                 (match diagnostics_value with
                 | None -> None
                 | Some v -> v)
             ; isPreferred =
                 (match isPreferred_value with
                 | None -> None
                 | Some v -> v)
             ; edit =
                 (match edit_value with
                 | None -> None
                 | Some v -> v)
             ; command =
                 (match command_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) title_field)
                     None
                 , "title" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { title = v_title
       ; kind = v_kind
       ; diagnostics = v_diagnostics
       ; isPreferred = v_isPreferred
       ; edit = v_edit
       ; command = v_command
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_command then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t Command.yojson_of_t) v_command
           in
           let bnd = ("command", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_edit then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t WorkspaceEdit.yojson_of_t) v_edit
           in
           let bnd = ("edit", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_isPreferred then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_isPreferred
           in
           let bnd = ("isPreferred", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_diagnostics then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list Diagnostic.yojson_of_t))
               v_diagnostics
           in
           let bnd = ("diagnostics", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_kind then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t CodeActionKind.yojson_of_t)
               v_kind
           in
           let bnd = ("kind", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_title in
         ("title", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(title : string) ?(kind : CodeActionKind.t option)
      ?(diagnostics : Diagnostic.t list option) ?(isPreferred : bool option)
      ?(edit : WorkspaceEdit.t option) ?(command : Command.t option) (() : unit)
      : t =
    { title; kind; diagnostics; isPreferred; edit; command }
end

module CodeActionContext = struct
  type t =
    { diagnostics : Diagnostic.t list
    ; only : CodeActionKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CodeActionContext.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let diagnostics_field = ref None
       and only_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "diagnostics" -> (
             match Ppx_yojson_conv_lib.( ! ) diagnostics_field with
             | None ->
               let fvalue =
                 list_of_yojson Diagnostic.t_of_yojson _field_yojson
               in
               diagnostics_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "only" -> (
             match Ppx_yojson_conv_lib.( ! ) only_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson CodeActionKind.t_of_yojson)
                   _field_yojson
               in
               only_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) diagnostics_field
             , Ppx_yojson_conv_lib.( ! ) only_field )
           with
           | Some diagnostics_value, only_value ->
             { diagnostics = diagnostics_value
             ; only =
                 (match only_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) diagnostics_field)
                     None
                 , "diagnostics" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { diagnostics = v_diagnostics; only = v_only } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_only then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list CodeActionKind.yojson_of_t))
               v_only
           in
           let bnd = ("only", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_list Diagnostic.yojson_of_t v_diagnostics in
         ("diagnostics", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(diagnostics : Diagnostic.t list)
      ?(only : CodeActionKind.t list option) (() : unit) : t =
    { diagnostics; only }
end

module WorkDoneProgressOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkDoneProgressOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module CodeActionOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeActionKinds : CodeActionKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CodeActionOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and codeActionKinds_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "codeActionKinds" -> (
             match Ppx_yojson_conv_lib.( ! ) codeActionKinds_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson CodeActionKind.t_of_yojson)
                   _field_yojson
               in
               codeActionKinds_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value, codeActionKinds_value =
             ( Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) codeActionKinds_field )
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; codeActionKinds =
               (match codeActionKinds_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress
       ; codeActionKinds = v_codeActionKinds
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_codeActionKinds then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list CodeActionKind.yojson_of_t))
               v_codeActionKinds
           in
           let bnd = ("codeActionKinds", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option)
      ?(codeActionKinds : CodeActionKind.t list option) (() : unit) : t =
    { workDoneProgress; codeActionKinds }
end

module ProgressToken = struct
  type t = Jsonrpc.Id.t [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (Jsonrpc.Id.t_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (Jsonrpc.Id.yojson_of_t : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]
end

module PartialResultParams = struct
  type t =
    { partialResultToken : ProgressToken.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.PartialResultParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let partialResultToken_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "partialResultToken" -> (
             match Ppx_yojson_conv_lib.( ! ) partialResultToken_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson ProgressToken.t_of_yojson
                   _field_yojson
               in
               partialResultToken_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let partialResultToken_value =
             Ppx_yojson_conv_lib.( ! ) partialResultToken_field
           in
           { partialResultToken =
               (match partialResultToken_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { partialResultToken = v_partialResultToken } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_partialResultToken then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t ProgressToken.yojson_of_t)
               v_partialResultToken
           in
           let bnd = ("partialResultToken", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(partialResultToken : ProgressToken.t option) (() : unit) : t =
    { partialResultToken }
end

module WorkDoneProgressParams = struct
  type t =
    { workDoneToken : ProgressToken.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkDoneProgressParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneToken_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneToken" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneToken_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson ProgressToken.t_of_yojson
                   _field_yojson
               in
               workDoneToken_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneToken_value =
             Ppx_yojson_conv_lib.( ! ) workDoneToken_field
           in
           { workDoneToken =
               (match workDoneToken_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneToken = v_workDoneToken } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneToken then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t ProgressToken.yojson_of_t)
               v_workDoneToken
           in
           let bnd = ("workDoneToken", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneToken : ProgressToken.t option) (() : unit) : t =
    { workDoneToken }
end

module CodeActionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; context : CodeActionContext.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CodeActionParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and range_field = ref None
       and context_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "range" -> (
             match Ppx_yojson_conv_lib.( ! ) range_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               range_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "context" -> (
             match Ppx_yojson_conv_lib.( ! ) context_field with
             | None ->
               let fvalue = CodeActionContext.t_of_yojson _field_yojson in
               context_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) range_field
             , Ppx_yojson_conv_lib.( ! ) context_field )
           with
           | Some textDocument_value, Some range_value, Some context_value ->
             { textDocument = textDocument_value
             ; range = range_value
             ; context = context_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) range_field)
                     None
                 , "range" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) context_field)
                     None
                 , "context" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; range = v_range; context = v_context }
       ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = CodeActionContext.yojson_of_t v_context in
         ("context", arg) :: bnds
       in
       let bnds =
         let arg = Range.yojson_of_t v_range in
         ("range", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(range : Range.t)
      ~(context : CodeActionContext.t) : t =
    { textDocument; range; context }
end

module DocumentFilter = struct
  type t =
    { language : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; scheme : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; pattern : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentFilter.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let language_field = ref None
       and scheme_field = ref None
       and pattern_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "language" -> (
             match Ppx_yojson_conv_lib.( ! ) language_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               language_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "scheme" -> (
             match Ppx_yojson_conv_lib.( ! ) scheme_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               scheme_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "pattern" -> (
             match Ppx_yojson_conv_lib.( ! ) pattern_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               pattern_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let language_value, scheme_value, pattern_value =
             ( Ppx_yojson_conv_lib.( ! ) language_field
             , Ppx_yojson_conv_lib.( ! ) scheme_field
             , Ppx_yojson_conv_lib.( ! ) pattern_field )
           in
           { language =
               (match language_value with
               | None -> None
               | Some v -> v)
           ; scheme =
               (match scheme_value with
               | None -> None
               | Some v -> v)
           ; pattern =
               (match pattern_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { language = v_language; scheme = v_scheme; pattern = v_pattern } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_pattern then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_pattern
           in
           let bnd = ("pattern", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_scheme then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_scheme
           in
           let bnd = ("scheme", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_language then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_language
           in
           let bnd = ("language", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(language : string option) ?(scheme : string option)
      ?(pattern : string option) (() : unit) : t =
    { language; scheme; pattern }
end

module DocumentSelector = struct
  type t = DocumentFilter.t list [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentSelector.t" in
     fun t -> list_of_yojson DocumentFilter.t_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (fun v -> yojson_of_list DocumentFilter.yojson_of_t v
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]
end

module TextDocumentRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TextDocumentRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let documentSelector_value =
             Ppx_yojson_conv_lib.( ! ) documentSelector_field
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option) (() : unit) : t =
    { documentSelector }
end

module CodeActionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeActionKinds : CodeActionKind.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CodeActionRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and codeActionKinds_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "codeActionKinds" -> (
             match Ppx_yojson_conv_lib.( ! ) codeActionKinds_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson CodeActionKind.t_of_yojson)
                   _field_yojson
               in
               codeActionKinds_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( documentSelector_value
               , workDoneProgress_value
               , codeActionKinds_value ) =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) codeActionKinds_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; codeActionKinds =
               (match codeActionKinds_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       ; codeActionKinds = v_codeActionKinds
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_codeActionKinds then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list CodeActionKind.yojson_of_t))
               v_codeActionKinds
           in
           let bnd = ("codeActionKinds", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option)
      ?(codeActionKinds : CodeActionKind.t list option) (() : unit) : t =
    { documentSelector; workDoneProgress; codeActionKinds }
end

module CodeLens = struct
  type t =
    { range : Range.t
    ; command : Command.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; data : Json.t option [@yojson.option]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CodeLens.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let range_field = ref None
       and command_field = ref None
       and data_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "range" -> (
             match Ppx_yojson_conv_lib.( ! ) range_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               range_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "command" -> (
             match Ppx_yojson_conv_lib.( ! ) command_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson Command.t_of_yojson
                   _field_yojson
               in
               command_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "data" -> (
             match Ppx_yojson_conv_lib.( ! ) data_field with
             | None ->
               let fvalue = Json.t_of_yojson _field_yojson in
               data_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) range_field
             , Ppx_yojson_conv_lib.( ! ) command_field
             , Ppx_yojson_conv_lib.( ! ) data_field )
           with
           | Some range_value, command_value, data_value ->
             { range = range_value
             ; command =
                 (match command_value with
                 | None -> None
                 | Some v -> v)
             ; data = data_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) range_field)
                     None
                 , "range" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { range = v_range; command = v_command; data = v_data } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         match v_data with
         | None -> bnds
         | Some v ->
           let arg = Json.yojson_of_t v in
           let bnd = ("data", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_command then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t Command.yojson_of_t) v_command
           in
           let bnd = ("command", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = Range.yojson_of_t v_range in
         ("range", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(range : Range.t) ?(command : Command.t option)
      ?(data : Json.t option) (() : unit) : t =
    { range; command; data }
end

module CodeLensOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CodeLensOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and resolveProvider_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "resolveProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) resolveProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               resolveProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value, resolveProvider_value =
             ( Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) resolveProvider_field )
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; resolveProvider =
               (match resolveProvider_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress
       ; resolveProvider = v_resolveProvider
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_resolveProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_resolveProvider
           in
           let bnd = ("resolveProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) ?(resolveProvider : bool option)
      (() : unit) : t =
    { workDoneProgress; resolveProvider }
end

module CodeLensParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CodeLensParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) textDocument_field with
           | Some textDocument_value -> { textDocument = textDocument_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) : t = { textDocument }
end

module CodeLensRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CodeLensRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and resolveProvider_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "resolveProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) resolveProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               resolveProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( documentSelector_value
               , workDoneProgress_value
               , resolveProvider_value ) =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) resolveProvider_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; resolveProvider =
               (match resolveProvider_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       ; resolveProvider = v_resolveProvider
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_resolveProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_resolveProvider
           in
           let bnd = ("resolveProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option) ?(resolveProvider : bool option)
      (() : unit) : t =
    { documentSelector; workDoneProgress; resolveProvider }
end

module Color = struct
  type t =
    { red : int
    ; green : int
    ; blue : int
    ; alpha : int
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.Color.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let red_field = ref None
       and green_field = ref None
       and blue_field = ref None
       and alpha_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "red" -> (
             match Ppx_yojson_conv_lib.( ! ) red_field with
             | None ->
               let fvalue = int_of_yojson _field_yojson in
               red_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "green" -> (
             match Ppx_yojson_conv_lib.( ! ) green_field with
             | None ->
               let fvalue = int_of_yojson _field_yojson in
               green_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "blue" -> (
             match Ppx_yojson_conv_lib.( ! ) blue_field with
             | None ->
               let fvalue = int_of_yojson _field_yojson in
               blue_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "alpha" -> (
             match Ppx_yojson_conv_lib.( ! ) alpha_field with
             | None ->
               let fvalue = int_of_yojson _field_yojson in
               alpha_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) red_field
             , Ppx_yojson_conv_lib.( ! ) green_field
             , Ppx_yojson_conv_lib.( ! ) blue_field
             , Ppx_yojson_conv_lib.( ! ) alpha_field )
           with
           | Some red_value, Some green_value, Some blue_value, Some alpha_value
             ->
             { red = red_value
             ; green = green_value
             ; blue = blue_value
             ; alpha = alpha_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) red_field)
                     None
                 , "red" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) green_field)
                     None
                 , "green" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) blue_field)
                     None
                 , "blue" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) alpha_field)
                     None
                 , "alpha" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { red = v_red; green = v_green; blue = v_blue; alpha = v_alpha } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_int v_alpha in
         ("alpha", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_int v_blue in
         ("blue", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_int v_green in
         ("green", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_int v_red in
         ("red", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(red : int) ~(green : int) ~(blue : int) ~(alpha : int) : t =
    { red; green; blue; alpha }
end

module ColorInformation = struct
  type t =
    { range : Range.t
    ; color : Color.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ColorInformation.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let range_field = ref None
       and color_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "range" -> (
             match Ppx_yojson_conv_lib.( ! ) range_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               range_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "color" -> (
             match Ppx_yojson_conv_lib.( ! ) color_field with
             | None ->
               let fvalue = Color.t_of_yojson _field_yojson in
               color_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) range_field
             , Ppx_yojson_conv_lib.( ! ) color_field )
           with
           | Some range_value, Some color_value ->
             { range = range_value; color = color_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) range_field)
                     None
                 , "range" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) color_field)
                     None
                 , "color" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { range = v_range; color = v_color } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Color.yojson_of_t v_color in
         ("color", arg) :: bnds
       in
       let bnds =
         let arg = Range.yojson_of_t v_range in
         ("range", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(range : Range.t) ~(color : Color.t) : t = { range; color }
end

module ColorPresentation = struct
  type t =
    { label : string
    ; textEdit : TextEdit.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; additionalTextEdits : TextEdit.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ColorPresentation.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let label_field = ref None
       and textEdit_field = ref None
       and additionalTextEdits_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "label" -> (
             match Ppx_yojson_conv_lib.( ! ) label_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               label_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "textEdit" -> (
             match Ppx_yojson_conv_lib.( ! ) textEdit_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson TextEdit.t_of_yojson
                   _field_yojson
               in
               textEdit_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "additionalTextEdits" -> (
             match Ppx_yojson_conv_lib.( ! ) additionalTextEdits_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson TextEdit.t_of_yojson)
                   _field_yojson
               in
               additionalTextEdits_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) label_field
             , Ppx_yojson_conv_lib.( ! ) textEdit_field
             , Ppx_yojson_conv_lib.( ! ) additionalTextEdits_field )
           with
           | Some label_value, textEdit_value, additionalTextEdits_value ->
             { label = label_value
             ; textEdit =
                 (match textEdit_value with
                 | None -> None
                 | Some v -> v)
             ; additionalTextEdits =
                 (match additionalTextEdits_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) label_field)
                     None
                 , "label" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { label = v_label
       ; textEdit = v_textEdit
       ; additionalTextEdits = v_additionalTextEdits
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_additionalTextEdits then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list TextEdit.yojson_of_t))
               v_additionalTextEdits
           in
           let bnd = ("additionalTextEdits", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_textEdit then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t TextEdit.yojson_of_t) v_textEdit
           in
           let bnd = ("textEdit", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_label in
         ("label", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(label : string) ?(textEdit : TextEdit.t option)
      ?(additionalTextEdits : TextEdit.t list option) (() : unit) : t =
    { label; textEdit; additionalTextEdits }
end

module ColorPresentationParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; color : Color.t
    ; range : Range.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ColorPresentationParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and color_field = ref None
       and range_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "color" -> (
             match Ppx_yojson_conv_lib.( ! ) color_field with
             | None ->
               let fvalue = Color.t_of_yojson _field_yojson in
               color_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "range" -> (
             match Ppx_yojson_conv_lib.( ! ) range_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               range_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) color_field
             , Ppx_yojson_conv_lib.( ! ) range_field )
           with
           | Some textDocument_value, Some color_value, Some range_value ->
             { textDocument = textDocument_value
             ; color = color_value
             ; range = range_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) color_field)
                     None
                 , "color" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) range_field)
                     None
                 , "range" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; color = v_color; range = v_range } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Range.yojson_of_t v_range in
         ("range", arg) :: bnds
       in
       let bnds =
         let arg = Color.yojson_of_t v_color in
         ("color", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(color : Color.t)
      ~(range : Range.t) : t =
    { textDocument; color; range }
end

module CompletionTriggerKind = struct
  type t =
    | Invoked
    | TriggerCharacter
    | TriggerForIncompleteCompletions

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Invoked -> `Int 1
    | TriggerCharacter -> `Int 2
    | TriggerForIncompleteCompletions -> `Int 3

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Invoked
    | `Int 2 -> TriggerCharacter
    | `Int 3 -> TriggerForIncompleteCompletions
    | _ -> Json.error "t" json
end

module CompletionContext = struct
  type t =
    { triggerKind : CompletionTriggerKind.t
    ; triggerCharacter : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CompletionContext.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let triggerKind_field = ref None
       and triggerCharacter_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "triggerKind" -> (
             match Ppx_yojson_conv_lib.( ! ) triggerKind_field with
             | None ->
               let fvalue = CompletionTriggerKind.t_of_yojson _field_yojson in
               triggerKind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "triggerCharacter" -> (
             match Ppx_yojson_conv_lib.( ! ) triggerCharacter_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               triggerCharacter_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) triggerKind_field
             , Ppx_yojson_conv_lib.( ! ) triggerCharacter_field )
           with
           | Some triggerKind_value, triggerCharacter_value ->
             { triggerKind = triggerKind_value
             ; triggerCharacter =
                 (match triggerCharacter_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) triggerKind_field)
                     None
                 , "triggerKind" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { triggerKind = v_triggerKind; triggerCharacter = v_triggerCharacter } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_triggerCharacter then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string)
               v_triggerCharacter
           in
           let bnd = ("triggerCharacter", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = CompletionTriggerKind.yojson_of_t v_triggerKind in
         ("triggerKind", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(triggerKind : CompletionTriggerKind.t)
      ?(triggerCharacter : string option) (() : unit) : t =
    { triggerKind; triggerCharacter }
end

module InsertTextFormat = struct
  type t =
    | PlainText
    | Snippet

  let yojson_of_t (t : t) : Json.t =
    match t with
    | PlainText -> `Int 1
    | Snippet -> `Int 2

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> PlainText
    | `Int 2 -> Snippet
    | _ -> Json.error "t" json
end

module MarkupContent = struct
  type t =
    { kind : MarkupKind.t
    ; value : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.MarkupContent.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let kind_field = ref None
       and value_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "kind" -> (
             match Ppx_yojson_conv_lib.( ! ) kind_field with
             | None ->
               let fvalue = MarkupKind.t_of_yojson _field_yojson in
               kind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "value" -> (
             match Ppx_yojson_conv_lib.( ! ) value_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               value_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) kind_field
             , Ppx_yojson_conv_lib.( ! ) value_field )
           with
           | Some kind_value, Some value_value ->
             { kind = kind_value; value = value_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) kind_field)
                     None
                 , "kind" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) value_field)
                     None
                 , "value" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { kind = v_kind; value = v_value } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_value in
         ("value", arg) :: bnds
       in
       let bnds =
         let arg = MarkupKind.yojson_of_t v_kind in
         ("kind", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(kind : MarkupKind.t) ~(value : string) : t = { kind; value }
end

module CompletionItem = struct
  type documentation =
    [ `String of string
    | `MarkupContent of MarkupContent.t
    ]

  let documentation_of_yojson (json : Json.t) : documentation =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union "documentation"
        [ (fun json -> `MarkupContent (MarkupContent.t_of_yojson json)) ]
        json

  let yojson_of_documentation (documentation : documentation) : Json.t =
    match documentation with
    | `String j -> `String j
    | `MarkupContent s -> MarkupContent.yojson_of_t s

  type t =
    { label : string
    ; kind : CompletionItemKind.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; tags : CompletionItemTag.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; detail : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentation : documentation Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; deprecated : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; preselect : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; sortText : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; filterText : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; insertText : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; insertTextFormat : InsertTextFormat.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; textEdit : TextEdit.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; additionalTextEdits : TextEdit.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; commitCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; command : Command.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; data : Json.t option [@yojson.option]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CompletionItem.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let label_field = ref None
       and kind_field = ref None
       and tags_field = ref None
       and detail_field = ref None
       and documentation_field = ref None
       and deprecated_field = ref None
       and preselect_field = ref None
       and sortText_field = ref None
       and filterText_field = ref None
       and insertText_field = ref None
       and insertTextFormat_field = ref None
       and textEdit_field = ref None
       and additionalTextEdits_field = ref None
       and commitCharacters_field = ref None
       and command_field = ref None
       and data_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "label" -> (
             match Ppx_yojson_conv_lib.( ! ) label_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               label_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "kind" -> (
             match Ppx_yojson_conv_lib.( ! ) kind_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson CompletionItemKind.t_of_yojson
                   _field_yojson
               in
               kind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "tags" -> (
             match Ppx_yojson_conv_lib.( ! ) tags_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson CompletionItemTag.t_of_yojson)
                   _field_yojson
               in
               tags_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "detail" -> (
             match Ppx_yojson_conv_lib.( ! ) detail_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               detail_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentation" -> (
             match Ppx_yojson_conv_lib.( ! ) documentation_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson documentation_of_yojson
                   _field_yojson
               in
               documentation_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "deprecated" -> (
             match Ppx_yojson_conv_lib.( ! ) deprecated_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               deprecated_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "preselect" -> (
             match Ppx_yojson_conv_lib.( ! ) preselect_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               preselect_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "sortText" -> (
             match Ppx_yojson_conv_lib.( ! ) sortText_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               sortText_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "filterText" -> (
             match Ppx_yojson_conv_lib.( ! ) filterText_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               filterText_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "insertText" -> (
             match Ppx_yojson_conv_lib.( ! ) insertText_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               insertText_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "insertTextFormat" -> (
             match Ppx_yojson_conv_lib.( ! ) insertTextFormat_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson InsertTextFormat.t_of_yojson
                   _field_yojson
               in
               insertTextFormat_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "textEdit" -> (
             match Ppx_yojson_conv_lib.( ! ) textEdit_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson TextEdit.t_of_yojson
                   _field_yojson
               in
               textEdit_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "additionalTextEdits" -> (
             match Ppx_yojson_conv_lib.( ! ) additionalTextEdits_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson TextEdit.t_of_yojson)
                   _field_yojson
               in
               additionalTextEdits_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "commitCharacters" -> (
             match Ppx_yojson_conv_lib.( ! ) commitCharacters_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson string_of_yojson)
                   _field_yojson
               in
               commitCharacters_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "command" -> (
             match Ppx_yojson_conv_lib.( ! ) command_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson Command.t_of_yojson
                   _field_yojson
               in
               command_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "data" -> (
             match Ppx_yojson_conv_lib.( ! ) data_field with
             | None ->
               let fvalue = Json.t_of_yojson _field_yojson in
               data_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) label_field
             , Ppx_yojson_conv_lib.( ! ) kind_field
             , Ppx_yojson_conv_lib.( ! ) tags_field
             , Ppx_yojson_conv_lib.( ! ) detail_field
             , Ppx_yojson_conv_lib.( ! ) documentation_field
             , Ppx_yojson_conv_lib.( ! ) deprecated_field
             , Ppx_yojson_conv_lib.( ! ) preselect_field
             , Ppx_yojson_conv_lib.( ! ) sortText_field
             , Ppx_yojson_conv_lib.( ! ) filterText_field
             , Ppx_yojson_conv_lib.( ! ) insertText_field
             , Ppx_yojson_conv_lib.( ! ) insertTextFormat_field
             , Ppx_yojson_conv_lib.( ! ) textEdit_field
             , Ppx_yojson_conv_lib.( ! ) additionalTextEdits_field
             , Ppx_yojson_conv_lib.( ! ) commitCharacters_field
             , Ppx_yojson_conv_lib.( ! ) command_field
             , Ppx_yojson_conv_lib.( ! ) data_field )
           with
           | ( Some label_value
             , kind_value
             , tags_value
             , detail_value
             , documentation_value
             , deprecated_value
             , preselect_value
             , sortText_value
             , filterText_value
             , insertText_value
             , insertTextFormat_value
             , textEdit_value
             , additionalTextEdits_value
             , commitCharacters_value
             , command_value
             , data_value ) ->
             { label = label_value
             ; kind =
                 (match kind_value with
                 | None -> None
                 | Some v -> v)
             ; tags =
                 (match tags_value with
                 | None -> None
                 | Some v -> v)
             ; detail =
                 (match detail_value with
                 | None -> None
                 | Some v -> v)
             ; documentation =
                 (match documentation_value with
                 | None -> None
                 | Some v -> v)
             ; deprecated =
                 (match deprecated_value with
                 | None -> None
                 | Some v -> v)
             ; preselect =
                 (match preselect_value with
                 | None -> None
                 | Some v -> v)
             ; sortText =
                 (match sortText_value with
                 | None -> None
                 | Some v -> v)
             ; filterText =
                 (match filterText_value with
                 | None -> None
                 | Some v -> v)
             ; insertText =
                 (match insertText_value with
                 | None -> None
                 | Some v -> v)
             ; insertTextFormat =
                 (match insertTextFormat_value with
                 | None -> None
                 | Some v -> v)
             ; textEdit =
                 (match textEdit_value with
                 | None -> None
                 | Some v -> v)
             ; additionalTextEdits =
                 (match additionalTextEdits_value with
                 | None -> None
                 | Some v -> v)
             ; commitCharacters =
                 (match commitCharacters_value with
                 | None -> None
                 | Some v -> v)
             ; command =
                 (match command_value with
                 | None -> None
                 | Some v -> v)
             ; data = data_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) label_field)
                     None
                 , "label" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { label = v_label
       ; kind = v_kind
       ; tags = v_tags
       ; detail = v_detail
       ; documentation = v_documentation
       ; deprecated = v_deprecated
       ; preselect = v_preselect
       ; sortText = v_sortText
       ; filterText = v_filterText
       ; insertText = v_insertText
       ; insertTextFormat = v_insertTextFormat
       ; textEdit = v_textEdit
       ; additionalTextEdits = v_additionalTextEdits
       ; commitCharacters = v_commitCharacters
       ; command = v_command
       ; data = v_data
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         match v_data with
         | None -> bnds
         | Some v ->
           let arg = Json.yojson_of_t v in
           let bnd = ("data", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_command then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t Command.yojson_of_t) v_command
           in
           let bnd = ("command", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_commitCharacters then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list yojson_of_string))
               v_commitCharacters
           in
           let bnd = ("commitCharacters", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_additionalTextEdits then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list TextEdit.yojson_of_t))
               v_additionalTextEdits
           in
           let bnd = ("additionalTextEdits", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_textEdit then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t TextEdit.yojson_of_t) v_textEdit
           in
           let bnd = ("textEdit", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_insertTextFormat then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t InsertTextFormat.yojson_of_t)
               v_insertTextFormat
           in
           let bnd = ("insertTextFormat", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_insertText then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_insertText
           in
           let bnd = ("insertText", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_filterText then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_filterText
           in
           let bnd = ("filterText", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_sortText then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_sortText
           in
           let bnd = ("sortText", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_preselect then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_preselect
           in
           let bnd = ("preselect", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_deprecated then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_deprecated
           in
           let bnd = ("deprecated", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentation then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_documentation)
               v_documentation
           in
           let bnd = ("documentation", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_detail then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_detail
           in
           let bnd = ("detail", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_tags then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list CompletionItemTag.yojson_of_t))
               v_tags
           in
           let bnd = ("tags", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_kind then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t CompletionItemKind.yojson_of_t)
               v_kind
           in
           let bnd = ("kind", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_label in
         ("label", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(label : string) ?(kind : CompletionItemKind.t option)
      ?(tags : CompletionItemTag.t list option) ?(detail : string option)
      ?(documentation : documentation option) ?(deprecated : bool option)
      ?(preselect : bool option) ?(sortText : string option)
      ?(filterText : string option) ?(insertText : string option)
      ?(insertTextFormat : InsertTextFormat.t option)
      ?(textEdit : TextEdit.t option)
      ?(additionalTextEdits : TextEdit.t list option)
      ?(commitCharacters : string list option) ?(command : Command.t option)
      ?(data : Json.t option) (() : unit) : t =
    { label
    ; kind
    ; tags
    ; detail
    ; documentation
    ; deprecated
    ; preselect
    ; sortText
    ; filterText
    ; insertText
    ; insertTextFormat
    ; textEdit
    ; additionalTextEdits
    ; commitCharacters
    ; command
    ; data
    }
end

module CompletionList = struct
  type t =
    { isIncomplete : bool
    ; items : CompletionItem.t list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CompletionList.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let isIncomplete_field = ref None
       and items_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "isIncomplete" -> (
             match Ppx_yojson_conv_lib.( ! ) isIncomplete_field with
             | None ->
               let fvalue = bool_of_yojson _field_yojson in
               isIncomplete_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "items" -> (
             match Ppx_yojson_conv_lib.( ! ) items_field with
             | None ->
               let fvalue =
                 list_of_yojson CompletionItem.t_of_yojson _field_yojson
               in
               items_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) isIncomplete_field
             , Ppx_yojson_conv_lib.( ! ) items_field )
           with
           | Some isIncomplete_value, Some items_value ->
             { isIncomplete = isIncomplete_value; items = items_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) isIncomplete_field)
                     None
                 , "isIncomplete" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) items_field)
                     None
                 , "items" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { isIncomplete = v_isIncomplete; items = v_items } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list CompletionItem.yojson_of_t v_items in
         ("items", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_bool v_isIncomplete in
         ("isIncomplete", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(isIncomplete : bool) ~(items : CompletionItem.t list) : t =
    { isIncomplete; items }
end

module CompletionOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; triggerCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; allCommitCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CompletionOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and triggerCharacters_field = ref None
       and allCommitCharacters_field = ref None
       and resolveProvider_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "triggerCharacters" -> (
             match Ppx_yojson_conv_lib.( ! ) triggerCharacters_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson string_of_yojson)
                   _field_yojson
               in
               triggerCharacters_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "allCommitCharacters" -> (
             match Ppx_yojson_conv_lib.( ! ) allCommitCharacters_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson string_of_yojson)
                   _field_yojson
               in
               allCommitCharacters_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "resolveProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) resolveProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               resolveProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( workDoneProgress_value
               , triggerCharacters_value
               , allCommitCharacters_value
               , resolveProvider_value ) =
             ( Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) triggerCharacters_field
             , Ppx_yojson_conv_lib.( ! ) allCommitCharacters_field
             , Ppx_yojson_conv_lib.( ! ) resolveProvider_field )
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; triggerCharacters =
               (match triggerCharacters_value with
               | None -> None
               | Some v -> v)
           ; allCommitCharacters =
               (match allCommitCharacters_value with
               | None -> None
               | Some v -> v)
           ; resolveProvider =
               (match resolveProvider_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress
       ; triggerCharacters = v_triggerCharacters
       ; allCommitCharacters = v_allCommitCharacters
       ; resolveProvider = v_resolveProvider
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_resolveProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_resolveProvider
           in
           let bnd = ("resolveProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_allCommitCharacters then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list yojson_of_string))
               v_allCommitCharacters
           in
           let bnd = ("allCommitCharacters", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_triggerCharacters then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list yojson_of_string))
               v_triggerCharacters
           in
           let bnd = ("triggerCharacters", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option)
      ?(triggerCharacters : string list option)
      ?(allCommitCharacters : string list option)
      ?(resolveProvider : bool option) (() : unit) : t =
    { workDoneProgress
    ; triggerCharacters
    ; allCommitCharacters
    ; resolveProvider
    }
end

module TextDocumentPositionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TextDocumentPositionParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and position_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "position" -> (
             match Ppx_yojson_conv_lib.( ! ) position_field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               position_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) position_field )
           with
           | Some textDocument_value, Some position_value ->
             { textDocument = textDocument_value; position = position_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) position_field)
                     None
                 , "position" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; position = v_position } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Position.yojson_of_t v_position in
         ("position", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(position : Position.t)
      : t =
    { textDocument; position }
end

module CompletionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; context : CompletionContext.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CompletionParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and position_field = ref None
       and context_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "position" -> (
             match Ppx_yojson_conv_lib.( ! ) position_field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               position_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "context" -> (
             match Ppx_yojson_conv_lib.( ! ) context_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson CompletionContext.t_of_yojson
                   _field_yojson
               in
               context_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) position_field
             , Ppx_yojson_conv_lib.( ! ) context_field )
           with
           | Some textDocument_value, Some position_value, context_value ->
             { textDocument = textDocument_value
             ; position = position_value
             ; context =
                 (match context_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) position_field)
                     None
                 , "position" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument
       ; position = v_position
       ; context = v_context
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_context then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t CompletionContext.yojson_of_t)
               v_context
           in
           let bnd = ("context", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = Position.yojson_of_t v_position in
         ("position", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(position : Position.t)
      ?(context : CompletionContext.t option) (() : unit) : t =
    { textDocument; position; context }
end

module CompletionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; triggerCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; allCommitCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.CompletionRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and triggerCharacters_field = ref None
       and allCommitCharacters_field = ref None
       and resolveProvider_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "triggerCharacters" -> (
             match Ppx_yojson_conv_lib.( ! ) triggerCharacters_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson string_of_yojson)
                   _field_yojson
               in
               triggerCharacters_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "allCommitCharacters" -> (
             match Ppx_yojson_conv_lib.( ! ) allCommitCharacters_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson string_of_yojson)
                   _field_yojson
               in
               allCommitCharacters_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "resolveProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) resolveProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               resolveProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( documentSelector_value
               , workDoneProgress_value
               , triggerCharacters_value
               , allCommitCharacters_value
               , resolveProvider_value ) =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) triggerCharacters_field
             , Ppx_yojson_conv_lib.( ! ) allCommitCharacters_field
             , Ppx_yojson_conv_lib.( ! ) resolveProvider_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; triggerCharacters =
               (match triggerCharacters_value with
               | None -> None
               | Some v -> v)
           ; allCommitCharacters =
               (match allCommitCharacters_value with
               | None -> None
               | Some v -> v)
           ; resolveProvider =
               (match resolveProvider_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       ; triggerCharacters = v_triggerCharacters
       ; allCommitCharacters = v_allCommitCharacters
       ; resolveProvider = v_resolveProvider
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_resolveProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_resolveProvider
           in
           let bnd = ("resolveProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_allCommitCharacters then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list yojson_of_string))
               v_allCommitCharacters
           in
           let bnd = ("allCommitCharacters", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_triggerCharacters then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list yojson_of_string))
               v_triggerCharacters
           in
           let bnd = ("triggerCharacters", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option)
      ?(triggerCharacters : string list option)
      ?(allCommitCharacters : string list option)
      ?(resolveProvider : bool option) (() : unit) : t =
    { documentSelector
    ; workDoneProgress
    ; triggerCharacters
    ; allCommitCharacters
    ; resolveProvider
    }
end

module ConfigurationItem = struct
  type t =
    { scopeUri : DocumentUri.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; section : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ConfigurationItem.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let scopeUri_field = ref None
       and section_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "scopeUri" -> (
             match Ppx_yojson_conv_lib.( ! ) scopeUri_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentUri.t_of_yojson
                   _field_yojson
               in
               scopeUri_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "section" -> (
             match Ppx_yojson_conv_lib.( ! ) section_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               section_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let scopeUri_value, section_value =
             ( Ppx_yojson_conv_lib.( ! ) scopeUri_field
             , Ppx_yojson_conv_lib.( ! ) section_field )
           in
           { scopeUri =
               (match scopeUri_value with
               | None -> None
               | Some v -> v)
           ; section =
               (match section_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { scopeUri = v_scopeUri; section = v_section } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_section then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_section
           in
           let bnd = ("section", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_scopeUri then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentUri.yojson_of_t)
               v_scopeUri
           in
           let bnd = ("scopeUri", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(scopeUri : DocumentUri.t option) ?(section : string option)
      (() : unit) : t =
    { scopeUri; section }
end

module ConfigurationParams = struct
  type t = { items : ConfigurationItem.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ConfigurationParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let items_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "items" -> (
             match Ppx_yojson_conv_lib.( ! ) items_field with
             | None ->
               let fvalue =
                 list_of_yojson ConfigurationItem.t_of_yojson _field_yojson
               in
               items_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) items_field with
           | Some items_value -> { items = items_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) items_field)
                     None
                 , "items" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { items = v_items } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list ConfigurationItem.yojson_of_t v_items in
         ("items", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(items : ConfigurationItem.t list) : t = { items }
end

module DeclarationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DeclarationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module DeclarationParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DeclarationParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and position_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "position" -> (
             match Ppx_yojson_conv_lib.( ! ) position_field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               position_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) position_field )
           with
           | Some textDocument_value, Some position_value ->
             { textDocument = textDocument_value; position = position_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) position_field)
                     None
                 , "position" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; position = v_position } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Position.yojson_of_t v_position in
         ("position", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(position : Position.t)
      : t =
    { textDocument; position }
end

module StaticRegistrationOptions = struct
  type t =
    { id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.StaticRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let id_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "id" -> (
             match Ppx_yojson_conv_lib.( ! ) id_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               id_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let id_value = Ppx_yojson_conv_lib.( ! ) id_field in
           { id =
               (match id_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { id = v_id } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_id then
           bnds
         else
           let arg = (Json.Nullable_option.yojson_of_t yojson_of_string) v_id in
           let bnd = ("id", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(id : string option) (() : unit) : t = { id }
end

module DeclarationRegistrationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DeclarationRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and documentSelector_field = ref None
       and id_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "id" -> (
             match Ppx_yojson_conv_lib.( ! ) id_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               id_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value, documentSelector_value, id_value =
             ( Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) id_field )
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; id =
               (match id_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress
       ; documentSelector = v_documentSelector
       ; id = v_id
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_id then
           bnds
         else
           let arg = (Json.Nullable_option.yojson_of_t yojson_of_string) v_id in
           let bnd = ("id", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option)
      ?(documentSelector : DocumentSelector.t option) ?(id : string option)
      (() : unit) : t =
    { workDoneProgress; documentSelector; id }
end

module DefinitionOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DefinitionOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module DefinitionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DefinitionParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and position_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "position" -> (
             match Ppx_yojson_conv_lib.( ! ) position_field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               position_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) position_field )
           with
           | Some textDocument_value, Some position_value ->
             { textDocument = textDocument_value; position = position_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) position_field)
                     None
                 , "position" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; position = v_position } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Position.yojson_of_t v_position in
         ("position", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(position : Position.t)
      : t =
    { textDocument; position }
end

module DefinitionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DefinitionRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let documentSelector_value, workDoneProgress_value =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option) (() : unit) : t =
    { documentSelector; workDoneProgress }
end

module DidChangeConfigurationParams = struct
  type t = { settings : Json.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DidChangeConfigurationParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let settings_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "settings" -> (
             match Ppx_yojson_conv_lib.( ! ) settings_field with
             | None ->
               let fvalue = Json.t_of_yojson _field_yojson in
               settings_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) settings_field with
           | Some settings_value -> { settings = settings_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) settings_field)
                     None
                 , "settings" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { settings = v_settings } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Json.yojson_of_t v_settings in
         ("settings", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(settings : Json.t) : t = { settings }
end

module TextDocumentContentChangeEvent = struct
  type t =
    { range : Range.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; rangeLength : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; text : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TextDocumentContentChangeEvent.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let range_field = ref None
       and rangeLength_field = ref None
       and text_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "range" -> (
             match Ppx_yojson_conv_lib.( ! ) range_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson Range.t_of_yojson
                   _field_yojson
               in
               range_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "rangeLength" -> (
             match Ppx_yojson_conv_lib.( ! ) rangeLength_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson int_of_yojson _field_yojson
               in
               rangeLength_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "text" -> (
             match Ppx_yojson_conv_lib.( ! ) text_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               text_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) range_field
             , Ppx_yojson_conv_lib.( ! ) rangeLength_field
             , Ppx_yojson_conv_lib.( ! ) text_field )
           with
           | range_value, rangeLength_value, Some text_value ->
             { range =
                 (match range_value with
                 | None -> None
                 | Some v -> v)
             ; rangeLength =
                 (match rangeLength_value with
                 | None -> None
                 | Some v -> v)
             ; text = text_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) text_field)
                     None
                 , "text" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { range = v_range; rangeLength = v_rangeLength; text = v_text } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_text in
         ("text", arg) :: bnds
       in
       let bnds =
         if None = v_rangeLength then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_int) v_rangeLength
           in
           let bnd = ("rangeLength", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_range then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t Range.yojson_of_t) v_range
           in
           let bnd = ("range", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(range : Range.t option) ?(rangeLength : int option)
      ~(text : string) (() : unit) : t =
    { range; rangeLength; text }
end

module DidChangeTextDocumentParams = struct
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; contentChanges : TextDocumentContentChangeEvent.t list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DidChangeTextDocumentParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and contentChanges_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue =
                 VersionedTextDocumentIdentifier.t_of_yojson _field_yojson
               in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "contentChanges" -> (
             match Ppx_yojson_conv_lib.( ! ) contentChanges_field with
             | None ->
               let fvalue =
                 list_of_yojson TextDocumentContentChangeEvent.t_of_yojson
                   _field_yojson
               in
               contentChanges_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) contentChanges_field )
           with
           | Some textDocument_value, Some contentChanges_value ->
             { textDocument = textDocument_value
             ; contentChanges = contentChanges_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) contentChanges_field)
                     None
                 , "contentChanges" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; contentChanges = v_contentChanges } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg =
           yojson_of_list TextDocumentContentChangeEvent.yojson_of_t
             v_contentChanges
         in
         ("contentChanges", arg) :: bnds
       in
       let bnds =
         let arg = VersionedTextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : VersionedTextDocumentIdentifier.t)
      ~(contentChanges : TextDocumentContentChangeEvent.t list) : t =
    { textDocument; contentChanges }
end

module FileEvent = struct
  type t =
    { uri : DocumentUri.t
    ; type_ : int [@key "type"]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.FileEvent.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let uri_field = ref None
       and type__field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "uri" -> (
             match Ppx_yojson_conv_lib.( ! ) uri_field with
             | None ->
               let fvalue = DocumentUri.t_of_yojson _field_yojson in
               uri_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "type" -> (
             match Ppx_yojson_conv_lib.( ! ) type__field with
             | None ->
               let fvalue = int_of_yojson _field_yojson in
               type__field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) uri_field
             , Ppx_yojson_conv_lib.( ! ) type__field )
           with
           | Some uri_value, Some type__value ->
             { uri = uri_value; type_ = type__value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) uri_field)
                     None
                 , "uri" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) type__field)
                     None
                 , "type_" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { uri = v_uri; type_ = v_type_ } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_int v_type_ in
         ("type", arg) :: bnds
       in
       let bnds =
         let arg = DocumentUri.yojson_of_t v_uri in
         ("uri", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(uri : DocumentUri.t) ~(type_ : int) : t = { uri; type_ }
end

module DidChangeWatchedFilesParams = struct
  type t = { changes : FileEvent.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DidChangeWatchedFilesParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let changes_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "changes" -> (
             match Ppx_yojson_conv_lib.( ! ) changes_field with
             | None ->
               let fvalue =
                 list_of_yojson FileEvent.t_of_yojson _field_yojson
               in
               changes_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) changes_field with
           | Some changes_value -> { changes = changes_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) changes_field)
                     None
                 , "changes" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { changes = v_changes } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list FileEvent.yojson_of_t v_changes in
         ("changes", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(changes : FileEvent.t list) : t = { changes }
end

module FileSystemWatcher = struct
  type t =
    { globPattern : string
    ; kind : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.FileSystemWatcher.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let globPattern_field = ref None
       and kind_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "globPattern" -> (
             match Ppx_yojson_conv_lib.( ! ) globPattern_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               globPattern_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "kind" -> (
             match Ppx_yojson_conv_lib.( ! ) kind_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson int_of_yojson _field_yojson
               in
               kind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) globPattern_field
             , Ppx_yojson_conv_lib.( ! ) kind_field )
           with
           | Some globPattern_value, kind_value ->
             { globPattern = globPattern_value
             ; kind =
                 (match kind_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) globPattern_field)
                     None
                 , "globPattern" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { globPattern = v_globPattern; kind = v_kind } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_kind then
           bnds
         else
           let arg = (Json.Nullable_option.yojson_of_t yojson_of_int) v_kind in
           let bnd = ("kind", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_globPattern in
         ("globPattern", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(globPattern : string) ?(kind : int option) (() : unit) : t =
    { globPattern; kind }
end

module DidChangeWatchedFilesRegistrationOptions = struct
  type t = { watchers : FileSystemWatcher.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.DidChangeWatchedFilesRegistrationOptions.t"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let watchers_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "watchers" -> (
             match Ppx_yojson_conv_lib.( ! ) watchers_field with
             | None ->
               let fvalue =
                 list_of_yojson FileSystemWatcher.t_of_yojson _field_yojson
               in
               watchers_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) watchers_field with
           | Some watchers_value -> { watchers = watchers_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) watchers_field)
                     None
                 , "watchers" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { watchers = v_watchers } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list FileSystemWatcher.yojson_of_t v_watchers in
         ("watchers", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(watchers : FileSystemWatcher.t list) : t = { watchers }
end

module WorkspaceFolder = struct
  type t =
    { uri : DocumentUri.t
    ; name : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkspaceFolder.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let uri_field = ref None
       and name_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "uri" -> (
             match Ppx_yojson_conv_lib.( ! ) uri_field with
             | None ->
               let fvalue = DocumentUri.t_of_yojson _field_yojson in
               uri_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "name" -> (
             match Ppx_yojson_conv_lib.( ! ) name_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               name_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) uri_field
             , Ppx_yojson_conv_lib.( ! ) name_field )
           with
           | Some uri_value, Some name_value ->
             { uri = uri_value; name = name_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) uri_field)
                     None
                 , "uri" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) name_field)
                     None
                 , "name" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { uri = v_uri; name = v_name } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_name in
         ("name", arg) :: bnds
       in
       let bnds =
         let arg = DocumentUri.yojson_of_t v_uri in
         ("uri", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(uri : DocumentUri.t) ~(name : string) : t = { uri; name }
end

module WorkspaceFoldersChangeEvent = struct
  type t =
    { added : WorkspaceFolder.t list
    ; removed : WorkspaceFolder.t list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkspaceFoldersChangeEvent.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let added_field = ref None
       and removed_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "added" -> (
             match Ppx_yojson_conv_lib.( ! ) added_field with
             | None ->
               let fvalue =
                 list_of_yojson WorkspaceFolder.t_of_yojson _field_yojson
               in
               added_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "removed" -> (
             match Ppx_yojson_conv_lib.( ! ) removed_field with
             | None ->
               let fvalue =
                 list_of_yojson WorkspaceFolder.t_of_yojson _field_yojson
               in
               removed_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) added_field
             , Ppx_yojson_conv_lib.( ! ) removed_field )
           with
           | Some added_value, Some removed_value ->
             { added = added_value; removed = removed_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) added_field)
                     None
                 , "added" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) removed_field)
                     None
                 , "removed" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { added = v_added; removed = v_removed } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list WorkspaceFolder.yojson_of_t v_removed in
         ("removed", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_list WorkspaceFolder.yojson_of_t v_added in
         ("added", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(added : WorkspaceFolder.t list)
      ~(removed : WorkspaceFolder.t list) : t =
    { added; removed }
end

module DidChangeWorkspaceFoldersParams = struct
  type t = { event : WorkspaceFoldersChangeEvent.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DidChangeWorkspaceFoldersParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let event_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "event" -> (
             match Ppx_yojson_conv_lib.( ! ) event_field with
             | None ->
               let fvalue =
                 WorkspaceFoldersChangeEvent.t_of_yojson _field_yojson
               in
               event_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) event_field with
           | Some event_value -> { event = event_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) event_field)
                     None
                 , "event" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { event = v_event } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = WorkspaceFoldersChangeEvent.yojson_of_t v_event in
         ("event", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(event : WorkspaceFoldersChangeEvent.t) : t = { event }
end

module DidCloseTextDocumentParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DidCloseTextDocumentParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) textDocument_field with
           | Some textDocument_value -> { textDocument = textDocument_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) : t = { textDocument }
end

module TextDocumentItem = struct
  type t =
    { uri : DocumentUri.t
    ; languageId : string
    ; version : int
    ; text : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TextDocumentItem.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let uri_field = ref None
       and languageId_field = ref None
       and version_field = ref None
       and text_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "uri" -> (
             match Ppx_yojson_conv_lib.( ! ) uri_field with
             | None ->
               let fvalue = DocumentUri.t_of_yojson _field_yojson in
               uri_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "languageId" -> (
             match Ppx_yojson_conv_lib.( ! ) languageId_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               languageId_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "version" -> (
             match Ppx_yojson_conv_lib.( ! ) version_field with
             | None ->
               let fvalue = int_of_yojson _field_yojson in
               version_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "text" -> (
             match Ppx_yojson_conv_lib.( ! ) text_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               text_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) uri_field
             , Ppx_yojson_conv_lib.( ! ) languageId_field
             , Ppx_yojson_conv_lib.( ! ) version_field
             , Ppx_yojson_conv_lib.( ! ) text_field )
           with
           | ( Some uri_value
             , Some languageId_value
             , Some version_value
             , Some text_value ) ->
             { uri = uri_value
             ; languageId = languageId_value
             ; version = version_value
             ; text = text_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) uri_field)
                     None
                 , "uri" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) languageId_field)
                     None
                 , "languageId" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) version_field)
                     None
                 , "version" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) text_field)
                     None
                 , "text" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { uri = v_uri
       ; languageId = v_languageId
       ; version = v_version
       ; text = v_text
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_text in
         ("text", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_int v_version in
         ("version", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_languageId in
         ("languageId", arg) :: bnds
       in
       let bnds =
         let arg = DocumentUri.yojson_of_t v_uri in
         ("uri", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(uri : DocumentUri.t) ~(languageId : string) ~(version : int)
      ~(text : string) : t =
    { uri; languageId; version; text }
end

module DidOpenTextDocumentParams = struct
  type t = { textDocument : TextDocumentItem.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DidOpenTextDocumentParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentItem.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) textDocument_field with
           | Some textDocument_value -> { textDocument = textDocument_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = TextDocumentItem.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentItem.t) : t = { textDocument }
end

module DidSaveTextDocumentParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; text : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DidSaveTextDocumentParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and text_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "text" -> (
             match Ppx_yojson_conv_lib.( ! ) text_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               text_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) text_field )
           with
           | Some textDocument_value, text_value ->
             { textDocument = textDocument_value
             ; text =
                 (match text_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; text = v_text } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_text then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_text
           in
           let bnd = ("text", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ?(text : string option)
      (() : unit) : t =
    { textDocument; text }
end

module DocumentColorOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentColorOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module DocumentColorParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentColorParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) textDocument_field with
           | Some textDocument_value -> { textDocument = textDocument_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) : t = { textDocument }
end

module DocumentColorRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentColorRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and id_field = ref None
       and workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "id" -> (
             match Ppx_yojson_conv_lib.( ! ) id_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               id_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let documentSelector_value, id_value, workDoneProgress_value =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) id_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; id =
               (match id_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; id = v_id
       ; workDoneProgress = v_workDoneProgress
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_id then
           bnds
         else
           let arg = (Json.Nullable_option.yojson_of_t yojson_of_string) v_id in
           let bnd = ("id", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(id : string option) ?(workDoneProgress : bool option) (() : unit) : t =
    { documentSelector; id; workDoneProgress }
end

module DocumentFormattingOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentFormattingOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module FormattingOptions = struct
  type t =
    { tabSize : int
    ; insertSpaces : bool
    ; trimTrailingWhitespace : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; insertFinalNewline : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; trimFinalNewlines : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.FormattingOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let tabSize_field = ref None
       and insertSpaces_field = ref None
       and trimTrailingWhitespace_field = ref None
       and insertFinalNewline_field = ref None
       and trimFinalNewlines_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "tabSize" -> (
             match Ppx_yojson_conv_lib.( ! ) tabSize_field with
             | None ->
               let fvalue = int_of_yojson _field_yojson in
               tabSize_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "insertSpaces" -> (
             match Ppx_yojson_conv_lib.( ! ) insertSpaces_field with
             | None ->
               let fvalue = bool_of_yojson _field_yojson in
               insertSpaces_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "trimTrailingWhitespace" -> (
             match Ppx_yojson_conv_lib.( ! ) trimTrailingWhitespace_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               trimTrailingWhitespace_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "insertFinalNewline" -> (
             match Ppx_yojson_conv_lib.( ! ) insertFinalNewline_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               insertFinalNewline_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "trimFinalNewlines" -> (
             match Ppx_yojson_conv_lib.( ! ) trimFinalNewlines_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               trimFinalNewlines_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) tabSize_field
             , Ppx_yojson_conv_lib.( ! ) insertSpaces_field
             , Ppx_yojson_conv_lib.( ! ) trimTrailingWhitespace_field
             , Ppx_yojson_conv_lib.( ! ) insertFinalNewline_field
             , Ppx_yojson_conv_lib.( ! ) trimFinalNewlines_field )
           with
           | ( Some tabSize_value
             , Some insertSpaces_value
             , trimTrailingWhitespace_value
             , insertFinalNewline_value
             , trimFinalNewlines_value ) ->
             { tabSize = tabSize_value
             ; insertSpaces = insertSpaces_value
             ; trimTrailingWhitespace =
                 (match trimTrailingWhitespace_value with
                 | None -> None
                 | Some v -> v)
             ; insertFinalNewline =
                 (match insertFinalNewline_value with
                 | None -> None
                 | Some v -> v)
             ; trimFinalNewlines =
                 (match trimFinalNewlines_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) tabSize_field)
                     None
                 , "tabSize" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) insertSpaces_field)
                     None
                 , "insertSpaces" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { tabSize = v_tabSize
       ; insertSpaces = v_insertSpaces
       ; trimTrailingWhitespace = v_trimTrailingWhitespace
       ; insertFinalNewline = v_insertFinalNewline
       ; trimFinalNewlines = v_trimFinalNewlines
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_trimFinalNewlines then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_trimFinalNewlines
           in
           let bnd = ("trimFinalNewlines", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_insertFinalNewline then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_insertFinalNewline
           in
           let bnd = ("insertFinalNewline", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_trimTrailingWhitespace then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_trimTrailingWhitespace
           in
           let bnd = ("trimTrailingWhitespace", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_bool v_insertSpaces in
         ("insertSpaces", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_int v_tabSize in
         ("tabSize", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(tabSize : int) ~(insertSpaces : bool)
      ?(trimTrailingWhitespace : bool option)
      ?(insertFinalNewline : bool option) ?(trimFinalNewlines : bool option)
      (() : unit) : t =
    { tabSize
    ; insertSpaces
    ; trimTrailingWhitespace
    ; insertFinalNewline
    ; trimFinalNewlines
    }
end

module DocumentFormattingParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; options : FormattingOptions.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentFormattingParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and options_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "options" -> (
             match Ppx_yojson_conv_lib.( ! ) options_field with
             | None ->
               let fvalue = FormattingOptions.t_of_yojson _field_yojson in
               options_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) options_field )
           with
           | Some textDocument_value, Some options_value ->
             { textDocument = textDocument_value; options = options_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) options_field)
                     None
                 , "options" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; options = v_options } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = FormattingOptions.yojson_of_t v_options in
         ("options", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t)
      ~(options : FormattingOptions.t) : t =
    { textDocument; options }
end

module DocumentFormattingRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentFormattingRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let documentSelector_value, workDoneProgress_value =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option) (() : unit) : t =
    { documentSelector; workDoneProgress }
end

module DocumentHighlightKind = struct
  type t =
    | Text
    | Read
    | Write

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Text -> `Int 1
    | Read -> `Int 2
    | Write -> `Int 3

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Text
    | `Int 2 -> Read
    | `Int 3 -> Write
    | _ -> Json.error "t" json
end

module DocumentHighlight = struct
  type t =
    { range : Range.t
    ; kind : DocumentHighlightKind.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentHighlight.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let range_field = ref None
       and kind_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "range" -> (
             match Ppx_yojson_conv_lib.( ! ) range_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               range_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "kind" -> (
             match Ppx_yojson_conv_lib.( ! ) kind_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DocumentHighlightKind.t_of_yojson _field_yojson
               in
               kind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) range_field
             , Ppx_yojson_conv_lib.( ! ) kind_field )
           with
           | Some range_value, kind_value ->
             { range = range_value
             ; kind =
                 (match kind_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) range_field)
                     None
                 , "range" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { range = v_range; kind = v_kind } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_kind then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentHighlightKind.yojson_of_t)
               v_kind
           in
           let bnd = ("kind", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = Range.yojson_of_t v_range in
         ("range", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(range : Range.t) ?(kind : DocumentHighlightKind.t option)
      (() : unit) : t =
    { range; kind }
end

module DocumentHighlightOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentHighlightOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module DocumentHighlightParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentHighlightParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and position_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "position" -> (
             match Ppx_yojson_conv_lib.( ! ) position_field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               position_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) position_field )
           with
           | Some textDocument_value, Some position_value ->
             { textDocument = textDocument_value; position = position_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) position_field)
                     None
                 , "position" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; position = v_position } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Position.yojson_of_t v_position in
         ("position", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(position : Position.t)
      : t =
    { textDocument; position }
end

module DocumentHighlightRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentHighlightRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let documentSelector_value, workDoneProgress_value =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option) (() : unit) : t =
    { documentSelector; workDoneProgress }
end

module DocumentLink = struct
  type t =
    { range : Range.t
    ; target : DocumentUri.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; tooltip : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; data : Json.t option [@yojson.option]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentLink.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let range_field = ref None
       and target_field = ref None
       and tooltip_field = ref None
       and data_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "range" -> (
             match Ppx_yojson_conv_lib.( ! ) range_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               range_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "target" -> (
             match Ppx_yojson_conv_lib.( ! ) target_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentUri.t_of_yojson
                   _field_yojson
               in
               target_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "tooltip" -> (
             match Ppx_yojson_conv_lib.( ! ) tooltip_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               tooltip_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "data" -> (
             match Ppx_yojson_conv_lib.( ! ) data_field with
             | None ->
               let fvalue = Json.t_of_yojson _field_yojson in
               data_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) range_field
             , Ppx_yojson_conv_lib.( ! ) target_field
             , Ppx_yojson_conv_lib.( ! ) tooltip_field
             , Ppx_yojson_conv_lib.( ! ) data_field )
           with
           | Some range_value, target_value, tooltip_value, data_value ->
             { range = range_value
             ; target =
                 (match target_value with
                 | None -> None
                 | Some v -> v)
             ; tooltip =
                 (match tooltip_value with
                 | None -> None
                 | Some v -> v)
             ; data = data_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) range_field)
                     None
                 , "range" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { range = v_range
       ; target = v_target
       ; tooltip = v_tooltip
       ; data = v_data
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         match v_data with
         | None -> bnds
         | Some v ->
           let arg = Json.yojson_of_t v in
           let bnd = ("data", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_tooltip then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_tooltip
           in
           let bnd = ("tooltip", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_target then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentUri.yojson_of_t) v_target
           in
           let bnd = ("target", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = Range.yojson_of_t v_range in
         ("range", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(range : Range.t) ?(target : DocumentUri.t option)
      ?(tooltip : string option) ?(data : Json.t option) (() : unit) : t =
    { range; target; tooltip; data }
end

module DocumentLinkOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentLinkOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and resolveProvider_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "resolveProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) resolveProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               resolveProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value, resolveProvider_value =
             ( Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) resolveProvider_field )
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; resolveProvider =
               (match resolveProvider_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress
       ; resolveProvider = v_resolveProvider
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_resolveProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_resolveProvider
           in
           let bnd = ("resolveProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) ?(resolveProvider : bool option)
      (() : unit) : t =
    { workDoneProgress; resolveProvider }
end

module DocumentLinkParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentLinkParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) textDocument_field with
           | Some textDocument_value -> { textDocument = textDocument_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) : t = { textDocument }
end

module DocumentLinkRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; resolveProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentLinkRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and resolveProvider_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "resolveProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) resolveProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               resolveProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( documentSelector_value
               , workDoneProgress_value
               , resolveProvider_value ) =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) resolveProvider_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; resolveProvider =
               (match resolveProvider_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       ; resolveProvider = v_resolveProvider
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_resolveProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_resolveProvider
           in
           let bnd = ("resolveProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option) ?(resolveProvider : bool option)
      (() : unit) : t =
    { documentSelector; workDoneProgress; resolveProvider }
end

module DocumentOnTypeFormattingOptions = struct
  type t =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentOnTypeFormattingOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let firstTriggerCharacter_field = ref None
       and moreTriggerCharacter_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "firstTriggerCharacter" -> (
             match Ppx_yojson_conv_lib.( ! ) firstTriggerCharacter_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               firstTriggerCharacter_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "moreTriggerCharacter" -> (
             match Ppx_yojson_conv_lib.( ! ) moreTriggerCharacter_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson string_of_yojson)
                   _field_yojson
               in
               moreTriggerCharacter_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) firstTriggerCharacter_field
             , Ppx_yojson_conv_lib.( ! ) moreTriggerCharacter_field )
           with
           | Some firstTriggerCharacter_value, moreTriggerCharacter_value ->
             { firstTriggerCharacter = firstTriggerCharacter_value
             ; moreTriggerCharacter =
                 (match moreTriggerCharacter_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) firstTriggerCharacter_field)
                     None
                 , "firstTriggerCharacter" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { firstTriggerCharacter = v_firstTriggerCharacter
       ; moreTriggerCharacter = v_moreTriggerCharacter
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_moreTriggerCharacter then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list yojson_of_string))
               v_moreTriggerCharacter
           in
           let bnd = ("moreTriggerCharacter", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_firstTriggerCharacter in
         ("firstTriggerCharacter", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(firstTriggerCharacter : string)
      ?(moreTriggerCharacter : string list option) (() : unit) : t =
    { firstTriggerCharacter; moreTriggerCharacter }
end

module DocumentOnTypeFormattingParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; ch : string
    ; options : FormattingOptions.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentOnTypeFormattingParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and position_field = ref None
       and ch_field = ref None
       and options_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "position" -> (
             match Ppx_yojson_conv_lib.( ! ) position_field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               position_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "ch" -> (
             match Ppx_yojson_conv_lib.( ! ) ch_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               ch_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "options" -> (
             match Ppx_yojson_conv_lib.( ! ) options_field with
             | None ->
               let fvalue = FormattingOptions.t_of_yojson _field_yojson in
               options_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) position_field
             , Ppx_yojson_conv_lib.( ! ) ch_field
             , Ppx_yojson_conv_lib.( ! ) options_field )
           with
           | ( Some textDocument_value
             , Some position_value
             , Some ch_value
             , Some options_value ) ->
             { textDocument = textDocument_value
             ; position = position_value
             ; ch = ch_value
             ; options = options_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) position_field)
                     None
                 , "position" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) ch_field)
                     None
                 , "ch" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) options_field)
                     None
                 , "options" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument
       ; position = v_position
       ; ch = v_ch
       ; options = v_options
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = FormattingOptions.yojson_of_t v_options in
         ("options", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_ch in
         ("ch", arg) :: bnds
       in
       let bnds =
         let arg = Position.yojson_of_t v_position in
         ("position", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(position : Position.t)
      ~(ch : string) ~(options : FormattingOptions.t) : t =
    { textDocument; position; ch; options }
end

module DocumentOnTypeFormattingRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; firstTriggerCharacter : string
    ; moreTriggerCharacter : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.DocumentOnTypeFormattingRegistrationOptions.t"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and firstTriggerCharacter_field = ref None
       and moreTriggerCharacter_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "firstTriggerCharacter" -> (
             match Ppx_yojson_conv_lib.( ! ) firstTriggerCharacter_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               firstTriggerCharacter_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "moreTriggerCharacter" -> (
             match Ppx_yojson_conv_lib.( ! ) moreTriggerCharacter_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson string_of_yojson)
                   _field_yojson
               in
               moreTriggerCharacter_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) firstTriggerCharacter_field
             , Ppx_yojson_conv_lib.( ! ) moreTriggerCharacter_field )
           with
           | ( documentSelector_value
             , Some firstTriggerCharacter_value
             , moreTriggerCharacter_value ) ->
             { documentSelector =
                 (match documentSelector_value with
                 | None -> None
                 | Some v -> v)
             ; firstTriggerCharacter = firstTriggerCharacter_value
             ; moreTriggerCharacter =
                 (match moreTriggerCharacter_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) firstTriggerCharacter_field)
                     None
                 , "firstTriggerCharacter" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; firstTriggerCharacter = v_firstTriggerCharacter
       ; moreTriggerCharacter = v_moreTriggerCharacter
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_moreTriggerCharacter then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list yojson_of_string))
               v_moreTriggerCharacter
           in
           let bnd = ("moreTriggerCharacter", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_firstTriggerCharacter in
         ("firstTriggerCharacter", arg) :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ~(firstTriggerCharacter : string)
      ?(moreTriggerCharacter : string list option) (() : unit) : t =
    { documentSelector; firstTriggerCharacter; moreTriggerCharacter }
end

module DocumentRangeFormattingOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentRangeFormattingOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module DocumentRangeFormattingParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; options : FormattingOptions.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentRangeFormattingParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and range_field = ref None
       and options_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "range" -> (
             match Ppx_yojson_conv_lib.( ! ) range_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               range_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "options" -> (
             match Ppx_yojson_conv_lib.( ! ) options_field with
             | None ->
               let fvalue = FormattingOptions.t_of_yojson _field_yojson in
               options_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) range_field
             , Ppx_yojson_conv_lib.( ! ) options_field )
           with
           | Some textDocument_value, Some range_value, Some options_value ->
             { textDocument = textDocument_value
             ; range = range_value
             ; options = options_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) range_field)
                     None
                 , "range" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) options_field)
                     None
                 , "options" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; range = v_range; options = v_options }
       ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = FormattingOptions.yojson_of_t v_options in
         ("options", arg) :: bnds
       in
       let bnds =
         let arg = Range.yojson_of_t v_range in
         ("range", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(range : Range.t)
      ~(options : FormattingOptions.t) : t =
    { textDocument; range; options }
end

module DocumentRangeFormattingRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc =
       "lsp/src/types.ml.DocumentRangeFormattingRegistrationOptions.t"
     in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let documentSelector_value, workDoneProgress_value =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option) (() : unit) : t =
    { documentSelector; workDoneProgress }
end

module DocumentSymbol = struct
  type t =
    { name : string
    ; detail : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; kind : SymbolKind.t
    ; deprecated : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; range : Range.t
    ; selectionRange : Range.t
    ; children : t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let rec t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentSymbol.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let name_field = ref None
       and detail_field = ref None
       and kind_field = ref None
       and deprecated_field = ref None
       and range_field = ref None
       and selectionRange_field = ref None
       and children_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "name" -> (
             match Ppx_yojson_conv_lib.( ! ) name_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               name_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "detail" -> (
             match Ppx_yojson_conv_lib.( ! ) detail_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               detail_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "kind" -> (
             match Ppx_yojson_conv_lib.( ! ) kind_field with
             | None ->
               let fvalue = SymbolKind.t_of_yojson _field_yojson in
               kind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "deprecated" -> (
             match Ppx_yojson_conv_lib.( ! ) deprecated_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               deprecated_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "range" -> (
             match Ppx_yojson_conv_lib.( ! ) range_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               range_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "selectionRange" -> (
             match Ppx_yojson_conv_lib.( ! ) selectionRange_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               selectionRange_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "children" -> (
             match Ppx_yojson_conv_lib.( ! ) children_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson t_of_yojson)
                   _field_yojson
               in
               children_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) name_field
             , Ppx_yojson_conv_lib.( ! ) detail_field
             , Ppx_yojson_conv_lib.( ! ) kind_field
             , Ppx_yojson_conv_lib.( ! ) deprecated_field
             , Ppx_yojson_conv_lib.( ! ) range_field
             , Ppx_yojson_conv_lib.( ! ) selectionRange_field
             , Ppx_yojson_conv_lib.( ! ) children_field )
           with
           | ( Some name_value
             , detail_value
             , Some kind_value
             , deprecated_value
             , Some range_value
             , Some selectionRange_value
             , children_value ) ->
             { name = name_value
             ; detail =
                 (match detail_value with
                 | None -> None
                 | Some v -> v)
             ; kind = kind_value
             ; deprecated =
                 (match deprecated_value with
                 | None -> None
                 | Some v -> v)
             ; range = range_value
             ; selectionRange = selectionRange_value
             ; children =
                 (match children_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) name_field)
                     None
                 , "name" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) kind_field)
                     None
                 , "kind" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) range_field)
                     None
                 , "range" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) selectionRange_field)
                     None
                 , "selectionRange" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let rec yojson_of_t =
    (function
     | { name = v_name
       ; detail = v_detail
       ; kind = v_kind
       ; deprecated = v_deprecated
       ; range = v_range
       ; selectionRange = v_selectionRange
       ; children = v_children
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_children then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t (yojson_of_list yojson_of_t))
               v_children
           in
           let bnd = ("children", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = Range.yojson_of_t v_selectionRange in
         ("selectionRange", arg) :: bnds
       in
       let bnds =
         let arg = Range.yojson_of_t v_range in
         ("range", arg) :: bnds
       in
       let bnds =
         if None = v_deprecated then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_deprecated
           in
           let bnd = ("deprecated", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = SymbolKind.yojson_of_t v_kind in
         ("kind", arg) :: bnds
       in
       let bnds =
         if None = v_detail then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_detail
           in
           let bnd = ("detail", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_name in
         ("name", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(name : string) ?(detail : string option) ~(kind : SymbolKind.t)
      ?(deprecated : bool option) ~(range : Range.t) ~(selectionRange : Range.t)
      ?(children : t list option) (() : unit) : t =
    { name; detail; kind; deprecated; range; selectionRange; children }
end

module DocumentSymbolOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentSymbolOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module DocumentSymbolParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentSymbolParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) textDocument_field with
           | Some textDocument_value -> { textDocument = textDocument_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) : t = { textDocument }
end

module DocumentSymbolRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.DocumentSymbolRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let documentSelector_value, workDoneProgress_value =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option) (() : unit) : t =
    { documentSelector; workDoneProgress }
end

module ErrorCodes = struct
  type t =
    | ParseError
    | InvalidRequest
    | MethodNotFound
    | InvalidParams
    | InternalError
    | ServerErrorStart
    | ServerErrorEnd
    | ServerNotInitialized
    | UnknownErrorCode
    | RequestCancelled
    | ContentModified

  let yojson_of_t (t : t) : Json.t =
    match t with
    | ParseError -> `Int (-32700)
    | InvalidRequest -> `Int (-32600)
    | MethodNotFound -> `Int (-32601)
    | InvalidParams -> `Int (-32602)
    | InternalError -> `Int (-32603)
    | ServerErrorStart -> `Int (-32099)
    | ServerErrorEnd -> `Int (-32000)
    | ServerNotInitialized -> `Int (-32002)
    | UnknownErrorCode -> `Int (-32001)
    | RequestCancelled -> `Int (-32800)
    | ContentModified -> `Int (-32801)

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int -32700 -> ParseError
    | `Int -32600 -> InvalidRequest
    | `Int -32601 -> MethodNotFound
    | `Int -32602 -> InvalidParams
    | `Int -32603 -> InternalError
    | `Int -32099 -> ServerErrorStart
    | `Int -32000 -> ServerErrorEnd
    | `Int -32002 -> ServerNotInitialized
    | `Int -32001 -> UnknownErrorCode
    | `Int -32800 -> RequestCancelled
    | `Int -32801 -> ContentModified
    | _ -> Json.error "t" json
end

module ExecuteCommandOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; commands : string list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ExecuteCommandOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and commands_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "commands" -> (
             match Ppx_yojson_conv_lib.( ! ) commands_field with
             | None ->
               let fvalue = list_of_yojson string_of_yojson _field_yojson in
               commands_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) commands_field )
           with
           | workDoneProgress_value, Some commands_value ->
             { workDoneProgress =
                 (match workDoneProgress_value with
                 | None -> None
                 | Some v -> v)
             ; commands = commands_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) commands_field)
                     None
                 , "commands" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress; commands = v_commands } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list yojson_of_string v_commands in
         ("commands", arg) :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) ~(commands : string list)
      (() : unit) : t =
    { workDoneProgress; commands }
end

module ExecuteCommandParams = struct
  type t =
    { command : string
    ; arguments : Json.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ExecuteCommandParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let command_field = ref None
       and arguments_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "command" -> (
             match Ppx_yojson_conv_lib.( ! ) command_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               command_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "arguments" -> (
             match Ppx_yojson_conv_lib.( ! ) arguments_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson Json.t_of_yojson)
                   _field_yojson
               in
               arguments_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) command_field
             , Ppx_yojson_conv_lib.( ! ) arguments_field )
           with
           | Some command_value, arguments_value ->
             { command = command_value
             ; arguments =
                 (match arguments_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) command_field)
                     None
                 , "command" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { command = v_command; arguments = v_arguments } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_arguments then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list Json.yojson_of_t))
               v_arguments
           in
           let bnd = ("arguments", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_command in
         ("command", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(command : string) ?(arguments : Json.t list option) (() : unit) :
      t =
    { command; arguments }
end

module ExecuteCommandRegistrationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; commands : string list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ExecuteCommandRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and commands_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "commands" -> (
             match Ppx_yojson_conv_lib.( ! ) commands_field with
             | None ->
               let fvalue = list_of_yojson string_of_yojson _field_yojson in
               commands_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) commands_field )
           with
           | workDoneProgress_value, Some commands_value ->
             { workDoneProgress =
                 (match workDoneProgress_value with
                 | None -> None
                 | Some v -> v)
             ; commands = commands_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) commands_field)
                     None
                 , "commands" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress; commands = v_commands } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list yojson_of_string v_commands in
         ("commands", arg) :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) ~(commands : string list)
      (() : unit) : t =
    { workDoneProgress; commands }
end

module FileChangeType = struct
  type t =
    | Created
    | Changed
    | Deleted

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Created -> `Int 1
    | Changed -> `Int 2
    | Deleted -> `Int 3

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Created
    | `Int 2 -> Changed
    | `Int 3 -> Deleted
    | _ -> Json.error "t" json
end

module FoldingRangeKind = struct
  type t =
    | Comment
    | Imports
    | Region

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Comment -> `String "comment"
    | Imports -> `String "imports"
    | Region -> `String "region"

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `String "comment" -> Comment
    | `String "imports" -> Imports
    | `String "region" -> Region
    | _ -> Json.error "t" json
end

module FoldingRange = struct
  type t =
    { startLine : int
    ; startCharacter : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; endLine : int
    ; endCharacter : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; kind : FoldingRangeKind.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.FoldingRange.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let startLine_field = ref None
       and startCharacter_field = ref None
       and endLine_field = ref None
       and endCharacter_field = ref None
       and kind_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "startLine" -> (
             match Ppx_yojson_conv_lib.( ! ) startLine_field with
             | None ->
               let fvalue = int_of_yojson _field_yojson in
               startLine_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "startCharacter" -> (
             match Ppx_yojson_conv_lib.( ! ) startCharacter_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson int_of_yojson _field_yojson
               in
               startCharacter_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "endLine" -> (
             match Ppx_yojson_conv_lib.( ! ) endLine_field with
             | None ->
               let fvalue = int_of_yojson _field_yojson in
               endLine_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "endCharacter" -> (
             match Ppx_yojson_conv_lib.( ! ) endCharacter_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson int_of_yojson _field_yojson
               in
               endCharacter_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "kind" -> (
             match Ppx_yojson_conv_lib.( ! ) kind_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson FoldingRangeKind.t_of_yojson
                   _field_yojson
               in
               kind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) startLine_field
             , Ppx_yojson_conv_lib.( ! ) startCharacter_field
             , Ppx_yojson_conv_lib.( ! ) endLine_field
             , Ppx_yojson_conv_lib.( ! ) endCharacter_field
             , Ppx_yojson_conv_lib.( ! ) kind_field )
           with
           | ( Some startLine_value
             , startCharacter_value
             , Some endLine_value
             , endCharacter_value
             , kind_value ) ->
             { startLine = startLine_value
             ; startCharacter =
                 (match startCharacter_value with
                 | None -> None
                 | Some v -> v)
             ; endLine = endLine_value
             ; endCharacter =
                 (match endCharacter_value with
                 | None -> None
                 | Some v -> v)
             ; kind =
                 (match kind_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) startLine_field)
                     None
                 , "startLine" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) endLine_field)
                     None
                 , "endLine" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { startLine = v_startLine
       ; startCharacter = v_startCharacter
       ; endLine = v_endLine
       ; endCharacter = v_endCharacter
       ; kind = v_kind
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_kind then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t FoldingRangeKind.yojson_of_t)
               v_kind
           in
           let bnd = ("kind", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_endCharacter then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_int) v_endCharacter
           in
           let bnd = ("endCharacter", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_int v_endLine in
         ("endLine", arg) :: bnds
       in
       let bnds =
         if None = v_startCharacter then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_int) v_startCharacter
           in
           let bnd = ("startCharacter", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_int v_startLine in
         ("startLine", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(startLine : int) ?(startCharacter : int option) ~(endLine : int)
      ?(endCharacter : int option) ?(kind : FoldingRangeKind.t option)
      (() : unit) : t =
    { startLine; startCharacter; endLine; endCharacter; kind }
end

module FoldingRangeOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.FoldingRangeOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module FoldingRangeParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.FoldingRangeParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) textDocument_field with
           | Some textDocument_value -> { textDocument = textDocument_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) : t = { textDocument }
end

module FoldingRangeRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.FoldingRangeRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and id_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "id" -> (
             match Ppx_yojson_conv_lib.( ! ) id_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               id_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let documentSelector_value, workDoneProgress_value, id_value =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) id_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; id =
               (match id_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       ; id = v_id
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_id then
           bnds
         else
           let arg = (Json.Nullable_option.yojson_of_t yojson_of_string) v_id in
           let bnd = ("id", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option) ?(id : string option) (() : unit) : t =
    { documentSelector; workDoneProgress; id }
end

module Hover = struct
  type contents =
    [ `MarkedString of MarkedString.t
    | `List of MarkedString.t list
    | `MarkupContent of MarkupContent.t
    ]

  let contents_of_yojson (json : Json.t) : contents =
    Json.Of.untagged_union "contents"
      [ (fun json -> `MarkedString (MarkedString.t_of_yojson json))
      ; (fun json -> `List (Json.Of.list MarkedString.t_of_yojson json))
      ; (fun json -> `MarkupContent (MarkupContent.t_of_yojson json))
      ]
      json

  let yojson_of_contents (contents : contents) : Json.t =
    match contents with
    | `MarkedString s -> MarkedString.yojson_of_t s
    | `List s -> Json.To.list MarkedString.yojson_of_t s
    | `MarkupContent s -> MarkupContent.yojson_of_t s

  type t =
    { contents : contents
    ; range : Range.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.Hover.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let contents_field = ref None
       and range_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "contents" -> (
             match Ppx_yojson_conv_lib.( ! ) contents_field with
             | None ->
               let fvalue = contents_of_yojson _field_yojson in
               contents_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "range" -> (
             match Ppx_yojson_conv_lib.( ! ) range_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson Range.t_of_yojson
                   _field_yojson
               in
               range_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) contents_field
             , Ppx_yojson_conv_lib.( ! ) range_field )
           with
           | Some contents_value, range_value ->
             { contents = contents_value
             ; range =
                 (match range_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) contents_field)
                     None
                 , "contents" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { contents = v_contents; range = v_range } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_range then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t Range.yojson_of_t) v_range
           in
           let bnd = ("range", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_contents v_contents in
         ("contents", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(contents : contents) ?(range : Range.t option) (() : unit) : t =
    { contents; range }
end

module HoverOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.HoverOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module HoverParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.HoverParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and position_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "position" -> (
             match Ppx_yojson_conv_lib.( ! ) position_field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               position_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) position_field )
           with
           | Some textDocument_value, Some position_value ->
             { textDocument = textDocument_value; position = position_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) position_field)
                     None
                 , "position" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; position = v_position } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Position.yojson_of_t v_position in
         ("position", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(position : Position.t)
      : t =
    { textDocument; position }
end

module HoverRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.HoverRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let documentSelector_value, workDoneProgress_value =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option) (() : unit) : t =
    { documentSelector; workDoneProgress }
end

module ImplementationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ImplementationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module ImplementationParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ImplementationParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and position_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "position" -> (
             match Ppx_yojson_conv_lib.( ! ) position_field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               position_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) position_field )
           with
           | Some textDocument_value, Some position_value ->
             { textDocument = textDocument_value; position = position_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) position_field)
                     None
                 , "position" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; position = v_position } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Position.yojson_of_t v_position in
         ("position", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(position : Position.t)
      : t =
    { textDocument; position }
end

module ImplementationRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ImplementationRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and id_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "id" -> (
             match Ppx_yojson_conv_lib.( ! ) id_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               id_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let documentSelector_value, workDoneProgress_value, id_value =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) id_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; id =
               (match id_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       ; id = v_id
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_id then
           bnds
         else
           let arg = (Json.Nullable_option.yojson_of_t yojson_of_string) v_id in
           let bnd = ("id", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option) ?(id : string option) (() : unit) : t =
    { documentSelector; workDoneProgress; id }
end

module InitializeError = struct
  type t = UnknownProtocolVersion

  let yojson_of_t (t : t) : Json.t =
    match t with
    | UnknownProtocolVersion -> `Int 1

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> UnknownProtocolVersion
    | _ -> Json.error "t" json
end

module InitializeParams = struct
  type clientInfo =
    { name : string
    ; version : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : clientInfo) -> ()

  let clientInfo_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.InitializeParams.clientInfo" in
     function
     | `Assoc field_yojsons as yojson -> (
       let name_field = ref None
       and version_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "name" -> (
             match Ppx_yojson_conv_lib.( ! ) name_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               name_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "version" -> (
             match Ppx_yojson_conv_lib.( ! ) version_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               version_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) name_field
             , Ppx_yojson_conv_lib.( ! ) version_field )
           with
           | Some name_value, version_value ->
             { name = name_value
             ; version =
                 (match version_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) name_field)
                     None
                 , "name" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> clientInfo)

  let _ = clientInfo_of_yojson

  let yojson_of_clientInfo =
    (function
     | { name = v_name; version = v_version } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_version then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_version
           in
           let bnd = ("version", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_name in
         ("name", arg) :: bnds
       in
       `Assoc bnds
      : clientInfo -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_clientInfo

  [@@@end]

  let create_clientInfo ~(name : string) ?(version : string option) (() : unit)
      : clientInfo =
    { name; version }

  type trace =
    [ `Off
    | `Messages
    | `Verbose
    ]

  let yojson_of_trace (trace : trace) : Json.t =
    match trace with
    | `Off -> `String "off"
    | `Messages -> `String "messages"
    | `Verbose -> `String "verbose"

  let trace_of_yojson (json : Json.t) : trace =
    match json with
    | `String "off" -> `Off
    | `String "messages" -> `Messages
    | `String "verbose" -> `Verbose
    | _ -> Json.error "trace" json

  type t =
    { processId : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; clientInfo : clientInfo Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; rootPath : string Json.Nullable_option.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; rootUri : DocumentUri.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; initializationOptions : Json.t option [@yojson.option]
    ; capabilities : ClientCapabilities.t
    ; trace : trace Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workspaceFolders :
        WorkspaceFolder.t list Json.Nullable_option.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.InitializeParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let processId_field = ref None
       and clientInfo_field = ref None
       and rootPath_field = ref None
       and rootUri_field = ref None
       and initializationOptions_field = ref None
       and capabilities_field = ref None
       and trace_field = ref None
       and workspaceFolders_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "processId" -> (
             match Ppx_yojson_conv_lib.( ! ) processId_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson int_of_yojson _field_yojson
               in
               processId_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "clientInfo" -> (
             match Ppx_yojson_conv_lib.( ! ) clientInfo_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson clientInfo_of_yojson
                   _field_yojson
               in
               clientInfo_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "rootPath" -> (
             match Ppx_yojson_conv_lib.( ! ) rootPath_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (Json.Nullable_option.t_of_yojson string_of_yojson)
                   _field_yojson
               in
               rootPath_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "rootUri" -> (
             match Ppx_yojson_conv_lib.( ! ) rootUri_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentUri.t_of_yojson
                   _field_yojson
               in
               rootUri_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "initializationOptions" -> (
             match Ppx_yojson_conv_lib.( ! ) initializationOptions_field with
             | None ->
               let fvalue = Json.t_of_yojson _field_yojson in
               initializationOptions_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "capabilities" -> (
             match Ppx_yojson_conv_lib.( ! ) capabilities_field with
             | None ->
               let fvalue = ClientCapabilities.t_of_yojson _field_yojson in
               capabilities_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "trace" -> (
             match Ppx_yojson_conv_lib.( ! ) trace_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson trace_of_yojson _field_yojson
               in
               trace_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workspaceFolders" -> (
             match Ppx_yojson_conv_lib.( ! ) workspaceFolders_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (Json.Nullable_option.t_of_yojson
                      (list_of_yojson WorkspaceFolder.t_of_yojson))
                   _field_yojson
               in
               workspaceFolders_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) processId_field
             , Ppx_yojson_conv_lib.( ! ) clientInfo_field
             , Ppx_yojson_conv_lib.( ! ) rootPath_field
             , Ppx_yojson_conv_lib.( ! ) rootUri_field
             , Ppx_yojson_conv_lib.( ! ) initializationOptions_field
             , Ppx_yojson_conv_lib.( ! ) capabilities_field
             , Ppx_yojson_conv_lib.( ! ) trace_field
             , Ppx_yojson_conv_lib.( ! ) workspaceFolders_field )
           with
           | ( processId_value
             , clientInfo_value
             , rootPath_value
             , rootUri_value
             , initializationOptions_value
             , Some capabilities_value
             , trace_value
             , workspaceFolders_value ) ->
             { processId =
                 (match processId_value with
                 | None -> None
                 | Some v -> v)
             ; clientInfo =
                 (match clientInfo_value with
                 | None -> None
                 | Some v -> v)
             ; rootPath =
                 (match rootPath_value with
                 | None -> None
                 | Some v -> v)
             ; rootUri =
                 (match rootUri_value with
                 | None -> None
                 | Some v -> v)
             ; initializationOptions = initializationOptions_value
             ; capabilities = capabilities_value
             ; trace =
                 (match trace_value with
                 | None -> None
                 | Some v -> v)
             ; workspaceFolders =
                 (match workspaceFolders_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) capabilities_field)
                     None
                 , "capabilities" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { processId = v_processId
       ; clientInfo = v_clientInfo
       ; rootPath = v_rootPath
       ; rootUri = v_rootUri
       ; initializationOptions = v_initializationOptions
       ; capabilities = v_capabilities
       ; trace = v_trace
       ; workspaceFolders = v_workspaceFolders
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workspaceFolders then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (Json.Nullable_option.yojson_of_t
                   (yojson_of_list WorkspaceFolder.yojson_of_t)))
               v_workspaceFolders
           in
           let bnd = ("workspaceFolders", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_trace then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_trace) v_trace
           in
           let bnd = ("trace", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = ClientCapabilities.yojson_of_t v_capabilities in
         ("capabilities", arg) :: bnds
       in
       let bnds =
         match v_initializationOptions with
         | None -> bnds
         | Some v ->
           let arg = Json.yojson_of_t v in
           let bnd = ("initializationOptions", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_rootUri then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentUri.yojson_of_t)
               v_rootUri
           in
           let bnd = ("rootUri", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_rootPath then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (Json.Nullable_option.yojson_of_t yojson_of_string))
               v_rootPath
           in
           let bnd = ("rootPath", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_clientInfo then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_clientInfo)
               v_clientInfo
           in
           let bnd = ("clientInfo", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_processId then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_int) v_processId
           in
           let bnd = ("processId", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(processId : int option) ?(clientInfo : clientInfo option)
      ?(rootPath : string Json.Nullable_option.t option)
      ?(rootUri : DocumentUri.t option) ?(initializationOptions : Json.t option)
      ~(capabilities : ClientCapabilities.t) ?(trace : trace option)
      ?(workspaceFolders : WorkspaceFolder.t list Json.Nullable_option.t option)
      (() : unit) : t =
    { processId
    ; clientInfo
    ; rootPath
    ; rootUri
    ; initializationOptions
    ; capabilities
    ; trace
    ; workspaceFolders
    }
end

module WorkspaceFoldersServerCapabilities = struct
  type changeNotifications =
    [ `String of string
    | `Bool of bool
    ]

  let changeNotifications_of_yojson (json : Json.t) : changeNotifications =
    match json with
    | `String j -> `String j
    | `Bool j -> `Bool j
    | _ -> Json.error "changeNotifications" json

  let yojson_of_changeNotifications (changeNotifications : changeNotifications)
      : Json.t =
    match changeNotifications with
    | `String j -> `String j
    | `Bool j -> `Bool j

  type t =
    { supported : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; changeNotifications : changeNotifications Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkspaceFoldersServerCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let supported_field = ref None
       and changeNotifications_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "supported" -> (
             match Ppx_yojson_conv_lib.( ! ) supported_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               supported_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "changeNotifications" -> (
             match Ppx_yojson_conv_lib.( ! ) changeNotifications_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson changeNotifications_of_yojson
                   _field_yojson
               in
               changeNotifications_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let supported_value, changeNotifications_value =
             ( Ppx_yojson_conv_lib.( ! ) supported_field
             , Ppx_yojson_conv_lib.( ! ) changeNotifications_field )
           in
           { supported =
               (match supported_value with
               | None -> None
               | Some v -> v)
           ; changeNotifications =
               (match changeNotifications_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { supported = v_supported; changeNotifications = v_changeNotifications }
       ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_changeNotifications then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_changeNotifications)
               v_changeNotifications
           in
           let bnd = ("changeNotifications", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_supported then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_supported
           in
           let bnd = ("supported", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(supported : bool option)
      ?(changeNotifications : changeNotifications option) (() : unit) : t =
    { supported; changeNotifications }
end

module SelectionRangeOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.SelectionRangeOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module SelectionRangeRegistrationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.SelectionRangeRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and documentSelector_field = ref None
       and id_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "id" -> (
             match Ppx_yojson_conv_lib.( ! ) id_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               id_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value, documentSelector_value, id_value =
             ( Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) id_field )
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; id =
               (match id_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress
       ; documentSelector = v_documentSelector
       ; id = v_id
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_id then
           bnds
         else
           let arg = (Json.Nullable_option.yojson_of_t yojson_of_string) v_id in
           let bnd = ("id", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option)
      ?(documentSelector : DocumentSelector.t option) ?(id : string option)
      (() : unit) : t =
    { workDoneProgress; documentSelector; id }
end

module RenameOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; prepareProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.RenameOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and prepareProvider_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "prepareProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) prepareProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               prepareProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value, prepareProvider_value =
             ( Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) prepareProvider_field )
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; prepareProvider =
               (match prepareProvider_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress
       ; prepareProvider = v_prepareProvider
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_prepareProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_prepareProvider
           in
           let bnd = ("prepareProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) ?(prepareProvider : bool option)
      (() : unit) : t =
    { workDoneProgress; prepareProvider }
end

module ReferenceOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ReferenceOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module TypeDefinitionOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TypeDefinitionOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module TypeDefinitionRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; id : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TypeDefinitionRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and id_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "id" -> (
             match Ppx_yojson_conv_lib.( ! ) id_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               id_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let documentSelector_value, workDoneProgress_value, id_value =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) id_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; id =
               (match id_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       ; id = v_id
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_id then
           bnds
         else
           let arg = (Json.Nullable_option.yojson_of_t yojson_of_string) v_id in
           let bnd = ("id", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option) ?(id : string option) (() : unit) : t =
    { documentSelector; workDoneProgress; id }
end

module SignatureHelpOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; triggerCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; retriggerCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.SignatureHelpOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and triggerCharacters_field = ref None
       and retriggerCharacters_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "triggerCharacters" -> (
             match Ppx_yojson_conv_lib.( ! ) triggerCharacters_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson string_of_yojson)
                   _field_yojson
               in
               triggerCharacters_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "retriggerCharacters" -> (
             match Ppx_yojson_conv_lib.( ! ) retriggerCharacters_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson string_of_yojson)
                   _field_yojson
               in
               retriggerCharacters_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( workDoneProgress_value
               , triggerCharacters_value
               , retriggerCharacters_value ) =
             ( Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) triggerCharacters_field
             , Ppx_yojson_conv_lib.( ! ) retriggerCharacters_field )
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; triggerCharacters =
               (match triggerCharacters_value with
               | None -> None
               | Some v -> v)
           ; retriggerCharacters =
               (match retriggerCharacters_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress
       ; triggerCharacters = v_triggerCharacters
       ; retriggerCharacters = v_retriggerCharacters
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_retriggerCharacters then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list yojson_of_string))
               v_retriggerCharacters
           in
           let bnd = ("retriggerCharacters", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_triggerCharacters then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list yojson_of_string))
               v_triggerCharacters
           in
           let bnd = ("triggerCharacters", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option)
      ?(triggerCharacters : string list option)
      ?(retriggerCharacters : string list option) (() : unit) : t =
    { workDoneProgress; triggerCharacters; retriggerCharacters }
end

module SaveOptions = struct
  type t =
    { includeText : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.SaveOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let includeText_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "includeText" -> (
             match Ppx_yojson_conv_lib.( ! ) includeText_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               includeText_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let includeText_value =
             Ppx_yojson_conv_lib.( ! ) includeText_field
           in
           { includeText =
               (match includeText_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { includeText = v_includeText } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_includeText then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_includeText
           in
           let bnd = ("includeText", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(includeText : bool option) (() : unit) : t = { includeText }
end

module TextDocumentSyncKind = struct
  type t =
    | None
    | Full
    | Incremental

  let yojson_of_t (t : t) : Json.t =
    match t with
    | None -> `Int 0
    | Full -> `Int 1
    | Incremental -> `Int 2

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 0 -> None
    | `Int 1 -> Full
    | `Int 2 -> Incremental
    | _ -> Json.error "t" json
end

module TextDocumentSyncOptions = struct
  type t =
    { openClose : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; change : TextDocumentSyncKind.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; willSave : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; willSaveWaitUntil : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; save : SaveOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TextDocumentSyncOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let openClose_field = ref None
       and change_field = ref None
       and willSave_field = ref None
       and willSaveWaitUntil_field = ref None
       and save_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "openClose" -> (
             match Ppx_yojson_conv_lib.( ! ) openClose_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               openClose_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "change" -> (
             match Ppx_yojson_conv_lib.( ! ) change_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   TextDocumentSyncKind.t_of_yojson _field_yojson
               in
               change_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "willSave" -> (
             match Ppx_yojson_conv_lib.( ! ) willSave_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               willSave_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "willSaveWaitUntil" -> (
             match Ppx_yojson_conv_lib.( ! ) willSaveWaitUntil_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               willSaveWaitUntil_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "save" -> (
             match Ppx_yojson_conv_lib.( ! ) save_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson SaveOptions.t_of_yojson
                   _field_yojson
               in
               save_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( openClose_value
               , change_value
               , willSave_value
               , willSaveWaitUntil_value
               , save_value ) =
             ( Ppx_yojson_conv_lib.( ! ) openClose_field
             , Ppx_yojson_conv_lib.( ! ) change_field
             , Ppx_yojson_conv_lib.( ! ) willSave_field
             , Ppx_yojson_conv_lib.( ! ) willSaveWaitUntil_field
             , Ppx_yojson_conv_lib.( ! ) save_field )
           in
           { openClose =
               (match openClose_value with
               | None -> None
               | Some v -> v)
           ; change =
               (match change_value with
               | None -> None
               | Some v -> v)
           ; willSave =
               (match willSave_value with
               | None -> None
               | Some v -> v)
           ; willSaveWaitUntil =
               (match willSaveWaitUntil_value with
               | None -> None
               | Some v -> v)
           ; save =
               (match save_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { openClose = v_openClose
       ; change = v_change
       ; willSave = v_willSave
       ; willSaveWaitUntil = v_willSaveWaitUntil
       ; save = v_save
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_save then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t SaveOptions.yojson_of_t) v_save
           in
           let bnd = ("save", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_willSaveWaitUntil then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_willSaveWaitUntil
           in
           let bnd = ("willSaveWaitUntil", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_willSave then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_willSave
           in
           let bnd = ("willSave", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_change then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t TextDocumentSyncKind.yojson_of_t)
               v_change
           in
           let bnd = ("change", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_openClose then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_openClose
           in
           let bnd = ("openClose", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(openClose : bool option)
      ?(change : TextDocumentSyncKind.t option) ?(willSave : bool option)
      ?(willSaveWaitUntil : bool option) ?(save : SaveOptions.t option)
      (() : unit) : t =
    { openClose; change; willSave; willSaveWaitUntil; save }
end

module ServerCapabilities = struct
  type workspace =
    { workspaceFolders :
        WorkspaceFoldersServerCapabilities.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : workspace) -> ()

  let workspace_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ServerCapabilities.workspace" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workspaceFolders_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workspaceFolders" -> (
             match Ppx_yojson_conv_lib.( ! ) workspaceFolders_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   WorkspaceFoldersServerCapabilities.t_of_yojson _field_yojson
               in
               workspaceFolders_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workspaceFolders_value =
             Ppx_yojson_conv_lib.( ! ) workspaceFolders_field
           in
           { workspaceFolders =
               (match workspaceFolders_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> workspace)

  let _ = workspace_of_yojson

  let yojson_of_workspace =
    (function
     | { workspaceFolders = v_workspaceFolders } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workspaceFolders then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                WorkspaceFoldersServerCapabilities.yojson_of_t)
               v_workspaceFolders
           in
           let bnd = ("workspaceFolders", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : workspace -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_workspace

  [@@@end]

  let create_workspace
      ?(workspaceFolders : WorkspaceFoldersServerCapabilities.t option)
      (() : unit) : workspace =
    { workspaceFolders }

  type textDocumentSync =
    [ `TextDocumentSyncOptions of TextDocumentSyncOptions.t
    | `Int of int
    ]

  let textDocumentSync_of_yojson (json : Json.t) : textDocumentSync =
    match json with
    | `Int j -> `Int j
    | _ ->
      Json.Of.untagged_union "textDocumentSync"
        [ (fun json ->
            `TextDocumentSyncOptions (TextDocumentSyncOptions.t_of_yojson json))
        ]
        json

  let yojson_of_textDocumentSync (textDocumentSync : textDocumentSync) : Json.t
      =
    match textDocumentSync with
    | `Int j -> `Int j
    | `TextDocumentSyncOptions s -> TextDocumentSyncOptions.yojson_of_t s

  type hoverProvider =
    [ `Bool of bool
    | `HoverOptions of HoverOptions.t
    ]

  let hoverProvider_of_yojson (json : Json.t) : hoverProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "hoverProvider"
        [ (fun json -> `HoverOptions (HoverOptions.t_of_yojson json)) ]
        json

  let yojson_of_hoverProvider (hoverProvider : hoverProvider) : Json.t =
    match hoverProvider with
    | `Bool j -> `Bool j
    | `HoverOptions s -> HoverOptions.yojson_of_t s

  type declarationProvider =
    [ `Bool of bool
    | `DeclarationOptions of DeclarationOptions.t
    | `DeclarationRegistrationOptions of DeclarationRegistrationOptions.t
    ]

  let declarationProvider_of_yojson (json : Json.t) : declarationProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "declarationProvider"
        [ (fun json ->
            `DeclarationOptions (DeclarationOptions.t_of_yojson json))
        ; (fun json ->
            `DeclarationRegistrationOptions
              (DeclarationRegistrationOptions.t_of_yojson json))
        ]
        json

  let yojson_of_declarationProvider (declarationProvider : declarationProvider)
      : Json.t =
    match declarationProvider with
    | `Bool j -> `Bool j
    | `DeclarationOptions s -> DeclarationOptions.yojson_of_t s
    | `DeclarationRegistrationOptions s ->
      DeclarationRegistrationOptions.yojson_of_t s

  type definitionProvider =
    [ `Bool of bool
    | `DefinitionOptions of DefinitionOptions.t
    ]

  let definitionProvider_of_yojson (json : Json.t) : definitionProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "definitionProvider"
        [ (fun json -> `DefinitionOptions (DefinitionOptions.t_of_yojson json))
        ]
        json

  let yojson_of_definitionProvider (definitionProvider : definitionProvider) :
      Json.t =
    match definitionProvider with
    | `Bool j -> `Bool j
    | `DefinitionOptions s -> DefinitionOptions.yojson_of_t s

  type typeDefinitionProvider =
    [ `Bool of bool
    | `TypeDefinitionOptions of TypeDefinitionOptions.t
    | `TypeDefinitionRegistrationOptions of TypeDefinitionRegistrationOptions.t
    ]

  let typeDefinitionProvider_of_yojson (json : Json.t) : typeDefinitionProvider
      =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "typeDefinitionProvider"
        [ (fun json ->
            `TypeDefinitionOptions (TypeDefinitionOptions.t_of_yojson json))
        ; (fun json ->
            `TypeDefinitionRegistrationOptions
              (TypeDefinitionRegistrationOptions.t_of_yojson json))
        ]
        json

  let yojson_of_typeDefinitionProvider
      (typeDefinitionProvider : typeDefinitionProvider) : Json.t =
    match typeDefinitionProvider with
    | `Bool j -> `Bool j
    | `TypeDefinitionOptions s -> TypeDefinitionOptions.yojson_of_t s
    | `TypeDefinitionRegistrationOptions s ->
      TypeDefinitionRegistrationOptions.yojson_of_t s

  type implementationProvider =
    [ `Bool of bool
    | `ImplementationOptions of ImplementationOptions.t
    | `ImplementationRegistrationOptions of ImplementationRegistrationOptions.t
    ]

  let implementationProvider_of_yojson (json : Json.t) : implementationProvider
      =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "implementationProvider"
        [ (fun json ->
            `ImplementationOptions (ImplementationOptions.t_of_yojson json))
        ; (fun json ->
            `ImplementationRegistrationOptions
              (ImplementationRegistrationOptions.t_of_yojson json))
        ]
        json

  let yojson_of_implementationProvider
      (implementationProvider : implementationProvider) : Json.t =
    match implementationProvider with
    | `Bool j -> `Bool j
    | `ImplementationOptions s -> ImplementationOptions.yojson_of_t s
    | `ImplementationRegistrationOptions s ->
      ImplementationRegistrationOptions.yojson_of_t s

  type referencesProvider =
    [ `Bool of bool
    | `ReferenceOptions of ReferenceOptions.t
    ]

  let referencesProvider_of_yojson (json : Json.t) : referencesProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "referencesProvider"
        [ (fun json -> `ReferenceOptions (ReferenceOptions.t_of_yojson json)) ]
        json

  let yojson_of_referencesProvider (referencesProvider : referencesProvider) :
      Json.t =
    match referencesProvider with
    | `Bool j -> `Bool j
    | `ReferenceOptions s -> ReferenceOptions.yojson_of_t s

  type documentHighlightProvider =
    [ `Bool of bool
    | `DocumentHighlightOptions of DocumentHighlightOptions.t
    ]

  let documentHighlightProvider_of_yojson (json : Json.t) :
      documentHighlightProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "documentHighlightProvider"
        [ (fun json ->
            `DocumentHighlightOptions
              (DocumentHighlightOptions.t_of_yojson json))
        ]
        json

  let yojson_of_documentHighlightProvider
      (documentHighlightProvider : documentHighlightProvider) : Json.t =
    match documentHighlightProvider with
    | `Bool j -> `Bool j
    | `DocumentHighlightOptions s -> DocumentHighlightOptions.yojson_of_t s

  type documentSymbolProvider =
    [ `Bool of bool
    | `DocumentSymbolOptions of DocumentSymbolOptions.t
    ]

  let documentSymbolProvider_of_yojson (json : Json.t) : documentSymbolProvider
      =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "documentSymbolProvider"
        [ (fun json ->
            `DocumentSymbolOptions (DocumentSymbolOptions.t_of_yojson json))
        ]
        json

  let yojson_of_documentSymbolProvider
      (documentSymbolProvider : documentSymbolProvider) : Json.t =
    match documentSymbolProvider with
    | `Bool j -> `Bool j
    | `DocumentSymbolOptions s -> DocumentSymbolOptions.yojson_of_t s

  type codeActionProvider =
    [ `Bool of bool
    | `CodeActionOptions of CodeActionOptions.t
    ]

  let codeActionProvider_of_yojson (json : Json.t) : codeActionProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "codeActionProvider"
        [ (fun json -> `CodeActionOptions (CodeActionOptions.t_of_yojson json))
        ]
        json

  let yojson_of_codeActionProvider (codeActionProvider : codeActionProvider) :
      Json.t =
    match codeActionProvider with
    | `Bool j -> `Bool j
    | `CodeActionOptions s -> CodeActionOptions.yojson_of_t s

  type colorProvider =
    [ `Bool of bool
    | `DocumentColorOptions of DocumentColorOptions.t
    | `DocumentColorRegistrationOptions of DocumentColorRegistrationOptions.t
    ]

  let colorProvider_of_yojson (json : Json.t) : colorProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "colorProvider"
        [ (fun json ->
            `DocumentColorOptions (DocumentColorOptions.t_of_yojson json))
        ; (fun json ->
            `DocumentColorRegistrationOptions
              (DocumentColorRegistrationOptions.t_of_yojson json))
        ]
        json

  let yojson_of_colorProvider (colorProvider : colorProvider) : Json.t =
    match colorProvider with
    | `Bool j -> `Bool j
    | `DocumentColorOptions s -> DocumentColorOptions.yojson_of_t s
    | `DocumentColorRegistrationOptions s ->
      DocumentColorRegistrationOptions.yojson_of_t s

  type documentFormattingProvider =
    [ `Bool of bool
    | `DocumentFormattingOptions of DocumentFormattingOptions.t
    ]

  let documentFormattingProvider_of_yojson (json : Json.t) :
      documentFormattingProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "documentFormattingProvider"
        [ (fun json ->
            `DocumentFormattingOptions
              (DocumentFormattingOptions.t_of_yojson json))
        ]
        json

  let yojson_of_documentFormattingProvider
      (documentFormattingProvider : documentFormattingProvider) : Json.t =
    match documentFormattingProvider with
    | `Bool j -> `Bool j
    | `DocumentFormattingOptions s -> DocumentFormattingOptions.yojson_of_t s

  type documentRangeFormattingProvider =
    [ `Bool of bool
    | `DocumentRangeFormattingOptions of DocumentRangeFormattingOptions.t
    ]

  let documentRangeFormattingProvider_of_yojson (json : Json.t) :
      documentRangeFormattingProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "documentRangeFormattingProvider"
        [ (fun json ->
            `DocumentRangeFormattingOptions
              (DocumentRangeFormattingOptions.t_of_yojson json))
        ]
        json

  let yojson_of_documentRangeFormattingProvider
      (documentRangeFormattingProvider : documentRangeFormattingProvider) :
      Json.t =
    match documentRangeFormattingProvider with
    | `Bool j -> `Bool j
    | `DocumentRangeFormattingOptions s ->
      DocumentRangeFormattingOptions.yojson_of_t s

  type renameProvider =
    [ `Bool of bool
    | `RenameOptions of RenameOptions.t
    ]

  let renameProvider_of_yojson (json : Json.t) : renameProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "renameProvider"
        [ (fun json -> `RenameOptions (RenameOptions.t_of_yojson json)) ]
        json

  let yojson_of_renameProvider (renameProvider : renameProvider) : Json.t =
    match renameProvider with
    | `Bool j -> `Bool j
    | `RenameOptions s -> RenameOptions.yojson_of_t s

  type foldingRangeProvider =
    [ `Bool of bool
    | `FoldingRangeOptions of FoldingRangeOptions.t
    | `FoldingRangeRegistrationOptions of FoldingRangeRegistrationOptions.t
    ]

  let foldingRangeProvider_of_yojson (json : Json.t) : foldingRangeProvider =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "foldingRangeProvider"
        [ (fun json ->
            `FoldingRangeOptions (FoldingRangeOptions.t_of_yojson json))
        ; (fun json ->
            `FoldingRangeRegistrationOptions
              (FoldingRangeRegistrationOptions.t_of_yojson json))
        ]
        json

  let yojson_of_foldingRangeProvider
      (foldingRangeProvider : foldingRangeProvider) : Json.t =
    match foldingRangeProvider with
    | `Bool j -> `Bool j
    | `FoldingRangeOptions s -> FoldingRangeOptions.yojson_of_t s
    | `FoldingRangeRegistrationOptions s ->
      FoldingRangeRegistrationOptions.yojson_of_t s

  type selectionRangeProvider =
    [ `Bool of bool
    | `SelectionRangeOptions of SelectionRangeOptions.t
    | `SelectionRangeRegistrationOptions of SelectionRangeRegistrationOptions.t
    ]

  let selectionRangeProvider_of_yojson (json : Json.t) : selectionRangeProvider
      =
    match json with
    | `Bool j -> `Bool j
    | _ ->
      Json.Of.untagged_union "selectionRangeProvider"
        [ (fun json ->
            `SelectionRangeOptions (SelectionRangeOptions.t_of_yojson json))
        ; (fun json ->
            `SelectionRangeRegistrationOptions
              (SelectionRangeRegistrationOptions.t_of_yojson json))
        ]
        json

  let yojson_of_selectionRangeProvider
      (selectionRangeProvider : selectionRangeProvider) : Json.t =
    match selectionRangeProvider with
    | `Bool j -> `Bool j
    | `SelectionRangeOptions s -> SelectionRangeOptions.yojson_of_t s
    | `SelectionRangeRegistrationOptions s ->
      SelectionRangeRegistrationOptions.yojson_of_t s

  type t =
    { textDocumentSync : textDocumentSync Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; completionProvider : CompletionOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; hoverProvider : hoverProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; signatureHelpProvider : SignatureHelpOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; declarationProvider : declarationProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; definitionProvider : definitionProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; typeDefinitionProvider : typeDefinitionProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; implementationProvider : implementationProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; referencesProvider : referencesProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentHighlightProvider :
        documentHighlightProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentSymbolProvider : documentSymbolProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeActionProvider : codeActionProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; codeLensProvider : CodeLensOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentLinkProvider : DocumentLinkOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; colorProvider : colorProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentFormattingProvider :
        documentFormattingProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentRangeFormattingProvider :
        documentRangeFormattingProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; documentOnTypeFormattingProvider :
        DocumentOnTypeFormattingOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; renameProvider : renameProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; foldingRangeProvider : foldingRangeProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; executeCommandProvider : ExecuteCommandOptions.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; selectionRangeProvider : selectionRangeProvider Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workspaceSymbolProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workspace : workspace Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; experimental : Json.t option [@yojson.option]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ServerCapabilities.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocumentSync_field = ref None
       and completionProvider_field = ref None
       and hoverProvider_field = ref None
       and signatureHelpProvider_field = ref None
       and declarationProvider_field = ref None
       and definitionProvider_field = ref None
       and typeDefinitionProvider_field = ref None
       and implementationProvider_field = ref None
       and referencesProvider_field = ref None
       and documentHighlightProvider_field = ref None
       and documentSymbolProvider_field = ref None
       and codeActionProvider_field = ref None
       and codeLensProvider_field = ref None
       and documentLinkProvider_field = ref None
       and colorProvider_field = ref None
       and documentFormattingProvider_field = ref None
       and documentRangeFormattingProvider_field = ref None
       and documentOnTypeFormattingProvider_field = ref None
       and renameProvider_field = ref None
       and foldingRangeProvider_field = ref None
       and executeCommandProvider_field = ref None
       and selectionRangeProvider_field = ref None
       and workspaceSymbolProvider_field = ref None
       and workspace_field = ref None
       and experimental_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocumentSync" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocumentSync_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson textDocumentSync_of_yojson
                   _field_yojson
               in
               textDocumentSync_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "completionProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) completionProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson CompletionOptions.t_of_yojson
                   _field_yojson
               in
               completionProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "hoverProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) hoverProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson hoverProvider_of_yojson
                   _field_yojson
               in
               hoverProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "signatureHelpProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) signatureHelpProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   SignatureHelpOptions.t_of_yojson _field_yojson
               in
               signatureHelpProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "declarationProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) declarationProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson declarationProvider_of_yojson
                   _field_yojson
               in
               declarationProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "definitionProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) definitionProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson definitionProvider_of_yojson
                   _field_yojson
               in
               definitionProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "typeDefinitionProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) typeDefinitionProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   typeDefinitionProvider_of_yojson _field_yojson
               in
               typeDefinitionProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "implementationProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) implementationProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   implementationProvider_of_yojson _field_yojson
               in
               implementationProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "referencesProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) referencesProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson referencesProvider_of_yojson
                   _field_yojson
               in
               referencesProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentHighlightProvider" -> (
             match
               Ppx_yojson_conv_lib.( ! ) documentHighlightProvider_field
             with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   documentHighlightProvider_of_yojson _field_yojson
               in
               documentHighlightProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentSymbolProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSymbolProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   documentSymbolProvider_of_yojson _field_yojson
               in
               documentSymbolProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "codeActionProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) codeActionProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson codeActionProvider_of_yojson
                   _field_yojson
               in
               codeActionProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "codeLensProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) codeLensProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson CodeLensOptions.t_of_yojson
                   _field_yojson
               in
               codeLensProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentLinkProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) documentLinkProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DocumentLinkOptions.t_of_yojson _field_yojson
               in
               documentLinkProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "colorProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) colorProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson colorProvider_of_yojson
                   _field_yojson
               in
               colorProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentFormattingProvider" -> (
             match
               Ppx_yojson_conv_lib.( ! ) documentFormattingProvider_field
             with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   documentFormattingProvider_of_yojson _field_yojson
               in
               documentFormattingProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentRangeFormattingProvider" -> (
             match
               Ppx_yojson_conv_lib.( ! ) documentRangeFormattingProvider_field
             with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   documentRangeFormattingProvider_of_yojson _field_yojson
               in
               documentRangeFormattingProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentOnTypeFormattingProvider" -> (
             match
               Ppx_yojson_conv_lib.( ! ) documentOnTypeFormattingProvider_field
             with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   DocumentOnTypeFormattingOptions.t_of_yojson _field_yojson
               in
               documentOnTypeFormattingProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "renameProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) renameProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson renameProvider_of_yojson
                   _field_yojson
               in
               renameProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "foldingRangeProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) foldingRangeProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson foldingRangeProvider_of_yojson
                   _field_yojson
               in
               foldingRangeProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "executeCommandProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) executeCommandProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   ExecuteCommandOptions.t_of_yojson _field_yojson
               in
               executeCommandProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "selectionRangeProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) selectionRangeProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   selectionRangeProvider_of_yojson _field_yojson
               in
               selectionRangeProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workspaceSymbolProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) workspaceSymbolProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workspaceSymbolProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workspace" -> (
             match Ppx_yojson_conv_lib.( ! ) workspace_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson workspace_of_yojson
                   _field_yojson
               in
               workspace_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "experimental" -> (
             match Ppx_yojson_conv_lib.( ! ) experimental_field with
             | None ->
               let fvalue = Json.t_of_yojson _field_yojson in
               experimental_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( textDocumentSync_value
               , completionProvider_value
               , hoverProvider_value
               , signatureHelpProvider_value
               , declarationProvider_value
               , definitionProvider_value
               , typeDefinitionProvider_value
               , implementationProvider_value
               , referencesProvider_value
               , documentHighlightProvider_value
               , documentSymbolProvider_value
               , codeActionProvider_value
               , codeLensProvider_value
               , documentLinkProvider_value
               , colorProvider_value
               , documentFormattingProvider_value
               , documentRangeFormattingProvider_value
               , documentOnTypeFormattingProvider_value
               , renameProvider_value
               , foldingRangeProvider_value
               , executeCommandProvider_value
               , selectionRangeProvider_value
               , workspaceSymbolProvider_value
               , workspace_value
               , experimental_value ) =
             ( Ppx_yojson_conv_lib.( ! ) textDocumentSync_field
             , Ppx_yojson_conv_lib.( ! ) completionProvider_field
             , Ppx_yojson_conv_lib.( ! ) hoverProvider_field
             , Ppx_yojson_conv_lib.( ! ) signatureHelpProvider_field
             , Ppx_yojson_conv_lib.( ! ) declarationProvider_field
             , Ppx_yojson_conv_lib.( ! ) definitionProvider_field
             , Ppx_yojson_conv_lib.( ! ) typeDefinitionProvider_field
             , Ppx_yojson_conv_lib.( ! ) implementationProvider_field
             , Ppx_yojson_conv_lib.( ! ) referencesProvider_field
             , Ppx_yojson_conv_lib.( ! ) documentHighlightProvider_field
             , Ppx_yojson_conv_lib.( ! ) documentSymbolProvider_field
             , Ppx_yojson_conv_lib.( ! ) codeActionProvider_field
             , Ppx_yojson_conv_lib.( ! ) codeLensProvider_field
             , Ppx_yojson_conv_lib.( ! ) documentLinkProvider_field
             , Ppx_yojson_conv_lib.( ! ) colorProvider_field
             , Ppx_yojson_conv_lib.( ! ) documentFormattingProvider_field
             , Ppx_yojson_conv_lib.( ! ) documentRangeFormattingProvider_field
             , Ppx_yojson_conv_lib.( ! ) documentOnTypeFormattingProvider_field
             , Ppx_yojson_conv_lib.( ! ) renameProvider_field
             , Ppx_yojson_conv_lib.( ! ) foldingRangeProvider_field
             , Ppx_yojson_conv_lib.( ! ) executeCommandProvider_field
             , Ppx_yojson_conv_lib.( ! ) selectionRangeProvider_field
             , Ppx_yojson_conv_lib.( ! ) workspaceSymbolProvider_field
             , Ppx_yojson_conv_lib.( ! ) workspace_field
             , Ppx_yojson_conv_lib.( ! ) experimental_field )
           in
           { textDocumentSync =
               (match textDocumentSync_value with
               | None -> None
               | Some v -> v)
           ; completionProvider =
               (match completionProvider_value with
               | None -> None
               | Some v -> v)
           ; hoverProvider =
               (match hoverProvider_value with
               | None -> None
               | Some v -> v)
           ; signatureHelpProvider =
               (match signatureHelpProvider_value with
               | None -> None
               | Some v -> v)
           ; declarationProvider =
               (match declarationProvider_value with
               | None -> None
               | Some v -> v)
           ; definitionProvider =
               (match definitionProvider_value with
               | None -> None
               | Some v -> v)
           ; typeDefinitionProvider =
               (match typeDefinitionProvider_value with
               | None -> None
               | Some v -> v)
           ; implementationProvider =
               (match implementationProvider_value with
               | None -> None
               | Some v -> v)
           ; referencesProvider =
               (match referencesProvider_value with
               | None -> None
               | Some v -> v)
           ; documentHighlightProvider =
               (match documentHighlightProvider_value with
               | None -> None
               | Some v -> v)
           ; documentSymbolProvider =
               (match documentSymbolProvider_value with
               | None -> None
               | Some v -> v)
           ; codeActionProvider =
               (match codeActionProvider_value with
               | None -> None
               | Some v -> v)
           ; codeLensProvider =
               (match codeLensProvider_value with
               | None -> None
               | Some v -> v)
           ; documentLinkProvider =
               (match documentLinkProvider_value with
               | None -> None
               | Some v -> v)
           ; colorProvider =
               (match colorProvider_value with
               | None -> None
               | Some v -> v)
           ; documentFormattingProvider =
               (match documentFormattingProvider_value with
               | None -> None
               | Some v -> v)
           ; documentRangeFormattingProvider =
               (match documentRangeFormattingProvider_value with
               | None -> None
               | Some v -> v)
           ; documentOnTypeFormattingProvider =
               (match documentOnTypeFormattingProvider_value with
               | None -> None
               | Some v -> v)
           ; renameProvider =
               (match renameProvider_value with
               | None -> None
               | Some v -> v)
           ; foldingRangeProvider =
               (match foldingRangeProvider_value with
               | None -> None
               | Some v -> v)
           ; executeCommandProvider =
               (match executeCommandProvider_value with
               | None -> None
               | Some v -> v)
           ; selectionRangeProvider =
               (match selectionRangeProvider_value with
               | None -> None
               | Some v -> v)
           ; workspaceSymbolProvider =
               (match workspaceSymbolProvider_value with
               | None -> None
               | Some v -> v)
           ; workspace =
               (match workspace_value with
               | None -> None
               | Some v -> v)
           ; experimental = experimental_value
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocumentSync = v_textDocumentSync
       ; completionProvider = v_completionProvider
       ; hoverProvider = v_hoverProvider
       ; signatureHelpProvider = v_signatureHelpProvider
       ; declarationProvider = v_declarationProvider
       ; definitionProvider = v_definitionProvider
       ; typeDefinitionProvider = v_typeDefinitionProvider
       ; implementationProvider = v_implementationProvider
       ; referencesProvider = v_referencesProvider
       ; documentHighlightProvider = v_documentHighlightProvider
       ; documentSymbolProvider = v_documentSymbolProvider
       ; codeActionProvider = v_codeActionProvider
       ; codeLensProvider = v_codeLensProvider
       ; documentLinkProvider = v_documentLinkProvider
       ; colorProvider = v_colorProvider
       ; documentFormattingProvider = v_documentFormattingProvider
       ; documentRangeFormattingProvider = v_documentRangeFormattingProvider
       ; documentOnTypeFormattingProvider = v_documentOnTypeFormattingProvider
       ; renameProvider = v_renameProvider
       ; foldingRangeProvider = v_foldingRangeProvider
       ; executeCommandProvider = v_executeCommandProvider
       ; selectionRangeProvider = v_selectionRangeProvider
       ; workspaceSymbolProvider = v_workspaceSymbolProvider
       ; workspace = v_workspace
       ; experimental = v_experimental
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         match v_experimental with
         | None -> bnds
         | Some v ->
           let arg = Json.yojson_of_t v in
           let bnd = ("experimental", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workspace then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_workspace) v_workspace
           in
           let bnd = ("workspace", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workspaceSymbolProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workspaceSymbolProvider
           in
           let bnd = ("workspaceSymbolProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_selectionRangeProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_selectionRangeProvider)
               v_selectionRangeProvider
           in
           let bnd = ("selectionRangeProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_executeCommandProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t ExecuteCommandOptions.yojson_of_t)
               v_executeCommandProvider
           in
           let bnd = ("executeCommandProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_foldingRangeProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_foldingRangeProvider)
               v_foldingRangeProvider
           in
           let bnd = ("foldingRangeProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_renameProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_renameProvider)
               v_renameProvider
           in
           let bnd = ("renameProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentOnTypeFormattingProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                DocumentOnTypeFormattingOptions.yojson_of_t)
               v_documentOnTypeFormattingProvider
           in
           let bnd = ("documentOnTypeFormattingProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentRangeFormattingProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                yojson_of_documentRangeFormattingProvider)
               v_documentRangeFormattingProvider
           in
           let bnd = ("documentRangeFormattingProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentFormattingProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                yojson_of_documentFormattingProvider)
               v_documentFormattingProvider
           in
           let bnd = ("documentFormattingProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_colorProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_colorProvider)
               v_colorProvider
           in
           let bnd = ("colorProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentLinkProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentLinkOptions.yojson_of_t)
               v_documentLinkProvider
           in
           let bnd = ("documentLinkProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_codeLensProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t CodeLensOptions.yojson_of_t)
               v_codeLensProvider
           in
           let bnd = ("codeLensProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_codeActionProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_codeActionProvider)
               v_codeActionProvider
           in
           let bnd = ("codeActionProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSymbolProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_documentSymbolProvider)
               v_documentSymbolProvider
           in
           let bnd = ("documentSymbolProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentHighlightProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                yojson_of_documentHighlightProvider)
               v_documentHighlightProvider
           in
           let bnd = ("documentHighlightProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_referencesProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_referencesProvider)
               v_referencesProvider
           in
           let bnd = ("referencesProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_implementationProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_implementationProvider)
               v_implementationProvider
           in
           let bnd = ("implementationProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_typeDefinitionProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_typeDefinitionProvider)
               v_typeDefinitionProvider
           in
           let bnd = ("typeDefinitionProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_definitionProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_definitionProvider)
               v_definitionProvider
           in
           let bnd = ("definitionProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_declarationProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_declarationProvider)
               v_declarationProvider
           in
           let bnd = ("declarationProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_signatureHelpProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t SignatureHelpOptions.yojson_of_t)
               v_signatureHelpProvider
           in
           let bnd = ("signatureHelpProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_hoverProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_hoverProvider)
               v_hoverProvider
           in
           let bnd = ("hoverProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_completionProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t CompletionOptions.yojson_of_t)
               v_completionProvider
           in
           let bnd = ("completionProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_textDocumentSync then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_textDocumentSync)
               v_textDocumentSync
           in
           let bnd = ("textDocumentSync", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(textDocumentSync : textDocumentSync option)
      ?(completionProvider : CompletionOptions.t option)
      ?(hoverProvider : hoverProvider option)
      ?(signatureHelpProvider : SignatureHelpOptions.t option)
      ?(declarationProvider : declarationProvider option)
      ?(definitionProvider : definitionProvider option)
      ?(typeDefinitionProvider : typeDefinitionProvider option)
      ?(implementationProvider : implementationProvider option)
      ?(referencesProvider : referencesProvider option)
      ?(documentHighlightProvider : documentHighlightProvider option)
      ?(documentSymbolProvider : documentSymbolProvider option)
      ?(codeActionProvider : codeActionProvider option)
      ?(codeLensProvider : CodeLensOptions.t option)
      ?(documentLinkProvider : DocumentLinkOptions.t option)
      ?(colorProvider : colorProvider option)
      ?(documentFormattingProvider : documentFormattingProvider option)
      ?(documentRangeFormattingProvider :
         documentRangeFormattingProvider option)
      ?(documentOnTypeFormattingProvider :
         DocumentOnTypeFormattingOptions.t option)
      ?(renameProvider : renameProvider option)
      ?(foldingRangeProvider : foldingRangeProvider option)
      ?(executeCommandProvider : ExecuteCommandOptions.t option)
      ?(selectionRangeProvider : selectionRangeProvider option)
      ?(workspaceSymbolProvider : bool option) ?(workspace : workspace option)
      ?(experimental : Json.t option) (() : unit) : t =
    { textDocumentSync
    ; completionProvider
    ; hoverProvider
    ; signatureHelpProvider
    ; declarationProvider
    ; definitionProvider
    ; typeDefinitionProvider
    ; implementationProvider
    ; referencesProvider
    ; documentHighlightProvider
    ; documentSymbolProvider
    ; codeActionProvider
    ; codeLensProvider
    ; documentLinkProvider
    ; colorProvider
    ; documentFormattingProvider
    ; documentRangeFormattingProvider
    ; documentOnTypeFormattingProvider
    ; renameProvider
    ; foldingRangeProvider
    ; executeCommandProvider
    ; selectionRangeProvider
    ; workspaceSymbolProvider
    ; workspace
    ; experimental
    }
end

module InitializeResult = struct
  type serverInfo =
    { name : string
    ; version : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : serverInfo) -> ()

  let serverInfo_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.InitializeResult.serverInfo" in
     function
     | `Assoc field_yojsons as yojson -> (
       let name_field = ref None
       and version_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "name" -> (
             match Ppx_yojson_conv_lib.( ! ) name_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               name_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "version" -> (
             match Ppx_yojson_conv_lib.( ! ) version_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               version_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) name_field
             , Ppx_yojson_conv_lib.( ! ) version_field )
           with
           | Some name_value, version_value ->
             { name = name_value
             ; version =
                 (match version_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) name_field)
                     None
                 , "name" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> serverInfo)

  let _ = serverInfo_of_yojson

  let yojson_of_serverInfo =
    (function
     | { name = v_name; version = v_version } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_version then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_version
           in
           let bnd = ("version", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_name in
         ("name", arg) :: bnds
       in
       `Assoc bnds
      : serverInfo -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_serverInfo

  [@@@end]

  let create_serverInfo ~(name : string) ?(version : string option) (() : unit)
      : serverInfo =
    { name; version }

  type t =
    { capabilities : ServerCapabilities.t
    ; serverInfo : serverInfo Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.InitializeResult.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let capabilities_field = ref None
       and serverInfo_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "capabilities" -> (
             match Ppx_yojson_conv_lib.( ! ) capabilities_field with
             | None ->
               let fvalue = ServerCapabilities.t_of_yojson _field_yojson in
               capabilities_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "serverInfo" -> (
             match Ppx_yojson_conv_lib.( ! ) serverInfo_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson serverInfo_of_yojson
                   _field_yojson
               in
               serverInfo_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) capabilities_field
             , Ppx_yojson_conv_lib.( ! ) serverInfo_field )
           with
           | Some capabilities_value, serverInfo_value ->
             { capabilities = capabilities_value
             ; serverInfo =
                 (match serverInfo_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) capabilities_field)
                     None
                 , "capabilities" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { capabilities = v_capabilities; serverInfo = v_serverInfo } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_serverInfo then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_serverInfo)
               v_serverInfo
           in
           let bnd = ("serverInfo", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = ServerCapabilities.yojson_of_t v_capabilities in
         ("capabilities", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(capabilities : ServerCapabilities.t)
      ?(serverInfo : serverInfo option) (() : unit) : t =
    { capabilities; serverInfo }
end

module LocationLink = struct
  type t =
    { originSelectionRange : Range.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; targetUri : DocumentUri.t
    ; targetRange : Range.t
    ; targetSelectionRange : Range.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.LocationLink.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let originSelectionRange_field = ref None
       and targetUri_field = ref None
       and targetRange_field = ref None
       and targetSelectionRange_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "originSelectionRange" -> (
             match Ppx_yojson_conv_lib.( ! ) originSelectionRange_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson Range.t_of_yojson
                   _field_yojson
               in
               originSelectionRange_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "targetUri" -> (
             match Ppx_yojson_conv_lib.( ! ) targetUri_field with
             | None ->
               let fvalue = DocumentUri.t_of_yojson _field_yojson in
               targetUri_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "targetRange" -> (
             match Ppx_yojson_conv_lib.( ! ) targetRange_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               targetRange_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "targetSelectionRange" -> (
             match Ppx_yojson_conv_lib.( ! ) targetSelectionRange_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               targetSelectionRange_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) originSelectionRange_field
             , Ppx_yojson_conv_lib.( ! ) targetUri_field
             , Ppx_yojson_conv_lib.( ! ) targetRange_field
             , Ppx_yojson_conv_lib.( ! ) targetSelectionRange_field )
           with
           | ( originSelectionRange_value
             , Some targetUri_value
             , Some targetRange_value
             , Some targetSelectionRange_value ) ->
             { originSelectionRange =
                 (match originSelectionRange_value with
                 | None -> None
                 | Some v -> v)
             ; targetUri = targetUri_value
             ; targetRange = targetRange_value
             ; targetSelectionRange = targetSelectionRange_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) targetUri_field)
                     None
                 , "targetUri" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) targetRange_field)
                     None
                 , "targetRange" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) targetSelectionRange_field)
                     None
                 , "targetSelectionRange" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { originSelectionRange = v_originSelectionRange
       ; targetUri = v_targetUri
       ; targetRange = v_targetRange
       ; targetSelectionRange = v_targetSelectionRange
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Range.yojson_of_t v_targetSelectionRange in
         ("targetSelectionRange", arg) :: bnds
       in
       let bnds =
         let arg = Range.yojson_of_t v_targetRange in
         ("targetRange", arg) :: bnds
       in
       let bnds =
         let arg = DocumentUri.yojson_of_t v_targetUri in
         ("targetUri", arg) :: bnds
       in
       let bnds =
         if None = v_originSelectionRange then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t Range.yojson_of_t)
               v_originSelectionRange
           in
           let bnd = ("originSelectionRange", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(originSelectionRange : Range.t option)
      ~(targetUri : DocumentUri.t) ~(targetRange : Range.t)
      ~(targetSelectionRange : Range.t) (() : unit) : t =
    { originSelectionRange; targetUri; targetRange; targetSelectionRange }
end

module LogMessageParams = struct
  type t =
    { type_ : int [@key "type"]
    ; message : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.LogMessageParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let type__field = ref None
       and message_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "type" -> (
             match Ppx_yojson_conv_lib.( ! ) type__field with
             | None ->
               let fvalue = int_of_yojson _field_yojson in
               type__field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "message" -> (
             match Ppx_yojson_conv_lib.( ! ) message_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               message_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) type__field
             , Ppx_yojson_conv_lib.( ! ) message_field )
           with
           | Some type__value, Some message_value ->
             { type_ = type__value; message = message_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) type__field)
                     None
                 , "type_" )
               ; ( Ppx_yojson_conv_lib.poly_equal
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
     | { type_ = v_type_; message = v_message } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_message in
         ("message", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_int v_type_ in
         ("type", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(type_ : int) ~(message : string) : t = { type_; message }
end

module MessageActionItem = struct
  type t = { title : string }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.MessageActionItem.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let title_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "title" -> (
             match Ppx_yojson_conv_lib.( ! ) title_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               title_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) title_field with
           | Some title_value -> { title = title_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) title_field)
                     None
                 , "title" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { title = v_title } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_title in
         ("title", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(title : string) : t = { title }
end

module MessageType = struct
  type t =
    | Error
    | Warning
    | Info
    | Log

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Error -> `Int 1
    | Warning -> `Int 2
    | Info -> `Int 3
    | Log -> `Int 4

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Error
    | `Int 2 -> Warning
    | `Int 3 -> Info
    | `Int 4 -> Log
    | _ -> Json.error "t" json
end

module ParameterInformation = struct
  type label =
    [ `String of string
    | `Offset of int * int
    ]

  let label_of_yojson (json : Json.t) : label =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union "label"
        [ (fun json -> `Offset (Json.Of.int_pair json)) ]
        json

  let yojson_of_label (label : label) : Json.t =
    match label with
    | `String j -> `String j
    | `Offset s -> Json.To.int_pair s

  type documentation =
    [ `String of string
    | `MarkupContent of MarkupContent.t
    ]

  let documentation_of_yojson (json : Json.t) : documentation =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union "documentation"
        [ (fun json -> `MarkupContent (MarkupContent.t_of_yojson json)) ]
        json

  let yojson_of_documentation (documentation : documentation) : Json.t =
    match documentation with
    | `String j -> `String j
    | `MarkupContent s -> MarkupContent.yojson_of_t s

  type t =
    { label : label
    ; documentation : documentation Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ParameterInformation.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let label_field = ref None
       and documentation_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "label" -> (
             match Ppx_yojson_conv_lib.( ! ) label_field with
             | None ->
               let fvalue = label_of_yojson _field_yojson in
               label_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentation" -> (
             match Ppx_yojson_conv_lib.( ! ) documentation_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson documentation_of_yojson
                   _field_yojson
               in
               documentation_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) label_field
             , Ppx_yojson_conv_lib.( ! ) documentation_field )
           with
           | Some label_value, documentation_value ->
             { label = label_value
             ; documentation =
                 (match documentation_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) label_field)
                     None
                 , "label" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { label = v_label; documentation = v_documentation } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_documentation then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_documentation)
               v_documentation
           in
           let bnd = ("documentation", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_label v_label in
         ("label", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(label : label) ?(documentation : documentation option)
      (() : unit) : t =
    { label; documentation }
end

module PrepareRenameParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.PrepareRenameParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and position_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "position" -> (
             match Ppx_yojson_conv_lib.( ! ) position_field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               position_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) position_field )
           with
           | Some textDocument_value, Some position_value ->
             { textDocument = textDocument_value; position = position_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) position_field)
                     None
                 , "position" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; position = v_position } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Position.yojson_of_t v_position in
         ("position", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(position : Position.t)
      : t =
    { textDocument; position }
end

module ProgressParams = struct
  type t =
    { token : ProgressToken.t
    ; value : Json.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ProgressParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let token_field = ref None
       and value_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "token" -> (
             match Ppx_yojson_conv_lib.( ! ) token_field with
             | None ->
               let fvalue = ProgressToken.t_of_yojson _field_yojson in
               token_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "value" -> (
             match Ppx_yojson_conv_lib.( ! ) value_field with
             | None ->
               let fvalue = Json.t_of_yojson _field_yojson in
               value_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) token_field
             , Ppx_yojson_conv_lib.( ! ) value_field )
           with
           | Some token_value, Some value_value ->
             { token = token_value; value = value_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) token_field)
                     None
                 , "token" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) value_field)
                     None
                 , "value" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { token = v_token; value = v_value } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Json.yojson_of_t v_value in
         ("value", arg) :: bnds
       in
       let bnds =
         let arg = ProgressToken.yojson_of_t v_token in
         ("token", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(token : ProgressToken.t) ~(value : Json.t) : t = { token; value }
end

module PublishDiagnosticsParams = struct
  type t =
    { uri : DocumentUri.t
    ; version : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; diagnostics : Diagnostic.t list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.PublishDiagnosticsParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let uri_field = ref None
       and version_field = ref None
       and diagnostics_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "uri" -> (
             match Ppx_yojson_conv_lib.( ! ) uri_field with
             | None ->
               let fvalue = DocumentUri.t_of_yojson _field_yojson in
               uri_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "version" -> (
             match Ppx_yojson_conv_lib.( ! ) version_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson int_of_yojson _field_yojson
               in
               version_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "diagnostics" -> (
             match Ppx_yojson_conv_lib.( ! ) diagnostics_field with
             | None ->
               let fvalue =
                 list_of_yojson Diagnostic.t_of_yojson _field_yojson
               in
               diagnostics_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) uri_field
             , Ppx_yojson_conv_lib.( ! ) version_field
             , Ppx_yojson_conv_lib.( ! ) diagnostics_field )
           with
           | Some uri_value, version_value, Some diagnostics_value ->
             { uri = uri_value
             ; version =
                 (match version_value with
                 | None -> None
                 | Some v -> v)
             ; diagnostics = diagnostics_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) uri_field)
                     None
                 , "uri" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) diagnostics_field)
                     None
                 , "diagnostics" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { uri = v_uri; version = v_version; diagnostics = v_diagnostics } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list Diagnostic.yojson_of_t v_diagnostics in
         ("diagnostics", arg) :: bnds
       in
       let bnds =
         if None = v_version then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_int) v_version
           in
           let bnd = ("version", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = DocumentUri.yojson_of_t v_uri in
         ("uri", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(uri : DocumentUri.t) ?(version : int option)
      ~(diagnostics : Diagnostic.t list) (() : unit) : t =
    { uri; version; diagnostics }
end

module ReferenceContext = struct
  type t = { includeDeclaration : bool }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ReferenceContext.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let includeDeclaration_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "includeDeclaration" -> (
             match Ppx_yojson_conv_lib.( ! ) includeDeclaration_field with
             | None ->
               let fvalue = bool_of_yojson _field_yojson in
               includeDeclaration_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) includeDeclaration_field with
           | Some includeDeclaration_value ->
             { includeDeclaration = includeDeclaration_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) includeDeclaration_field)
                     None
                 , "includeDeclaration" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { includeDeclaration = v_includeDeclaration } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_includeDeclaration in
         ("includeDeclaration", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(includeDeclaration : bool) : t = { includeDeclaration }
end

module ReferenceParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; context : ReferenceContext.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ReferenceParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and position_field = ref None
       and context_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "position" -> (
             match Ppx_yojson_conv_lib.( ! ) position_field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               position_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "context" -> (
             match Ppx_yojson_conv_lib.( ! ) context_field with
             | None ->
               let fvalue = ReferenceContext.t_of_yojson _field_yojson in
               context_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) position_field
             , Ppx_yojson_conv_lib.( ! ) context_field )
           with
           | Some textDocument_value, Some position_value, Some context_value ->
             { textDocument = textDocument_value
             ; position = position_value
             ; context = context_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) position_field)
                     None
                 , "position" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) context_field)
                     None
                 , "context" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument
       ; position = v_position
       ; context = v_context
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = ReferenceContext.yojson_of_t v_context in
         ("context", arg) :: bnds
       in
       let bnds =
         let arg = Position.yojson_of_t v_position in
         ("position", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(position : Position.t)
      ~(context : ReferenceContext.t) : t =
    { textDocument; position; context }
end

module ReferenceRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ReferenceRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let documentSelector_value, workDoneProgress_value =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option) (() : unit) : t =
    { documentSelector; workDoneProgress }
end

module Registration = struct
  type t =
    { id : string
    ; method_ : string [@key "method"]
    ; registerOptions : Json.t option [@yojson.option]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.Registration.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let id_field = ref None
       and method__field = ref None
       and registerOptions_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "id" -> (
             match Ppx_yojson_conv_lib.( ! ) id_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               id_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "method" -> (
             match Ppx_yojson_conv_lib.( ! ) method__field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               method__field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "registerOptions" -> (
             match Ppx_yojson_conv_lib.( ! ) registerOptions_field with
             | None ->
               let fvalue = Json.t_of_yojson _field_yojson in
               registerOptions_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) id_field
             , Ppx_yojson_conv_lib.( ! ) method__field
             , Ppx_yojson_conv_lib.( ! ) registerOptions_field )
           with
           | Some id_value, Some method__value, registerOptions_value ->
             { id = id_value
             ; method_ = method__value
             ; registerOptions = registerOptions_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) id_field)
                     None
                 , "id" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) method__field)
                     None
                 , "method_" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { id = v_id; method_ = v_method_; registerOptions = v_registerOptions }
       ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         match v_registerOptions with
         | None -> bnds
         | Some v ->
           let arg = Json.yojson_of_t v in
           let bnd = ("registerOptions", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_method_ in
         ("method", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_id in
         ("id", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(id : string) ~(method_ : string)
      ?(registerOptions : Json.t option) (() : unit) : t =
    { id; method_; registerOptions }
end

module RegistrationParams = struct
  type t = { registrations : Registration.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.RegistrationParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let registrations_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "registrations" -> (
             match Ppx_yojson_conv_lib.( ! ) registrations_field with
             | None ->
               let fvalue =
                 list_of_yojson Registration.t_of_yojson _field_yojson
               in
               registrations_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) registrations_field with
           | Some registrations_value -> { registrations = registrations_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) registrations_field)
                     None
                 , "registrations" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { registrations = v_registrations } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list Registration.yojson_of_t v_registrations in
         ("registrations", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(registrations : Registration.t list) : t = { registrations }
end

module RenameParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; newName : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.RenameParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and position_field = ref None
       and newName_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "position" -> (
             match Ppx_yojson_conv_lib.( ! ) position_field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               position_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "newName" -> (
             match Ppx_yojson_conv_lib.( ! ) newName_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               newName_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) position_field
             , Ppx_yojson_conv_lib.( ! ) newName_field )
           with
           | Some textDocument_value, Some position_value, Some newName_value ->
             { textDocument = textDocument_value
             ; position = position_value
             ; newName = newName_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) position_field)
                     None
                 , "position" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) newName_field)
                     None
                 , "newName" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument
       ; position = v_position
       ; newName = v_newName
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_newName in
         ("newName", arg) :: bnds
       in
       let bnds =
         let arg = Position.yojson_of_t v_position in
         ("position", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(position : Position.t)
      ~(newName : string) : t =
    { textDocument; position; newName }
end

module RenameRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; prepareProvider : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.RenameRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and prepareProvider_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "prepareProvider" -> (
             match Ppx_yojson_conv_lib.( ! ) prepareProvider_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               prepareProvider_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( documentSelector_value
               , workDoneProgress_value
               , prepareProvider_value ) =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) prepareProvider_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; prepareProvider =
               (match prepareProvider_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       ; prepareProvider = v_prepareProvider
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_prepareProvider then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_prepareProvider
           in
           let bnd = ("prepareProvider", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option) ?(prepareProvider : bool option)
      (() : unit) : t =
    { documentSelector; workDoneProgress; prepareProvider }
end

module SelectionRange = struct
  type t =
    { range : Range.t
    ; parent : t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let rec t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.SelectionRange.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let range_field = ref None
       and parent_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "range" -> (
             match Ppx_yojson_conv_lib.( ! ) range_field with
             | None ->
               let fvalue = Range.t_of_yojson _field_yojson in
               range_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "parent" -> (
             match Ppx_yojson_conv_lib.( ! ) parent_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson t_of_yojson _field_yojson
               in
               parent_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) range_field
             , Ppx_yojson_conv_lib.( ! ) parent_field )
           with
           | Some range_value, parent_value ->
             { range = range_value
             ; parent =
                 (match parent_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) range_field)
                     None
                 , "range" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let rec yojson_of_t =
    (function
     | { range = v_range; parent = v_parent } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_parent then
           bnds
         else
           let arg = (Json.Nullable_option.yojson_of_t yojson_of_t) v_parent in
           let bnd = ("parent", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = Range.yojson_of_t v_range in
         ("range", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(range : Range.t) ?(parent : t option) (() : unit) : t =
    { range; parent }
end

module SelectionRangeParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; positions : Position.t list
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.SelectionRangeParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and positions_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "positions" -> (
             match Ppx_yojson_conv_lib.( ! ) positions_field with
             | None ->
               let fvalue = list_of_yojson Position.t_of_yojson _field_yojson in
               positions_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) positions_field )
           with
           | Some textDocument_value, Some positions_value ->
             { textDocument = textDocument_value; positions = positions_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) positions_field)
                     None
                 , "positions" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; positions = v_positions } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list Position.yojson_of_t v_positions in
         ("positions", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t)
      ~(positions : Position.t list) : t =
    { textDocument; positions }
end

module ShowMessageParams = struct
  type t =
    { type_ : MessageType.t [@key "type"]
    ; message : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ShowMessageParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let type__field = ref None
       and message_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "type" -> (
             match Ppx_yojson_conv_lib.( ! ) type__field with
             | None ->
               let fvalue = MessageType.t_of_yojson _field_yojson in
               type__field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "message" -> (
             match Ppx_yojson_conv_lib.( ! ) message_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               message_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) type__field
             , Ppx_yojson_conv_lib.( ! ) message_field )
           with
           | Some type__value, Some message_value ->
             { type_ = type__value; message = message_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) type__field)
                     None
                 , "type_" )
               ; ( Ppx_yojson_conv_lib.poly_equal
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
     | { type_ = v_type_; message = v_message } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_message in
         ("message", arg) :: bnds
       in
       let bnds =
         let arg = MessageType.yojson_of_t v_type_ in
         ("type", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(type_ : MessageType.t) ~(message : string) : t =
    { type_; message }
end

module ShowMessageRequestParams = struct
  type t =
    { type_ : int [@key "type"]
    ; message : string
    ; actions : MessageActionItem.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.ShowMessageRequestParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let type__field = ref None
       and message_field = ref None
       and actions_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "type" -> (
             match Ppx_yojson_conv_lib.( ! ) type__field with
             | None ->
               let fvalue = int_of_yojson _field_yojson in
               type__field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "message" -> (
             match Ppx_yojson_conv_lib.( ! ) message_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               message_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "actions" -> (
             match Ppx_yojson_conv_lib.( ! ) actions_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson MessageActionItem.t_of_yojson)
                   _field_yojson
               in
               actions_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) type__field
             , Ppx_yojson_conv_lib.( ! ) message_field
             , Ppx_yojson_conv_lib.( ! ) actions_field )
           with
           | Some type__value, Some message_value, actions_value ->
             { type_ = type__value
             ; message = message_value
             ; actions =
                 (match actions_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) type__field)
                     None
                 , "type_" )
               ; ( Ppx_yojson_conv_lib.poly_equal
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
     | { type_ = v_type_; message = v_message; actions = v_actions } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_actions then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list MessageActionItem.yojson_of_t))
               v_actions
           in
           let bnd = ("actions", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_message in
         ("message", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_int v_type_ in
         ("type", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(type_ : int) ~(message : string)
      ?(actions : MessageActionItem.t list option) (() : unit) : t =
    { type_; message; actions }
end

module SignatureInformation = struct
  type documentation =
    [ `String of string
    | `MarkupContent of MarkupContent.t
    ]

  let documentation_of_yojson (json : Json.t) : documentation =
    match json with
    | `String j -> `String j
    | _ ->
      Json.Of.untagged_union "documentation"
        [ (fun json -> `MarkupContent (MarkupContent.t_of_yojson json)) ]
        json

  let yojson_of_documentation (documentation : documentation) : Json.t =
    match documentation with
    | `String j -> `String j
    | `MarkupContent s -> MarkupContent.yojson_of_t s

  type t =
    { label : string
    ; documentation : documentation Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; parameters : ParameterInformation.t list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.SignatureInformation.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let label_field = ref None
       and documentation_field = ref None
       and parameters_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "label" -> (
             match Ppx_yojson_conv_lib.( ! ) label_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               label_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "documentation" -> (
             match Ppx_yojson_conv_lib.( ! ) documentation_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson documentation_of_yojson
                   _field_yojson
               in
               documentation_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "parameters" -> (
             match Ppx_yojson_conv_lib.( ! ) parameters_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson ParameterInformation.t_of_yojson)
                   _field_yojson
               in
               parameters_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) label_field
             , Ppx_yojson_conv_lib.( ! ) documentation_field
             , Ppx_yojson_conv_lib.( ! ) parameters_field )
           with
           | Some label_value, documentation_value, parameters_value ->
             { label = label_value
             ; documentation =
                 (match documentation_value with
                 | None -> None
                 | Some v -> v)
             ; parameters =
                 (match parameters_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) label_field)
                     None
                 , "label" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { label = v_label
       ; documentation = v_documentation
       ; parameters = v_parameters
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_parameters then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list ParameterInformation.yojson_of_t))
               v_parameters
           in
           let bnd = ("parameters", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentation then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_documentation)
               v_documentation
           in
           let bnd = ("documentation", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_label in
         ("label", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(label : string) ?(documentation : documentation option)
      ?(parameters : ParameterInformation.t list option) (() : unit) : t =
    { label; documentation; parameters }
end

module SignatureHelp = struct
  type t =
    { signatures : SignatureInformation.t list
    ; activeSignature : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; activeParameter : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.SignatureHelp.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let signatures_field = ref None
       and activeSignature_field = ref None
       and activeParameter_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "signatures" -> (
             match Ppx_yojson_conv_lib.( ! ) signatures_field with
             | None ->
               let fvalue =
                 list_of_yojson SignatureInformation.t_of_yojson _field_yojson
               in
               signatures_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "activeSignature" -> (
             match Ppx_yojson_conv_lib.( ! ) activeSignature_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson int_of_yojson _field_yojson
               in
               activeSignature_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "activeParameter" -> (
             match Ppx_yojson_conv_lib.( ! ) activeParameter_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson int_of_yojson _field_yojson
               in
               activeParameter_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) signatures_field
             , Ppx_yojson_conv_lib.( ! ) activeSignature_field
             , Ppx_yojson_conv_lib.( ! ) activeParameter_field )
           with
           | Some signatures_value, activeSignature_value, activeParameter_value
             ->
             { signatures = signatures_value
             ; activeSignature =
                 (match activeSignature_value with
                 | None -> None
                 | Some v -> v)
             ; activeParameter =
                 (match activeParameter_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) signatures_field)
                     None
                 , "signatures" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { signatures = v_signatures
       ; activeSignature = v_activeSignature
       ; activeParameter = v_activeParameter
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_activeParameter then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_int) v_activeParameter
           in
           let bnd = ("activeParameter", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_activeSignature then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_int) v_activeSignature
           in
           let bnd = ("activeSignature", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg =
           yojson_of_list SignatureInformation.yojson_of_t v_signatures
         in
         ("signatures", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(signatures : SignatureInformation.t list)
      ?(activeSignature : int option) ?(activeParameter : int option)
      (() : unit) : t =
    { signatures; activeSignature; activeParameter }
end

module SignatureHelpTriggerKind = struct
  type t =
    | Invoked
    | TriggerCharacter
    | ContentChange

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Invoked -> `Int 1
    | TriggerCharacter -> `Int 2
    | ContentChange -> `Int 3

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Invoked
    | `Int 2 -> TriggerCharacter
    | `Int 3 -> ContentChange
    | _ -> Json.error "t" json
end

module SignatureHelpContext = struct
  type t =
    { triggerKind : SignatureHelpTriggerKind.t
    ; triggerCharacter : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; isRetrigger : bool
    ; activeSignatureHelp : SignatureHelp.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.SignatureHelpContext.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let triggerKind_field = ref None
       and triggerCharacter_field = ref None
       and isRetrigger_field = ref None
       and activeSignatureHelp_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "triggerKind" -> (
             match Ppx_yojson_conv_lib.( ! ) triggerKind_field with
             | None ->
               let fvalue =
                 SignatureHelpTriggerKind.t_of_yojson _field_yojson
               in
               triggerKind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "triggerCharacter" -> (
             match Ppx_yojson_conv_lib.( ! ) triggerCharacter_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               triggerCharacter_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "isRetrigger" -> (
             match Ppx_yojson_conv_lib.( ! ) isRetrigger_field with
             | None ->
               let fvalue = bool_of_yojson _field_yojson in
               isRetrigger_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "activeSignatureHelp" -> (
             match Ppx_yojson_conv_lib.( ! ) activeSignatureHelp_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson SignatureHelp.t_of_yojson
                   _field_yojson
               in
               activeSignatureHelp_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) triggerKind_field
             , Ppx_yojson_conv_lib.( ! ) triggerCharacter_field
             , Ppx_yojson_conv_lib.( ! ) isRetrigger_field
             , Ppx_yojson_conv_lib.( ! ) activeSignatureHelp_field )
           with
           | ( Some triggerKind_value
             , triggerCharacter_value
             , Some isRetrigger_value
             , activeSignatureHelp_value ) ->
             { triggerKind = triggerKind_value
             ; triggerCharacter =
                 (match triggerCharacter_value with
                 | None -> None
                 | Some v -> v)
             ; isRetrigger = isRetrigger_value
             ; activeSignatureHelp =
                 (match activeSignatureHelp_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) triggerKind_field)
                     None
                 , "triggerKind" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) isRetrigger_field)
                     None
                 , "isRetrigger" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { triggerKind = v_triggerKind
       ; triggerCharacter = v_triggerCharacter
       ; isRetrigger = v_isRetrigger
       ; activeSignatureHelp = v_activeSignatureHelp
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_activeSignatureHelp then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t SignatureHelp.yojson_of_t)
               v_activeSignatureHelp
           in
           let bnd = ("activeSignatureHelp", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_bool v_isRetrigger in
         ("isRetrigger", arg) :: bnds
       in
       let bnds =
         if None = v_triggerCharacter then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string)
               v_triggerCharacter
           in
           let bnd = ("triggerCharacter", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = SignatureHelpTriggerKind.yojson_of_t v_triggerKind in
         ("triggerKind", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(triggerKind : SignatureHelpTriggerKind.t)
      ?(triggerCharacter : string option) ~(isRetrigger : bool)
      ?(activeSignatureHelp : SignatureHelp.t option) (() : unit) : t =
    { triggerKind; triggerCharacter; isRetrigger; activeSignatureHelp }
end

module SignatureHelpParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; context : SignatureHelpContext.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.SignatureHelpParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and position_field = ref None
       and context_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "position" -> (
             match Ppx_yojson_conv_lib.( ! ) position_field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               position_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "context" -> (
             match Ppx_yojson_conv_lib.( ! ) context_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   SignatureHelpContext.t_of_yojson _field_yojson
               in
               context_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) position_field
             , Ppx_yojson_conv_lib.( ! ) context_field )
           with
           | Some textDocument_value, Some position_value, context_value ->
             { textDocument = textDocument_value
             ; position = position_value
             ; context =
                 (match context_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) position_field)
                     None
                 , "position" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument
       ; position = v_position
       ; context = v_context
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_context then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t SignatureHelpContext.yojson_of_t)
               v_context
           in
           let bnd = ("context", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = Position.yojson_of_t v_position in
         ("position", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(position : Position.t)
      ?(context : SignatureHelpContext.t option) (() : unit) : t =
    { textDocument; position; context }
end

module SignatureHelpRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; triggerCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; retriggerCharacters : string list Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.SignatureHelpRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and workDoneProgress_field = ref None
       and triggerCharacters_field = ref None
       and retriggerCharacters_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "triggerCharacters" -> (
             match Ppx_yojson_conv_lib.( ! ) triggerCharacters_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson string_of_yojson)
                   _field_yojson
               in
               triggerCharacters_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "retriggerCharacters" -> (
             match Ppx_yojson_conv_lib.( ! ) retriggerCharacters_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson
                   (list_of_yojson string_of_yojson)
                   _field_yojson
               in
               retriggerCharacters_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let ( documentSelector_value
               , workDoneProgress_value
               , triggerCharacters_value
               , retriggerCharacters_value ) =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
             , Ppx_yojson_conv_lib.( ! ) triggerCharacters_field
             , Ppx_yojson_conv_lib.( ! ) retriggerCharacters_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           ; triggerCharacters =
               (match triggerCharacters_value with
               | None -> None
               | Some v -> v)
           ; retriggerCharacters =
               (match retriggerCharacters_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector
       ; workDoneProgress = v_workDoneProgress
       ; triggerCharacters = v_triggerCharacters
       ; retriggerCharacters = v_retriggerCharacters
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_retriggerCharacters then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list yojson_of_string))
               v_retriggerCharacters
           in
           let bnd = ("retriggerCharacters", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_triggerCharacters then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t
                (yojson_of_list yojson_of_string))
               v_triggerCharacters
           in
           let bnd = ("triggerCharacters", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(workDoneProgress : bool option)
      ?(triggerCharacters : string list option)
      ?(retriggerCharacters : string list option) (() : unit) : t =
    { documentSelector
    ; workDoneProgress
    ; triggerCharacters
    ; retriggerCharacters
    }
end

module SymbolInformation = struct
  type t =
    { name : string
    ; kind : SymbolKind.t
    ; deprecated : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; location : Location.t
    ; containerName : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.SymbolInformation.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let name_field = ref None
       and kind_field = ref None
       and deprecated_field = ref None
       and location_field = ref None
       and containerName_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "name" -> (
             match Ppx_yojson_conv_lib.( ! ) name_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               name_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "kind" -> (
             match Ppx_yojson_conv_lib.( ! ) kind_field with
             | None ->
               let fvalue = SymbolKind.t_of_yojson _field_yojson in
               kind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "deprecated" -> (
             match Ppx_yojson_conv_lib.( ! ) deprecated_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               deprecated_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "location" -> (
             match Ppx_yojson_conv_lib.( ! ) location_field with
             | None ->
               let fvalue = Location.t_of_yojson _field_yojson in
               location_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "containerName" -> (
             match Ppx_yojson_conv_lib.( ! ) containerName_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               containerName_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) name_field
             , Ppx_yojson_conv_lib.( ! ) kind_field
             , Ppx_yojson_conv_lib.( ! ) deprecated_field
             , Ppx_yojson_conv_lib.( ! ) location_field
             , Ppx_yojson_conv_lib.( ! ) containerName_field )
           with
           | ( Some name_value
             , Some kind_value
             , deprecated_value
             , Some location_value
             , containerName_value ) ->
             { name = name_value
             ; kind = kind_value
             ; deprecated =
                 (match deprecated_value with
                 | None -> None
                 | Some v -> v)
             ; location = location_value
             ; containerName =
                 (match containerName_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) name_field)
                     None
                 , "name" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) kind_field)
                     None
                 , "kind" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) location_field)
                     None
                 , "location" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { name = v_name
       ; kind = v_kind
       ; deprecated = v_deprecated
       ; location = v_location
       ; containerName = v_containerName
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_containerName then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_containerName
           in
           let bnd = ("containerName", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = Location.yojson_of_t v_location in
         ("location", arg) :: bnds
       in
       let bnds =
         if None = v_deprecated then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_deprecated
           in
           let bnd = ("deprecated", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = SymbolKind.yojson_of_t v_kind in
         ("kind", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_name in
         ("name", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(name : string) ~(kind : SymbolKind.t) ?(deprecated : bool option)
      ~(location : Location.t) ?(containerName : string option) (() : unit) : t
      =
    { name; kind; deprecated; location; containerName }
end

module TextDocumentChangeRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; syncKind : TextDocumentSyncKind.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TextDocumentChangeRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and syncKind_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "syncKind" -> (
             match Ppx_yojson_conv_lib.( ! ) syncKind_field with
             | None ->
               let fvalue = TextDocumentSyncKind.t_of_yojson _field_yojson in
               syncKind_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) syncKind_field )
           with
           | documentSelector_value, Some syncKind_value ->
             { documentSelector =
                 (match documentSelector_value with
                 | None -> None
                 | Some v -> v)
             ; syncKind = syncKind_value
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) syncKind_field)
                     None
                 , "syncKind" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector; syncKind = v_syncKind } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = TextDocumentSyncKind.yojson_of_t v_syncKind in
         ("syncKind", arg) :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ~(syncKind : TextDocumentSyncKind.t) (() : unit) : t =
    { documentSelector; syncKind }
end

module TextDocumentSaveReason = struct
  type t =
    | Manual
    | AfterDelay
    | FocusOut

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Manual -> `Int 1
    | AfterDelay -> `Int 2
    | FocusOut -> `Int 3

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Manual
    | `Int 2 -> AfterDelay
    | `Int 3 -> FocusOut
    | _ -> Json.error "t" json
end

module TextDocumentSaveRegistrationOptions = struct
  type t =
    { documentSelector : DocumentSelector.t Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; includeText : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TextDocumentSaveRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let documentSelector_field = ref None
       and includeText_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "documentSelector" -> (
             match Ppx_yojson_conv_lib.( ! ) documentSelector_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson DocumentSelector.t_of_yojson
                   _field_yojson
               in
               documentSelector_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "includeText" -> (
             match Ppx_yojson_conv_lib.( ! ) includeText_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               includeText_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let documentSelector_value, includeText_value =
             ( Ppx_yojson_conv_lib.( ! ) documentSelector_field
             , Ppx_yojson_conv_lib.( ! ) includeText_field )
           in
           { documentSelector =
               (match documentSelector_value with
               | None -> None
               | Some v -> v)
           ; includeText =
               (match includeText_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { documentSelector = v_documentSelector; includeText = v_includeText } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_includeText then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_includeText
           in
           let bnd = ("includeText", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_documentSelector then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t DocumentSelector.yojson_of_t)
               v_documentSelector
           in
           let bnd = ("documentSelector", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(documentSelector : DocumentSelector.t option)
      ?(includeText : bool option) (() : unit) : t =
    { documentSelector; includeText }
end

module TypeDefinitionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.TypeDefinitionParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and position_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "position" -> (
             match Ppx_yojson_conv_lib.( ! ) position_field with
             | None ->
               let fvalue = Position.t_of_yojson _field_yojson in
               position_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) position_field )
           with
           | Some textDocument_value, Some position_value ->
             { textDocument = textDocument_value; position = position_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) position_field)
                     None
                 , "position" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; position = v_position } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = Position.yojson_of_t v_position in
         ("position", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(position : Position.t)
      : t =
    { textDocument; position }
end

module Unregistration = struct
  type t =
    { id : string
    ; method_ : string [@key "method"]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.Unregistration.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let id_field = ref None
       and method__field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "id" -> (
             match Ppx_yojson_conv_lib.( ! ) id_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               id_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "method" -> (
             match Ppx_yojson_conv_lib.( ! ) method__field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               method__field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) id_field
             , Ppx_yojson_conv_lib.( ! ) method__field )
           with
           | Some id_value, Some method__value ->
             { id = id_value; method_ = method__value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) id_field)
                     None
                 , "id" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) method__field)
                     None
                 , "method_" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { id = v_id; method_ = v_method_ } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_method_ in
         ("method", arg) :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_id in
         ("id", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(id : string) ~(method_ : string) : t = { id; method_ }
end

module UnregistrationParams = struct
  type t = { unregisterations : Unregistration.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.UnregistrationParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let unregisterations_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "unregisterations" -> (
             match Ppx_yojson_conv_lib.( ! ) unregisterations_field with
             | None ->
               let fvalue =
                 list_of_yojson Unregistration.t_of_yojson _field_yojson
               in
               unregisterations_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) unregisterations_field with
           | Some unregisterations_value ->
             { unregisterations = unregisterations_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) unregisterations_field)
                     None
                 , "unregisterations" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { unregisterations = v_unregisterations } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg =
           yojson_of_list Unregistration.yojson_of_t v_unregisterations
         in
         ("unregisterations", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(unregisterations : Unregistration.t list) : t =
    { unregisterations }
end

module WatchKind = struct
  type t =
    | Create
    | Change
    | Delete

  let yojson_of_t (t : t) : Json.t =
    match t with
    | Create -> `Int 1
    | Change -> `Int 2
    | Delete -> `Int 4

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Int 1 -> Create
    | `Int 2 -> Change
    | `Int 4 -> Delete
    | _ -> Json.error "t" json
end

module WillSaveTextDocumentParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; reason : int
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WillSaveTextDocumentParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let textDocument_field = ref None
       and reason_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "textDocument" -> (
             match Ppx_yojson_conv_lib.( ! ) textDocument_field with
             | None ->
               let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
               textDocument_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "reason" -> (
             match Ppx_yojson_conv_lib.( ! ) reason_field with
             | None ->
               let fvalue = int_of_yojson _field_yojson in
               reason_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) textDocument_field
             , Ppx_yojson_conv_lib.( ! ) reason_field )
           with
           | Some textDocument_value, Some reason_value ->
             { textDocument = textDocument_value; reason = reason_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                     None
                 , "textDocument" )
               ; ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) reason_field)
                     None
                 , "reason" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { textDocument = v_textDocument; reason = v_reason } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_int v_reason in
         ("reason", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(textDocument : TextDocumentIdentifier.t) ~(reason : int) : t =
    { textDocument; reason }
end

module WorkDoneProgressBegin = struct
  type t =
    { title : string
    ; cancellable : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; message : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; percentage : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkDoneProgressBegin.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let title_field = ref None
       and cancellable_field = ref None
       and message_field = ref None
       and percentage_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "title" -> (
             match Ppx_yojson_conv_lib.( ! ) title_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               title_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "cancellable" -> (
             match Ppx_yojson_conv_lib.( ! ) cancellable_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               cancellable_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "message" -> (
             match Ppx_yojson_conv_lib.( ! ) message_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               message_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "percentage" -> (
             match Ppx_yojson_conv_lib.( ! ) percentage_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson int_of_yojson _field_yojson
               in
               percentage_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match
             ( Ppx_yojson_conv_lib.( ! ) title_field
             , Ppx_yojson_conv_lib.( ! ) cancellable_field
             , Ppx_yojson_conv_lib.( ! ) message_field
             , Ppx_yojson_conv_lib.( ! ) percentage_field )
           with
           | ( Some title_value
             , cancellable_value
             , message_value
             , percentage_value ) ->
             { title = title_value
             ; cancellable =
                 (match cancellable_value with
                 | None -> None
                 | Some v -> v)
             ; message =
                 (match message_value with
                 | None -> None
                 | Some v -> v)
             ; percentage =
                 (match percentage_value with
                 | None -> None
                 | Some v -> v)
             }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) title_field)
                     None
                 , "title" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { title = v_title
       ; cancellable = v_cancellable
       ; message = v_message
       ; percentage = v_percentage
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_percentage then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_int) v_percentage
           in
           let bnd = ("percentage", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_message then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_message
           in
           let bnd = ("message", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_cancellable then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_cancellable
           in
           let bnd = ("cancellable", arg) in
           bnd :: bnds
       in
       let bnds =
         let arg = yojson_of_string v_title in
         ("title", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(title : string) ?(cancellable : bool option)
      ?(message : string option) ?(percentage : int option) (() : unit) : t =
    { title; cancellable; message; percentage }

  let yojson_of_t (t : t) : Json.t =
    Json.To.literal_field "kind" "begin" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "begin" t_of_yojson json
end

module WorkDoneProgressCancelParams = struct
  type t = { token : ProgressToken.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkDoneProgressCancelParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let token_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "token" -> (
             match Ppx_yojson_conv_lib.( ! ) token_field with
             | None ->
               let fvalue = ProgressToken.t_of_yojson _field_yojson in
               token_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) token_field with
           | Some token_value -> { token = token_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) token_field)
                     None
                 , "token" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { token = v_token } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = ProgressToken.yojson_of_t v_token in
         ("token", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(token : ProgressToken.t) : t = { token }
end

module WorkDoneProgressCreateParams = struct
  type t = { token : ProgressToken.t }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkDoneProgressCreateParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let token_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "token" -> (
             match Ppx_yojson_conv_lib.( ! ) token_field with
             | None ->
               let fvalue = ProgressToken.t_of_yojson _field_yojson in
               token_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) token_field with
           | Some token_value -> { token = token_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) token_field)
                     None
                 , "token" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { token = v_token } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = ProgressToken.yojson_of_t v_token in
         ("token", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(token : ProgressToken.t) : t = { token }
end

module WorkDoneProgressEnd = struct
  type t =
    { message : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkDoneProgressEnd.t" in
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
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               message_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let message_value = Ppx_yojson_conv_lib.( ! ) message_field in
           { message =
               (match message_value with
               | None -> None
               | Some v -> v)
           }))
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
         if None = v_message then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_message
           in
           let bnd = ("message", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(message : string option) (() : unit) : t = { message }

  let yojson_of_t (t : t) : Json.t =
    Json.To.literal_field "kind" "end" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "end" t_of_yojson json
end

module WorkDoneProgressReport = struct
  type t =
    { cancellable : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; message : string Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    ; percentage : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkDoneProgressReport.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let cancellable_field = ref None
       and message_field = ref None
       and percentage_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "cancellable" -> (
             match Ppx_yojson_conv_lib.( ! ) cancellable_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               cancellable_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "message" -> (
             match Ppx_yojson_conv_lib.( ! ) message_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson string_of_yojson _field_yojson
               in
               message_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | "percentage" -> (
             match Ppx_yojson_conv_lib.( ! ) percentage_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson int_of_yojson _field_yojson
               in
               percentage_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let cancellable_value, message_value, percentage_value =
             ( Ppx_yojson_conv_lib.( ! ) cancellable_field
             , Ppx_yojson_conv_lib.( ! ) message_field
             , Ppx_yojson_conv_lib.( ! ) percentage_field )
           in
           { cancellable =
               (match cancellable_value with
               | None -> None
               | Some v -> v)
           ; message =
               (match message_value with
               | None -> None
               | Some v -> v)
           ; percentage =
               (match percentage_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { cancellable = v_cancellable
       ; message = v_message
       ; percentage = v_percentage
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_percentage then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_int) v_percentage
           in
           let bnd = ("percentage", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_message then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_string) v_message
           in
           let bnd = ("message", arg) in
           bnd :: bnds
       in
       let bnds =
         if None = v_cancellable then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool) v_cancellable
           in
           let bnd = ("cancellable", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(cancellable : bool option) ?(message : string option)
      ?(percentage : int option) (() : unit) : t =
    { cancellable; message; percentage }

  let yojson_of_t (t : t) : Json.t =
    Json.To.literal_field "kind" "report" yojson_of_t t

  let t_of_yojson (json : Json.t) : t =
    Json.Of.literal_field "t" "kind" "report" t_of_yojson json
end

module WorkspaceSymbolOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkspaceSymbolOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

module WorkspaceSymbolParams = struct
  type t = { query : string }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkspaceSymbolParams.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let query_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "query" -> (
             match Ppx_yojson_conv_lib.( ! ) query_field with
             | None ->
               let fvalue = string_of_yojson _field_yojson in
               query_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
           match Ppx_yojson_conv_lib.( ! ) query_field with
           | Some query_value -> { query = query_value }
           | _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
               _tp_loc yojson
               [ ( Ppx_yojson_conv_lib.poly_equal
                     (Ppx_yojson_conv_lib.( ! ) query_field)
                     None
                 , "query" )
               ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { query = v_query } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_query in
         ("query", arg) :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ~(query : string) : t = { query }
end

module WorkspaceSymbolRegistrationOptions = struct
  type t =
    { workDoneProgress : bool Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc = "lsp/src/types.ml.WorkspaceSymbolRegistrationOptions.t" in
     function
     | `Assoc field_yojsons as yojson -> (
       let workDoneProgress_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
           | "workDoneProgress" -> (
             match Ppx_yojson_conv_lib.( ! ) workDoneProgress_field with
             | None ->
               let fvalue =
                 Json.Nullable_option.t_of_yojson bool_of_yojson _field_yojson
               in
               workDoneProgress_field := Some fvalue
             | Some _ ->
               duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
           | _ -> ());
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
         | [] ->
           let workDoneProgress_value =
             Ppx_yojson_conv_lib.( ! ) workDoneProgress_field
           in
           { workDoneProgress =
               (match workDoneProgress_value with
               | None -> None
               | Some v -> v)
           }))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { workDoneProgress = v_workDoneProgress } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_workDoneProgress then
           bnds
         else
           let arg =
             (Json.Nullable_option.yojson_of_t yojson_of_bool)
               v_workDoneProgress
           in
           let bnd = ("workDoneProgress", arg) in
           bnd :: bnds
       in
       `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_t

  [@@@end]

  let create ?(workDoneProgress : bool option) (() : unit) : t =
    { workDoneProgress }
end

(*$*)

module CodeActionResult = struct
  type t = [ `Command of Command.t | `CodeAction of CodeAction.t ] list option

  let yojson_of_t (t : t) : Json.t =
    match t with
    | None -> `Null
    | Some xs ->
      Json.To.list
        (function
          | `Command c -> Command.yojson_of_t c
          | `CodeAction a -> CodeAction.yojson_of_t a)
        xs

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Null -> None
    | `List _ ->
      Some
        (Json.Of.list
           (Json.Of.untagged_union "CodeActionResult"
              [ (fun j -> `Command (Command.t_of_yojson j))
              ; (fun j -> `CodeAction (CodeAction.t_of_yojson j))
              ])
           json)
    | _ -> Json.error "CodeActionResult" json
end

module Locations = struct
  type t =
    [ `Location of Location.t list
    | `LocationLink of LocationLink.t list
    ]

  let yojson_of_t (t : t) : Json.t =
    match t with
    | `Location xs -> `List (List.map ~f:Location.yojson_of_t xs)
    | `LocationLink l -> `List (List.map ~f:LocationLink.yojson_of_t l)

  let t_of_yojson (json : Json.t) : t =
    match json with
    | `Assoc _ -> `Location [ Location.t_of_yojson json ]
    | `List [] -> `Location []
    | `List (x :: xs) -> (
      match Location.t_of_yojson x with
      | loc -> `Location (loc :: List.map ~f:Location.t_of_yojson xs)
      | exception Of_yojson_error (_, _) ->
        `LocationLink (List.map ~f:LocationLink.t_of_yojson (x :: xs)))
    | _ -> Json.error "Locations.t" json
end
