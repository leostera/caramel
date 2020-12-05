external language_version : unit -> int = "caml_ts_language_version"

external min_compatible_language_version : unit -> int
  = "caml_ts_min_compatible_language_version"

external parser_header : unit -> string = "caml_ts_parser_header"

module Node = struct
  type t

  external to_sexp : t -> string = "caml_ts_node__to_sexp"
end

module Tree = struct
  type t

  external root_node : t -> Node.t = "caml_ts_tree__root_node"
end

module Language = struct
  type t

  type raw

  external from_raw_ptr : raw -> t = "caml_ts_language__from_raw_ptr"

  external version : t -> int = "caml_ts_language__version"

  external node_kind_count : t -> int = "caml_ts_language__node_kind_count"

  external node_kind_for_id : t -> int -> string option = "caml_ts_language__node_kind_for_id"
end

module Parser = struct
  type t

  external make : unit -> t = "caml_ts_parser__new"

  external timeout_micros : t -> int = "caml_ts_parser__timeout_micros"

  external set_timeout_micros : t -> int64 -> unit
    = "caml_ts_parser__set_timeout_micros"

  external set_language : t -> Language.t -> (unit, 'a) result
    = "caml_ts_parser__set_language"

  external parse : t -> string -> Tree.t option -> Tree.t option
    = "caml_ts_parser__parse"
end
