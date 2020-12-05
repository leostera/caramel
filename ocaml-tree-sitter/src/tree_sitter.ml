type input_edit

type language

type node

type parser_

type point

type range

type tree

type tree_cursor

external language_version : unit -> int = "caml_ts_language_version"

external min_compatible_language_version : unit -> int
  = "caml_ts_min_compatible_language_version"

external parser_header : unit -> string = "caml_ts_parser_header"

module Point = struct
  (*
  external make : row:int -> column:int -> point = "caml_ts_point__new"
  *)

end

module Range = struct
  (*
  external make :
    start_byte:int ->
    end_byte:int ->
    start_point:point ->
    end_point:point ->
    range = "caml_ts_range__new"
  *)

end

module Input_edit = struct
  (*
     external make :
       start_byte:int ->
       old_end_byte:int ->
       new_end_byte:int ->
       start_position:point ->
       old_end_position:point ->
       new_end_position:point ->
       input_edit = "caml_ts_input_edit__new"
  *)

end

module Node = struct
  (* TODO(@ostera): needs a `range` type *)
  (* external byte_range : node -> int range = "caml_ts_node__byte_range"

     external child : node -> int -> node option = "caml_ts_node__child"

     external child_by_field_id : node -> int -> node option
       = "caml_ts_node__child_by_field_id"

     external child_by_field_name : node -> string -> node option
       = "caml_ts_node__child_by_field_name"

     external child_count : node -> int = "caml_ts_node__child_count"

     external descendant_for_byte_range : node -> int -> int -> node option
       = "caml_ts_node__descendant_for_byte_range"

     external descendant_for_point_range : node -> point -> point -> node option
       = "caml_ts_node__descendant_for_point_range"

     external edit : node -> input_edit -> unit = "caml_ts_node__edit"

     external end_byte : node -> int = "caml_ts_node__end_byte"

     external end_position : node -> point = "caml_ts_node__end_position"

     external has_changes : node -> bool = "caml_ts_node__has_changes"

     external has_error : node -> bool = "caml_ts_node__has_error"

     external id : node -> int = "caml_ts_node__id"

     external is_error : node -> bool = "caml_ts_node__is_error"

     external is_extra : node -> bool = "caml_ts_node__is_extra"

     external is_missing : node -> bool = "caml_ts_node__is_missing"

     external is_named : node -> bool = "caml_ts_node__is_named"

     external kind : node -> string = "caml_ts_node__kind"

     external kind_id : node -> int = "caml_ts_node__kind_id"

     external language : node -> language = "caml_ts_node__language"

     external named_child : node -> int -> node option
       = "caml_ts_node__named_child"

     external named_child_count : node -> int = "caml_ts_node__named_child_count"

     external named_descendant_for_byte_range : node -> int -> int -> node option
       = "caml_ts_node__named_descendant_for_byte_range"

     external named_descendant_for_point_range :
       node -> point -> point -> node option
       = "caml_ts_node__named_descendant_for_point_range"

     external next_named_sibling : node -> node option
       = "caml_ts_node__next_named_sibling"

     external next_sibling : node -> node option = "caml_ts_node__next_sibling"

     external parent : node -> node option = "caml_ts_node__parent"

     external prev_named_sibling : node -> node option
       = "caml_ts_node__prev_named_sibling"

     external prev_sibling : node -> node option = "caml_ts_node__prev_sibling"

     external range : node -> range = "caml_ts_node__range"

     external start_byte : node -> int = "caml_ts_node__start_byte"

     external start_position : node -> point = "caml_ts_node__start_position"


     external utf16_text : node -> string = "caml_ts_node__utf16_text"


     external walk : node -> tree_cursor = "caml_ts_node__walk"
  *)
  external to_sexp : node -> string = "caml_ts_node__to_sexp"

  external utf8_text : node -> string -> string = "caml_ts_node__utf8_text"

  external walk : node -> tree_cursor = "caml_ts_node__walk"

  (* The following are bindings that require an implementation of an iterator.

     external children : t -> Tree.Cursor  -> iter? = "caml_ts_node__"
     external children_by_field_id:  t -> int -> Tree.Cursor -> iter ? = "caml_ts_node__"
     external children_by_field_name : t -> string -> Tree.Cursor -> iter ? = "caml_ts_node__"
     external named_children : t -> Tree.Cursor -> iter? = "caml_ts_node__"
  *)
end

module Tree = struct
  module Cursor = struct
    external node : tree_cursor -> node = "caml_ts_tree_cursor__node"

    (*
    external field_id : tree_cursor -> int option
      = "caml_ts_tree_cursor__field_id"

    external field_name : tree_cursor -> string option
      = "caml_ts_tree_cursor__field_name"

    external goto_first_child_for_byte : tree_cursor -> int -> int option
      = "caml_ts_tree_cursor__goto_first_child_for_byte"

    external reset : tree_cursor -> node -> unit = "caml_ts_tree_cursor__reset"
      *)

    external goto_first_child : tree_cursor -> bool
      = "caml_ts_tree_cursor__goto_first_child"

    external goto_parent : tree_cursor -> bool
      = "caml_ts_tree_cursor__goto_parent"

    external goto_next_sibling : tree_cursor -> bool
      = "caml_ts_tree_cursor__goto_next_sibling"
  end

  external root_node : tree -> node = "caml_ts_tree__root_node"

  external walk : tree -> tree_cursor = "caml_ts_tree__walk"

  (*
    external language : tree -> language = "caml_ts_tree__language"
     external edit : tree -> input_edit = "caml_ts_tree__edit"
     external changed_ranges : t -> t -> iter? = ""
   *)
end

module Language = struct
  (*
  external field_count : language -> int = "caml_ts_language__field_count"

  external field_id_for_name : language -> string -> int option
    = "caml_ts_language__field_id_for_name"

  external field_name_for_id : language -> int -> string option
    = "caml_ts_language__field_name_for_id"

  external id_for_node_kind : language -> string -> bool -> int
    = "caml_ts_language__id_for_node_kind"

  external node_kind_is_named : language -> int -> bool
    = "caml_ts_language__node_kind_is_name"

  external node_kind_is_visible : language -> int -> bool
    = "caml_ts_language__node_kind_is_visible"
    *)

  external node_kind_count : language -> int
    = "caml_ts_language__node_kind_count"

  external node_kind_for_id : language -> int -> string option
    = "caml_ts_language__node_kind_for_id"

  external version : language -> int = "caml_ts_language__version"

  type raw

  external from_raw_ptr : raw -> language = "caml_ts_language__from_raw_ptr"
end

module Parser = struct
  external make : unit -> parser_ = "caml_ts_parser__new"

  external language : parser_ -> language option = "caml_ts_parser__language"

  external parse : parser_ -> string -> tree option -> tree option
    = "caml_ts_parser__parse"

  (*
  external parse_utf16 : parser_ -> string -> tree option -> tree option
    = "caml_ts_parser__parse_utf16"

  external print_dot_graphs : parser_ -> string
    = "caml_ts_parser__print_dot_graphs"
    *)

  external reset : parser_ -> unit = "caml_ts_parser__reset"

  external set_language : parser_ -> language -> unit
    = "caml_ts_parser__set_language"

  external set_timeout_micros : parser_ -> int64 -> unit
    = "caml_ts_parser__set_timeout_micros"

  external timeout_micros : parser_ -> int = "caml_ts_parser__timeout_micros"

  (*
  external stop_printing_dot_graphs : parser_ -> unit
    = "caml_ts_parser__stop_printing_dot_graphs"

     external cancellation_flag : parser_ -> atomic_usize option = ""
     external set_cancellation_flag : parser_ -> atomic_usize option = ""
    external set_included_ranges : parser_ -> range list = "caml_ts_parser__set_included_ranges"

 *)
end
