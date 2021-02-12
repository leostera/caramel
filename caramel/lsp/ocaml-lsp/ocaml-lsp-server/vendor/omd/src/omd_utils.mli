(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013/2014 by Philippe Wang <philippe.wang@cl.cam.ac.uk>         *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

val debug : bool
(** Equals [true] if the environment variable DEBUG is set,
    or if the environment variable OMD_DEBUG is set to a string
    that is not ["false"]. *)

val trackfix : bool

exception Error of string

val raise : exn -> 'a
(** Same as [Pervasives.raise] except if [debug] equals true,
    in which case it prints a trace on stderr before raising the exception. *)

val warn : ?we:bool -> string -> unit
(** [warn we x] prints a warning with the message [x] if [we] is true,
    else raises [Omd_utils.Error x]. *)

module StringSet :
  sig
    include Set.S with type elt = string
    val of_list : elt list -> t
  end
(** Set of [string]. Cf. documentation of {!Set.S} *)

type 'a split = 'a list -> 'a split_action
(** Type of a split function *)

and 'a split_action =
  (** Don't split yet *)
  | Continue

  (** Don't split yet but continue with those two lists instead of default *)
  | Continue_with of 'a list * 'a list

  (** Do split with this split scheme *)
  | Split of 'a list * 'a list
(** Type of a split action *)


val fsplit_rev :
  ?excl:('a list -> bool) ->
  f:'a split -> 'a list -> ('a list * 'a list) option
(** [fsplit_rev ?excl ~f l] returns [Some(x,y)] where [x] is the
    **reversed** list of the consecutive elements of [l] that obey the
    split function [f].
    Note that [f] is applied to a list of elements and not just an
    element, so that [f] can look farther in the list when applied.
    [f l] returns [Continue] if there're more elements to consume,
    [Continue_with(left,right)] if there's more elements to consume
    but we want to choose what goes to the left part and what remains
    to process (right part), and returns [Split(left,right)] if
    the splitting is decided.
    When [f] is applied to an empty list, if it returns [Continue]
    then the result will be [None].

    If [excl] is given, then [excl] is applied before [f] is, to check
    if the splitting should be stopped right away. When the split
    fails, it returns [None]. *)


val fsplit :
  ?excl:('a list -> bool) ->
  f:'a split -> 'a list -> ('a list * 'a list) option
(** [fsplit ?excl ~f l] returns [Some(List.rev x, y)]
    if [fsplit ?excl ~f l] returns [Some(x,y)], else it returns [None]. *)

val id_of_string : < mangle : string -> string; .. > -> string -> string
(** [id_of_string ids id] returns a mangled version of [id], using the
    method [ids#mangle]. If you don't need mangling, you may use
    [object method mangle x = x end] for [ids].  However, the name
    [ids] also means that your object should have knowledge of all IDs
    it has issued, in order to avoid collision. This is why
    [id_of_string] asks for an object rather than "just a
    function". *)

val htmlentities : ?md:bool -> string -> string
(** [htmlentities s] returns a new string in which html-significant
    characters have been converted to html entities. For instance,
    "<Foo&Bar>" is converted to "&lt;Foo&amp;Bar&gt;". *)

val minimalize_blanks : string -> string
(** [minimalize_blanks s] returns a copy of [s] in which the first and last
   characters are never blank, and two consecutive blanks never happen. *)


val eat : ('a -> bool) -> 'a list -> 'a list
(** [eat f l] returns [l] where elements satisfying [f] have been removed,
    but it stops removing as soon as one element doesn't satisfy [f]. *)


val extract_html_attributes : string -> (string * string) list
(** Takes some HTML and returns the list of attributes of the first
    HTML tag.
    Notes:
    * Doesn't check the validity of HTML tags or attributes.
    * Doesn't support backslash escaping.
    * Attribute names are delimited by the space and equal characters.
    * Attribute values are either delimited by the double quote
      or the simple quote character.
*)

val extract_inner_html : string -> string
(** Takes an HTML node and returns the contents of the node.
    If it's not given a node, it returns something rubbish.
*)

val html_void_elements : StringSet.t
(** HTML void elements *)

val ( @ ) : 'a list -> 'a list -> 'a list
(**  Tail-recursive version of [Pervasives.(@)]. *)
