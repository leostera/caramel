module Invalid_locations : sig
  val structure : Lexing.lexbuf -> Location.t list

  val signature : Lexing.lexbuf -> Location.t list

  val use_file : Lexing.lexbuf -> Location.t list
end

module Make_parsable : sig
  val structure : string -> string
  (** [structure s] tries to parse [s] as a structure node of the AST, if there
      are invalid parts in the input, they are wrapped inside
      [\[%%invalid.ast.node "..."\]] attributes so the output string can
      successfully be parsed as a structure node by the standard OCaml parser. *)

  val signature : string -> string
  (** [signature s] tries to parse [s] as a signature node of the AST, if there
      are invalid parts in the input, they are wrapped inside
      [\[%%invalid.ast.node "..."\]] attributes so the output string can
      successfully be parsed as a signature node by the standard OCaml parser. *)

  val use_file : string -> string
  (** [use_file s] tries to parse [s] as a use_file node of the AST, if there
      are invalid parts in the input, they are wrapped inside
      [\[%%invalid.ast.node "..."\]] attributes so the output string can
      successfully be parsed as a use_file node by the standard OCaml parser. *)
end
