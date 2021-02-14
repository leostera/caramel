type token = Omd_representation.tok
type t = token list

val lex : string -> t
(** Translate a raw string into tokens for the parser.  To implement
    an extension to the lexer, one may process its result before
    giving it to the parser. To implement an extension to the
    parser, one may extend it using the constructor [Tag]
    from type [tok] and/or using the extensions mechanism
    of the parser (cf. the optional argument [extensions]).
    The main difference is that [Tag] is processed by the parser
    in highest priority whereas functions in [extensions] are applied
    with lowest priority. *)

type bigstring = (char,
                  Bigarray.int8_unsigned_elt,
                  Bigarray.c_layout) Bigarray.Array1.t

val lex_bigarray : bigstring -> t
(** As {!lex}, but read input from a bigarray rather than from a string. *)

val string_of_tokens : t -> string
(** [string_of_tokens t] return the string corresponding to the token
    list [t]. *)

val length : token -> int
(** [length t] number of characters of the string represented as [t]
    (i.e. [String.length(string_of_token t)]). *)

val string_of_token : token -> string
(** [string_of_token tk] return the string corresponding to the token
    [tk]. *)

val make_space : int -> token

val split_first : token -> token * token
(** [split_first(Xs n)] returns [(X, X(n-1))] where [X] is a token
    carrying an int count.

    @raise Invalid_argument is passed a single token. *)


val destring_of_tokens : ?limit:int -> t -> string
(** Converts the tokens to a simple string representation useful for
    debugging.  *)
