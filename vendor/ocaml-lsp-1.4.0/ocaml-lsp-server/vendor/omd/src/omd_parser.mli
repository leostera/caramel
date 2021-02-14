(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

(** Beware: the functions in this module may raise exceptions! If you
    use them, you should be careful. *)


type r = Omd_representation.t
(** accumulator (beware, reversed tokens) *)

and p = Omd_representation.tok list
(** context information: previous elements *)

and l = Omd_representation.tok list
(** tokens to parse *)

and main_loop =
  ?html:bool ->
  r -> (* accumulator (beware, reversed tokens) *)
  p -> (* info: previous elements *)
  l -> (* tokens to parse *)
  Omd_representation.t (* final result *)
(** most important loop, which has to be given as an argument *)


val default_parse :
  ?extensions:Omd_representation.extensions -> ?default_lang:string -> l
  -> Omd_representation.t
(** Translate tokens to Markdown representation.

    @param lang language for blocks of code where it was not specified.
    Default: [""].
*)

module type Env =
sig
  val rc: Omd_representation.ref_container
  (** reference container *)
  val extensions : Omd_representation.extensions
  (** list of parser extensions *)
  val default_lang : string
  (** default language for code blocks *)
  val gh_uemph_or_bold_style : bool
  (** flag: bold/emph using using underscores is by default
      github-style, which means that underscores inside words are
      left as underscore, rather than special characters, because
      it's more convenient. However it is also less expressive
      because then you can't bold/emph a part of a word. You might
      want to set this flag to false. *)
  val blind_html : bool
  (** flag: if true, will not check whether a used HTML tag actually
      exists in HTML. *)
  val strict_html : bool
  (** flag: if true, will only accept known inline HTML tags in inline HTML. *)
  val warning : bool
  (** flag: if true, will output warnings *)
  val warn_error : bool
  (** flag: if true, will convert warnings to errors *)
end

module Default_env : functor (Unit: sig end) -> Env

module Make : functor (Env : Env) ->
sig

  val rc: Omd_representation.ref_container
  (** reference container *)
  val extensions : Omd_representation.extensions
  (** list of parser extensions *)
  val default_lang : string
  (** default language for code blocks *)
  val gh_uemph_or_bold_style : bool
  (** flag: bold/emph using using underscores is by default
      github-style, which means that underscores inside words are
      left as underscore, rather than special characters, because
      it's more convenient. However it is also less expressive
      because then you can't bold/emph a part of a word. You might
      want to set this flag to false. *)
  val blind_html : bool
  (** flag: if true, will not check whether a used HTML tag actually
      exists in HTML. *)
  val strict_html : bool
  (** flag: if true, will only accept known inline HTML tags in inline HTML. *)


  val htmlcodes_set : Omd_utils.StringSet.t
  (** set of known HTML codes *)

  val inline_htmltags_set : Omd_utils.StringSet.t
  (** set of known inline HTML tags *)

  val htmltags_set : Omd_utils.StringSet.t
  (** All known HTML tags *)

  val unindent_rev :
    int ->
    Omd_representation.tok list ->
    Omd_representation.tok list * Omd_representation.tok list
  (** [unindent_rev n l] returns the same couple as [unindent n l]
      except that the first element (which is a list) is reversed.
      This function is used for lists. *)

  val unindent :
    int ->
    Omd_representation.tok list ->
    Omd_representation.tok list * Omd_representation.tok list
  (** [unindent n l] returns [(unindented, rest)] where [unindented] is
      the consecutive lines of [l] that are indented with at least [n]
      spaces, and de-indented by [n] spaces. If [l] starts with a line
      that is indented by less than [n] spaces, then it returns [([], l)].


      (* This function is used for lists, so it does not require [n] *)
      (* spaces on every single line, but only on some specific ones of them. *)

      This function is used for lists and blockquotes.

  *)

  (* val unindent_strict_rev : *)
  (*   int -> *)
  (*   Omd_representation.tok list -> *)
  (*   Omd_representation.tok list * Omd_representation.tok list *)
  (* (\** [unindent_strict_rev n l] returns the same couple as [unindent n l] *)
  (*     except that the first element (which is a list) is reversed. *)
  (*     This function is used for blockquotes. *\) *)

  (* val unindent_strict : *)
  (*   int -> *)
  (*   Omd_representation.tok list -> *)
  (*   Omd_representation.tok list * Omd_representation.tok list *)
  (* (\** [unindent_strict n l] returns [(unindented, rest)] where [unindented] is *)
  (*     the consecutive lines of [l] that are indented with at least [n] *)
  (*     spaces, and de-indented by [n] spaces. If [l] starts with a line *)
  (*     that is indented by less than [n] spaces, then it returns [([], l)]. *)
  (*     This function is used for blockquotes. *)
  (* *\) *)



  val is_blank : Omd_representation.tok list -> bool
  (** [is_blank l] returns [true] if [l] only contains blanks, which are
      spaces and newlines. *)

  val semph_or_bold :
    int ->
    Omd_representation.tok list ->
    (Omd_representation.tok list * Omd_representation.tok list) option
  (** [semph_or_bold n l] returns [None] if [l] doesn't start with
      a bold/emph phrase (marked using stars), else it returns [Some(x,y)]
      where [x] is the emph and/or bold phrase at the beginning of [l]
      and [y] is the rest of [l]. *)

  val sm_uemph_or_bold :
    int ->
    Omd_representation.tok list ->
    (Omd_representation.tok list * Omd_representation.tok list) option
  (** [sm_uemph_or_bold n l] returns [None] if [l] doesn't start with
      a bold/emph phrase (marked using underscores), else it returns [Some(x,y)]
      where [x] is the emph and/or bold phrase at the beginning of [l]
      and [y] is the rest of [l]. *)

  val gh_uemph_or_bold :
    int ->
    Omd_representation.tok list ->
    (Omd_representation.tok list * Omd_representation.tok list) option
  (** [gh_uemph_or_bold n l] returns [None] if [l] doesn't start with
      a bold/emph phrase (marked using underscores), else it returns [Some(x,y)]
      where [x] is the emph and/or bold phrase at the beginning of [l]
      and [y] is the rest of [l]. *)

  val uemph_or_bold :
    int ->
    Omd_representation.tok list ->
    (Omd_representation.tok list * Omd_representation.tok list) option
  (** [uemph_or_bold n l] returns [None] if [l] doesn't start with a
      bold/emph phrase (marked using underscores), else it returns
      [Some(x,y)] where [x] is the emph and/or bold phrase at the
      beginning of [l] and [y] is the rest of [l]. N.B. if
      [!gh_uemph_or_bold_style] then in Github style (i.e., underscores
      inside words are considered as underscores). *)

  val eat_blank : Omd_representation.tok list -> Omd_representation.tok list
  (** [eat_blank l] returns [l] where all blanks at the beginning of the
      list have been removed (it stops removing as soon as it meets an element
      that is not a blank). Blanks are spaces and newlines only. *)

  val tag__maybe_h1 : main_loop -> Omd_representation.tok
  (** [tag__maybe_h1 main_loop] is a tag that is injected everywhere that
      might preceed a H1 title. It needs [main_loop] as argument because
      it is used to parse the contents of the titles. *)

  val tag__maybe_h2 : main_loop -> Omd_representation.tok
  (** [tag__maybe_h2 main_loop] is the same as [tag__maybe_h1 main_loop]
      but for H2. *)

  val tag__md : Omd_representation.t -> Omd_representation.tok
  (** [tag__md md] encapsulates [md] to make it a value of type [tok].
      Its purpose is to inject some pre-parsed markdown (i.e., [md] of type [t])
      in a yet-to-parse token stream of type [tok]. *)

  val tag_setext :
    main_loop -> Omd_representation.tok list -> Omd_representation.tok list
  (** Tag used for the lines that *might* be titles using setext-style. *)


  val hr_m : l -> l option
  (** [hr_m l] returns [Some nl] where [nl] is the remaining of [l] if [l]
      contains a horizontal rule "drawn" with dashes. If there's no HR, then
      returns [None].*)

  val hr_s : l -> l option
  (** [hr_s l] is the same as [hr_m l]  but for horizontal rules
      "drawn" with stars instead. *)

  exception NL_exception
  exception Premature_ending

  val read_until_gt :
    ?bq:bool ->
    ?no_nl:bool ->
    Omd_representation.tok list ->
    Omd_representation.tok list * Omd_representation.tok list
  val read_until_lt :
    ?bq:bool ->
    ?no_nl:bool ->
    Omd_representation.tok list ->
    Omd_representation.tok list * Omd_representation.tok list
  val read_until_cparenth :
    ?bq:bool ->
    ?no_nl:bool ->
    Omd_representation.tok list ->
    Omd_representation.tok list * Omd_representation.tok list
  val read_until_oparenth :
    ?bq:bool ->
    ?no_nl:bool ->
    Omd_representation.tok list ->
    Omd_representation.tok list * Omd_representation.tok list
  val read_until_dq :
    ?bq:bool ->
    ?no_nl:bool ->
    Omd_representation.tok list ->
    Omd_representation.tok list * Omd_representation.tok list
  val read_until_q :
    ?bq:bool ->
    ?no_nl:bool ->
    Omd_representation.tok list ->
    Omd_representation.tok list * Omd_representation.tok list
  val read_until_obracket :
    ?bq:bool ->
    ?no_nl:bool ->
    Omd_representation.tok list ->
    Omd_representation.tok list * Omd_representation.tok list
  val read_until_cbracket :
    ?bq:bool ->
    ?no_nl:bool ->
    Omd_representation.tok list ->
    Omd_representation.tok list * Omd_representation.tok list
  val read_until_space :
    ?bq:bool ->
    ?no_nl:bool ->
    Omd_representation.tok list ->
    Omd_representation.tok list * Omd_representation.tok list
  val read_until_newline :
    Omd_representation.tok list ->
    Omd_representation.tok list * Omd_representation.tok list
  (** [read_until_...] are functions that read from a token list
      and return two token lists: the first one is the tokens read
      until a specific token is met, and the second one is the remainder.
      The particularity of these functions is that they do consider
      backslash-escaped characters and closing characters.
      For instance, [read_until_gt "1 < 2 > 3 > 4"] returns
      ["1 < 2 > 3 ", " 4"]: note that the ">" before " 4" has disappeared
      and that [read_until_gt] takes a [tok list] (not a string) and
      returns a couple of [tok list] (not a couple of strings), the
      string notation is used here for concision.

      Until otherwise noted, those functions do *not* consider
      backquote-trapped sections.
      For instance, [read_until_gt "1 < 2 > 3 `>` 4"]
      returns ["1 < 2 > 3 `", "` 4"].
      If you use these functions, you should make sure that they
      do what you think they do (i.e., do look at the code).

      If the expected characters are not found, the exception
      [Premature_ending] is raised. For instance,
      [read_until_gt "1 < > 3"] raises [Premature_ending].

      If [no_nl] is [true] (default value for [no_nl] is [false])
      and ['\n'] occurs before the splitting character,
      then [NL_exception] is raised.
  *)


  val read_title : main_loop -> int -> r -> p -> l -> (r * p * l) option
  (** [read_title main_loop n r p l] returns [Some(r,p,l)]
      if it succeeds, [None] otherwise.

      [read_title main_loop n r p l] expects to read a [n]-level
      hash-declared title from [l], where the hashes have *already*
      been *removed*. If [n] is not between 1 and 6 (included), then
      it returns [None].

      [main_loop] is used to parse the contents of the title.

      [r] and [p] are the classical "result" and "previous" parameters.
  *)

  val maybe_extension :
    Omd_representation.extensions ->
    r -> p -> l -> (r * p * l) option
  (** [maybe_extension e r p l] returns [None] if there is no extension or
      if extensions haven't had  any effect, returns [Some(nr, np, nl)] if
      at least one extension has applied successfully. *)

  val emailstyle_quoting : main_loop -> r -> p -> l -> (r * p * l) option
  (** [emailstyle_quoting main_loop r p l] returns [Some(r,p,l)] with
      [r] being the updated result, [p] being the last parsed token
      and [l] being the remaining tokens to parse. If [emailstyle_quoting]
      fails, then it returns [None], in which case its user is advise
      to investigate why it returns [None] because there's possibly a
      real problem. *)

  val maybe_reference :
    main_loop ->
    Omd_representation.ref_container -> r -> p -> l -> (r * p * l) option
  (** [maybe_reference] tries to parse a reference, a reference definition or
      a github-style short reference (e.g., [foo] as a shortcut for [foo][]),
      and returns [Some(r,p,l)] if it succeeds, [None] otherwise. *)

  val maybe_link : main_loop -> r -> p -> l -> (r * p * l) option
  (** [maybe_link] tries to parse a link,
      and returns [Some(r,p,l)] if it succeeds, [None] otherwise. *)


  val parse_list : main_loop -> r -> p -> l -> r * p * l
  (** [parse_list main_loop r p l] parses a list from [l].

      ***Important property***
      It is considered in Omd that a sub-list is always more indented than
      the item that contains it (so, 2 items with different indentations cannot
      have the direct same parent).
  *)

  val make_paragraphs : Omd_representation.t -> Omd_representation.t
  (** Since [Omd_parser.parse] doesn't build paragraphs, if you want
      Markdown-style paragraphs, you need to apply this function to
      the result of [Omd_parser.parse]. *)


  val bcode :
    ?default_lang:Omd_representation.name ->
    r -> p -> l -> (r * p * l) option
  (** [bcode default_lang r p l]
      tries to parse some code that's delimited by backquotes,
      and returns [Some(r,p,l)] if it succeeds, [None] otherwise.
  *)

  val icode :
    ?default_lang:Omd_representation.name ->
    r -> p -> l -> (r * p * l) option
  (** [icode default_lang r p l]
      tries to parse some code that's delimited by space indentation.
      It should always return [Some(r,p,l)], if it returns [None]
      it means that it's been misused or there's a bug.
  *)


  val main_loop_rev : ?html:bool -> r -> p -> l -> r
  val main_loop : ?html:bool -> r -> p -> l -> Omd_representation.t
  val main_parse : Omd_representation.tok list -> Omd_representation.t
  val parse : Omd_representation.tok list -> Omd_representation.t

end

