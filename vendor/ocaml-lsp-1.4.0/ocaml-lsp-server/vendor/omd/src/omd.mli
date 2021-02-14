(** A markdown parser in OCaml, with no extra dependencies.

    This module represents this entire Markdown library written in
    OCaml only.

    Its main purpose is to allow you to use the Markdown library while
    keeping you away from the other modules.

    If you want to extend the Markdown parser, you can do it without
    accessing any module of this library but this one, and by doing
    so, you are free from having to maintain a fork of this library.

    N.B. This module is supposed to be reentrant,
    if it's not then please report the bug. *)


(************************************************************************)
(** {2 Representation of Markdown documents} *)

type t = element list
(** Representation of a Markdown document.  *)

and ref_container =
    (< add_ref: string -> string -> string -> unit ;
       get_ref : string -> (string*string) option;
       get_all : (string * (string * string)) list;
     >)

(** A element of a Markdown document. *)
and element = Omd_representation.element =
  | H1 of t          (** Header of level 1 *)
  | H2 of t          (** Header of level 2 *)
  | H3 of t          (** Header of level 3 *)
  | H4 of t          (** Header of level 4 *)
  | H5 of t          (** Header of level 5 *)
  | H6 of t          (** Header of level 6 *)
  | Paragraph of t
  (** A Markdown paragraph (must be enabled in {!of_string}) *)
  | Text of string   (** Text. *)
  | Emph of t        (** Emphasis (italic) *)
  | Bold of t        (** Bold *)
  | Ul of t list     (** Unumbered list *)
  | Ol of t list     (** Ordered (i.e. numbered) list *)
  | Ulp of t list
  | Olp of t list
  | Code of name * string
  (** [Code(lang, code)] represent [code] within the text (Markdown:
      `code`).  The language [lang] cannot be specified from Markdown,
      it can be from {!of_string} though or when programatically
      generating Markdown documents.  Beware that the [code] is taken
      verbatim from Markdown and may contain characters that must be
      escaped for HTML. *)
  | Code_block of name * string
  (** [Code_block(lang, code)]: a code clock (e.g. indented by 4
      spaces in the text).  The first parameter [lang] is the language
      if specified.  Beware that the [code] is taken verbatim from
      Markdown and may contain characters that must be escaped for
      HTML. *)
  | Br (** (Forced) line break *)
  | Hr (** Horizontal rule *)
  | NL (** Newline character.  Newline characters that act
           like delimiters (e.g. for paragraphs) are removed from the AST. *)
  | Url of href * t * title
  | Ref of ref_container * name * string * fallback
  | Img_ref of ref_container * name * alt * fallback
  | Html of name * (string * string option) list * t
  | Html_block of name * (string * string option) list * t
  | Html_comment of string
  (** An HTML comment, including "<!--" and "-->". *)
  | Raw of string
  (** Raw: something that shall never be converted *)
  | Raw_block of string
  (** Raw_block: a block with contents that shall never be converted *)
  | Blockquote of t  (** Quoted block *)
  | Img of alt * src * title
  | X of (< (* extension of [element]. *)
           name: string;
           (* N.B. [to_html] means that htmlentities will not
              be applied to its output. *)
           to_html: ?indent:int -> (t -> string) -> t -> string option;
           to_sexpr: (t -> string) -> t -> string option;
           to_t: t -> t option >)

and fallback = < to_string : string ; to_t : t >
(** Fallback for references in case they refer to non-existant references *)

and name = string
(** Markdown reference name. *)

and alt = string
(** HTML img tag attribute. *)

and src = string
(** HTML attribute. *)

and href = string
(** HTML attribute. *)

and title = string
(** HTML attribute. *)

type code_stylist = lang:string -> string -> string
(** Function that takes a language name and some code and returns
    that code with style. *)


(************************************************************************)
(** {2 Input and Output} *)

val of_string : ?extensions:Omd_representation.extensions ->
                ?default_lang: name ->
                string -> t
(** [of_string s] returns the Markdown representation of the string
    [s].

    @param lang language for blocks of code where it was not
    specified.  Default: [""].

    If you want to use a custom lexer or parser, use {!Omd_lexer.lex}
    and {!Omd_parser.parse}.  *)

val of_bigarray : ?extensions:Omd_representation.extensions ->
                  ?default_lang: name ->
                  Omd_lexer.bigstring -> t
(** As {!of_string}, but read input from a bigarray rather than from a
    string. *)

val set_default_lang : name -> t -> t
(** [set_default_lang lang md] return a copy of [md] where the
    language of all [Code] or [Code_block] with an empty language is
    set to [lang]. *)

val to_html :
  ?override:(Omd_representation.element -> string option) ->
  ?pindent:bool -> ?nl2br:bool -> ?cs:code_stylist -> t -> string
(** Translate markdown representation into raw HTML.  If you need a
    full HTML representation, you mainly have to figure out how to
    convert [Html of string] and [Html_block of string]
    into your HTML representation.  *)

val to_markdown : t -> string
(** Translate markdown representation into textual markdown. *)

val to_text : t -> string
(** Translate markdown representation into raw text. *)


(************************************************************************)
(** {2 Tansforming Markdown documents} *)

val toc : ?start:int list -> ?depth:int -> t -> t
(** [toc md] returns [toc] a table of contents for [md].

    @param start gives the section for which the TOC must be built.
    For example [~start:[2;3]] will build the TOC for subsections of
    the second [H1] header, and within that section, the third [h2]
    header.  If a number is [0], it means to look for the first
    section at that level but stop if one encounters any other
    subsection.  If no subsection exists, an empty TOC [[]] will be
    returned.  Default: [[]] i.e. list all sections, starting with the
    first [H1].

    @param depth the table of contents.  Default: [2].  *)

;;
