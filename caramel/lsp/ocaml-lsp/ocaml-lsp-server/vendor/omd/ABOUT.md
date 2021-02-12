<!-- -*- coding: utf-8 -*- -->

About [OMD](https://github.com/pw374/omd/)
==========================================

The implementation of this library and command-line tool
is based on [DFMSD][].
That description doesn't define a grammar but a sort of guide for 
human users who are not trying to implement it. In other words, 
it's ambiguous, which is a problem since there are no errors in the
Markdown language, which design is mostly based on some 
email-writing experience: the meaning of a phrase is the meaning
a human would give when reading the phrase as some email contents.
For instance, if there are blank lines that have spaces
(lines that read empty but actually contain some characters, from
the computer point of view since spaces are represented by characters), 
since they're invisible to the normal human reader, they should be ignored.


Specificities
-------------

There follows a list of specificities of OMD.
This list is probably not exhaustive.

**Please note that OMD's semantics have changed over time, but they are becoming
more and more stable with time and new releases. The goal is to eventually
have a semantics that's as sane as it can possibly be for a Markdown parser. 
Please [browse and open issues](https://github.com/pw374/omd/issues/)
if you find something that seems wrong.**

- Email addresses encoding: email addresses are not hex entity-encoded.
  
- `[foo]` is a short-cut for `[foo][]`, but if `foo` is not a reference
  then `[foo]` is printed `[foo]`, not `[foo][]`.
  *(Taken from Github Flavour Markdown.)*

- The Markdown to Markdown conversion may performe 
  some cleaning (some meaningless characters may disappear)
  or spoiling (some meaningless characters may appear),
  but both inputs and ouputs should have the same semantics (otherwise
  please do report the bug).

- A list containing at least one item which has at least one paragraph
  is a list for which all items have paragraphs and/or blocks.
  In HTML words, in practice, if an `li` of a `ul` or `ol` has a `p`,
  then all other `li`s of that list have at least a `p` or a `pre`.

- It's not possible to emphasise a part of a word using underscores.
  *(Taken from Github Flavour Markdown.)*

- A code section declared with at least 3 backquotes (`` ` ``) at the
  first element on a line is a code block. The backquotes should be 
  followed by a language name (made of a-z characters) or by a newline.

- A code block starting with several backquotes (e.g., ```` ``` ````) 
  immediately followed by a word W made of a-z characters is a code block
  for which the code language is W. (If you use other characters than
  a-z, the semantics is currently undefined although it's deterministic
  of course, because it may change in the near future.) Also, if you use
  the command line tool `omd`, you can define programs to process code
  blocks specifically to the languages that are declared for those code
  blocks.

- Each and every tabulation is converted by OMD to 4 spaces at the lexing
  step. And the behaviour of the parser is undefined for tabulations.
  - Note that it does mean that if you have a document with some code written 
    using the 
    [Whitespace](http://en.wikipedia.org/wiki/Whitespace_(programming_language))
    language, it will not work very well. This might be fixed in the future
    but unless you have a very good reason for OMD to support tabulations,
    it will probably not.

- Parentheses and square brackets are generally parsed in a way such that
  `[a[b]](http://c/(d))` is the URL `http://c/(d)` with the text `a[b]`.
  If you want a parenthesis or bracket not to count in the balanced parsing,
  escape it with a backslash, such as in `[a\[b](http://c/\(d)`.
  *This is typically something that's not defined in [DFMSD].*
  - Note about backslashes in URLs: some web browsers (e.g., Safari) 
    automatically convert `\` to `/`. It's not the case of CURL.
    However I assume it's safe to consider that backslashes are not
    to be used in URLs. Still it's always possible to
    backslashe-escape them anyways.

- HTML is somewhat a part of Markdown. OMD will partially parse HTML tags
  and if you have a tag that isn't a known HTML tag, then it's possible
  that OMD will not consider it as HTML. For instance, a document
  containing just `<foo></foo>` will be converted to 
  `<p>&lt;foo&gt;&lt;/foo&gt;</p>`.
  - It's possible to ask `omd` to relax this constraint.

- Some additional features are available on the command line. 
  For more information, used the command `omd -help`



[DFMSD]: http://daringfireball.net/projects/markdown/syntax 
        "John Gruber's description of the syntax of Markdown"

"DFMSD" is short for "Daring Fireball: Markdown Syntax Documentation", 
which is the HTML title of the page located at 
<http://daringfireball.net/projects/markdown/syntax>.

Extension mechanisms
--------------------

The parser  is implemented using  a big (very big)  recursive function
(`Omd_parser.Make(Env).main_loop_rev`), with  a set of  some auxiliary
functions.  Some  parts  are  easy   to  understand,  some  parts  are
not. However, overall, it should be easy enough.


The parser has a double extension mechanism.

1. To use the  first mechanism, you may define a  set of functions in
the module  `Env` given to instanciate  the functor `Omd_parser.Make`.
  * The   value  `Env.extensions`   is  a   list  of   elements  of
    type `Omd_representation.extension` which is equal to 
    `r -> p -> l -> (r * p * l) option` where
    * `r = Omd_representation.t`
       and represents the result of the parsing process,
    * `p = Omd_representation.tok list`
       and represents the tokens preceding `l`,  
    * and `l = tok list` and is the list of tokens to parse.
    * The result, of type `(r * p * l) option`, is `None` if
      the extension has no effect (and the parser will continue
      doing its job with its state it had before using the
      extension), and is `Some(r,p,l)` when it gives a new set of
      data to the parser.
  * Each element of the list `Env.extensions` is applied in a fold left
    manner. (The first element of that list is applied first.)
  * And they are applied when a standard parsing rule fails.

2. The second extension stands in the representation of the lexemes
   (`Tag of string * extension`).
   It allows to insert extensions directly into the lexeme list. 

The  Markdown representation  also  provides  an extension  mechanism,
which is  useful if you want  to insert “smart objects”  (which are as
“smart” as smartphones). Those objects have four methods, 2 of them
are particularly useful: `to_html` and `to_t`, and implementing one
of them is necessary. They both return a `string option`, and a default
dummy such smart object can be defined as follows:

```ocaml
let dummy =
  X (object
    method name = "dummy"
    method to_html ?(indent=0) _ _ = None
    method to_sexpr _ _ = None
    method to_t _ = None
  end)
```



History
-------

OMD has been developed by [Philippe Wang](https://github.com/pw374/)
at [OCaml Labs](http://ocaml.io/) in [Cambridge](http://www.cl.cam.ac.uk),
with precious feedbacks and [pull requests](https://github.com/pw374/omd/pulls)
(cf. next section).

Its development was motivated by at least these facts:

- We wanted an OCaml implementation of Markdown; some OCaml parsers of
  Markdown existed before but they were incomplete. It's easier for an
  OCaml project to depend on an pure-OCaml implementation of Markdown than
  to depend some interface to a library implemented using another language,
  and this is ever more important since [Opam](https://opam.ocaml.org) exists.

- We wanted to provide a way to make the contents of
  the [OCaml.org](http://ocaml.org/) website be essentially in Markdown
  instead of HTML. And we wanted to this website to be implemented in
  OCaml.

- Having an OCaml implementation of Markdown is virtually mandatory for
  those who want to use a Markdown parser in 
  a [Mirage](http://www.openmirage.org) application.
  Note that OMD has replaced the previous Markdown parser of
  [COW](https://github.com/mirage/ocaml-cow), which has been developed 
  as part of the Mirage project.



Thanks
------

Thank you to 
[Christophe Troestler](https://github.com/Chris00),
[Ashish Argawal](https://github.com/agarwal),
[Sebastien Mondet](https://github.com/smondet),
[Thomas Gazagnaire](https://github.com/samoht),
[Daniel Bünzli](https://github.com/dbuenzli),
[Amir Chaudry](https://github.com/amirmc),
[Anil Madhavapeddy](https://github.com/avsm/),
[David Sheets](https://github.com/dsheets/),
[Jeremy Yallop](https://github.com/yallop/),
and \<please insert your name here if you believe you've been forgotten\>
for their feedbacks and contributions to this project.



Miscellaneous notes
-------------------

- There's been absolutely no effort in making OMD fast, but it should be 
  amongst the fastest parsers of Markdown, just thanks to the fact that 
  it is implemented in OCaml. That being said, there's quite some room
  for performance improvements. One way would be to make a several-pass
  parser with different intermediate representations (there're currently
  only 2 representations: one for the lexing tokens and one for the parse
  tree).

- The hardest part of implementing a parser of Markdown is the process
  of understanding and unravelling the grammar of Markdown to turn it into
  a program.

- OMD 1.0.0 will probably use some external libraries,
  e.g., [UUNF](http://erratique.ch/software/uunf)
  and perhaps [Xmlm](http://erratique.ch/software/xmlm/doc/Xmlm)


- "OMD" is the name of this library and command-line tool.
  - It might be written "Omd" or "omd" sometimes, but it should
    be written using capital letters because it should be read 
    `əʊ ɛm diː` rather than `ə'md` or `ˌɒmd`.

- "`Omd`" is a module.
  - It's written using monospace font and it's capitalized.

- "`omd`" is a command-line tool.
  - It's written using monospace font and it's always lowercase letters only
    because unless you have a non-sensitive file system, calling `Omd` on the
    command line is not just another way of calling `omd`.

- OMD has been added on the quite long list of Markdown parsers
  <https://github.com/markdown/markdown.github.com/wiki/Implementations>
  on the 29th of January.

