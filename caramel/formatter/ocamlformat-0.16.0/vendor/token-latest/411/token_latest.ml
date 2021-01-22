type token = Parser.token =
  | WITH
  | WHILE
  | WHEN
  | VIRTUAL
  | VAL
  | UNDERSCORE
  | UIDENT of string
  | TYPE
  | TRY
  | TRUE
  | TO
  | TILDE
  | THEN
  | STRUCT
  | STRING of (string * Location.t * string option)
  | STAR
  | SIG
  | SEMISEMI
  | SEMI
  | RPAREN
  | REC
  | RBRACKET
  | RBRACE
  | QUOTED_STRING_ITEM of
      (string * Location.t * string * Location.t * string option)
  | QUOTED_STRING_EXPR of
      (string * Location.t * string * Location.t * string option)
  | QUOTE
  | QUESTION
  | PRIVATE
  | PREFIXOP of string
  | PLUSEQ
  | PLUSDOT
  | PLUS
  | PERCENT
  | OR
  | OPTLABEL of string
  | OPEN
  | OF
  | OBJECT
  | NONREC
  | NEW
  | MUTABLE
  | MODULE
  | MINUSGREATER
  | MINUSDOT
  | MINUS
  | METHOD
  | MATCH
  | LPAREN
  | LIDENT of string
  | LETOP of string
  | LET
  | LESSMINUS
  | LESS
  | LBRACKETPERCENTPERCENT
  | LBRACKETPERCENT
  | LBRACKETLESS
  | LBRACKETGREATER
  | LBRACKETBAR
  | LBRACKETATATAT
  | LBRACKETATAT
  | LBRACKETAT
  | LBRACKET
  | LBRACELESS
  | LBRACE
  | LAZY
  | LABEL of string
  | INT of (string * char option)
  | INITIALIZER
  | INHERIT
  | INFIXOP4 of string
  | INFIXOP3 of string
  | INFIXOP2 of string
  | INFIXOP1 of string
  | INFIXOP0 of string
  | INCLUDE
  | IN
  | IF
  | HASHOP of string
  | HASH
  | GREATERRBRACKET
  | GREATERRBRACE
  | GREATER
  | FUNCTOR
  | FUNCTION
  | FUN
  | FOR
  | FLOAT of (string * char option)
  | FALSE
  | EXTERNAL
  | EXCEPTION
  | EQUAL
  | EOL
  | EOF
  | END
  | ELSE
  | DOWNTO
  | DOTOP of string
  | DOTDOT
  | DOT
  | DONE
  | DOCSTRING of Docstrings.docstring
  | DO
  | CONSTRAINT
  | COMMENT of (string * Location.t)
  | COMMA
  | COLONGREATER
  | COLONEQUAL
  | COLONCOLON
  | COLON
  | CLASS
  | CHAR of char
  | BEGIN
  | BARRBRACKET
  | BARBAR
  | BAR
  | BANG
  | BACKQUOTE
  | ASSERT
  | AS
  | ANDOP of string
  | AND
  | AMPERSAND
  | AMPERAMPER

let of_compiler_libs x = x
