open Sexplib.Std

type atom = Atom of string [@@deriving sexp]

type comment = Comment of string [@@deriving sexp]
