external raw : unit -> Tree_sitter.Language.raw = "tree_sitter_sexp"

let language () = Tree_sitter.Language.from_raw_ptr (raw ())
