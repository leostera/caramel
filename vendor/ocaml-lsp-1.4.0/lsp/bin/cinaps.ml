let ocaml =
  lazy
    (let lexing = Lexing.from_string Spec._3_15 in
     let typescript = Markdown.read_typescript lexing in
     let asts = Typescript.of_snippets typescript in
     let asts = Typescript.resolve_all asts in
     Ocaml.of_typescript asts)

let print_ml () = Ocaml.output (Lazy.force ocaml) ~kind:Impl stdout

let print_mli () = Ocaml.output (Lazy.force ocaml) ~kind:Intf stdout
