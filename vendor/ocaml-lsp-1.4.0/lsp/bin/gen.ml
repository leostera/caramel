open Stdune
open Lsp_gen

module Output = struct
  type t =
    | Test
    | Ocaml

  let of_string = function
    | "test" -> Some Test
    | "ocaml" -> Some Ocaml
    | _ -> None

  let arg () =
    let field = ref None in
    let name = "--out" in
    let set s =
      match of_string s with
      | Some f -> field := Some f
      | None -> raise (Arg.Bad (sprintf "invalid output mode %s" s))
    in
    ( (name, Arg.String set, "test | ocaml")
    , lazy
        (match !field with
        | Some f -> f
        | None -> raise (Arg.Bad (sprintf "%s not set" name))) )
end

let () =
  let md_file = ref None in
  let out_arg, out = Output.arg () in
  let args =
    [ ( "--md"
      , Arg.String (fun s -> md_file := Some s)
      , "markdown file containing specification" )
    ; out_arg
    ]
  in
  let anon s = raise (Arg.Bad (sprintf "don't know what to do with %s" s)) in
  let usage =
    sprintf "%s --md [FILE] --out [test | ocaml]"
      (Filename.basename Sys.executable_name)
  in
  Arg.parse args anon usage;
  let md_file = Option.value_exn !md_file in
  let ch = open_in md_file in
  let lexing = Lexing.from_channel ch in
  let typescript = Markdown.read_typescript lexing in
  match Lazy.force out with
  | Test ->
    let tests = Typescript.test_snippets typescript in
    Format.printf "%a%!" Typescript.pp_results tests
  | Ocaml -> failwith "not supported anymore"
