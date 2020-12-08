let pp = Format.printf

let unwrap x = match x with None -> exit 1 | Some y -> y

let mk_parser ts =
  let p = Tree_sitter.Parser.make () in
  Tree_sitter.Parser.set_language p ts;
  p

let inspect_parser p =
  let lang = Tree_sitter.Parser.language p |> unwrap in
  pp "tree-sitter v%d\n%!" (Tree_sitter.language_version ());
  pp "Language version: %d\n%!" (Tree_sitter.Language.version lang);
  let node_count = Tree_sitter.Language.node_kind_count lang in
  pp "Node kind count: %d\n%!" node_count;
  pp "Nodes: \n%!";

  for i = 0 to node_count do
    match Tree_sitter.Language.node_kind_for_id lang i with
    | None -> ()
    | Some s -> pp "  #%d: %s\n%!" i s
  done;

  pp "DONE\n%!"

let rec print_all_nodes cursor sample =
  let node = Tree_sitter.Tree.Cursor.node cursor in

  pp "%s\n%!" (Tree_sitter.Node.utf8_text node sample);

  if Tree_sitter.Tree.Cursor.goto_first_child cursor then
    print_all_nodes cursor sample
  else if Tree_sitter.Tree.Cursor.goto_next_sibling cursor then
    print_all_nodes cursor sample
  else pp "DONE\n%!"

let () =
  let p = Tree_sitter_sexp.language () |> mk_parser in
  inspect_parser p;

  let sample = "(hello (world (this is working (i think))))" in

  let tree = Tree_sitter.Parser.parse p sample None |> unwrap in

  let root = Tree_sitter.Tree.root_node tree in

  let walker = Tree_sitter.Node.walk root in

  Tree_sitter.Tree.Cursor.goto_first_child walker |> ignore;

  print_all_nodes walker sample
