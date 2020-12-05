let pp = Format.printf

let () =
  pp "tree-sitter v%d\n%!" (Tree_sitter.language_version ());

  let p = Tree_sitter.Parser.make () in
  pp "old parser timeout in micros is: %d\n%!"
    (Tree_sitter.Parser.timeout_micros p);

  Tree_sitter.Parser.set_timeout_micros p 2112L;

  pp "new parser timeout in micros is: %d\n%!"
    (Tree_sitter.Parser.timeout_micros p);

  pp "Getting sexp language...%!";
  let sexp_lang = Tree_sitter_sexp.language () in
  pp "OK";
  pp "\n%!";

  pp "Language version: %d\n%!" (Tree_sitter.Language.version sexp_lang);

  let node_count = Tree_sitter.Language.node_kind_count sexp_lang in
  pp "Node kind count: %d\n%!" node_count;
  pp "Nodes: \n%!";

  for i = 0 to node_count do
    match (Tree_sitter.Language.node_kind_for_id sexp_lang i) with
    | None -> ()
    | Some s -> pp "  #%d: %s\n%!" i s
  done;

  pp "Setting language...%!";
  let () =
    match Tree_sitter.Parser.set_language p sexp_lang with
    | Ok _ -> pp "OK"
    | Error _ -> pp "ERR"
  in
  pp "\n%!";

  let sample = "(hello (world))" in

  pp "Parsing...%!";
  let tree =
    match Tree_sitter.Parser.parse p sample None with
    | None ->
        pp "NO TREE";
        exit 1
    | Some tree -> tree
  in
  pp "\n%!";

  let root = Tree_sitter.Tree.root_node tree in

  pp "Printing...\n\n%!";
  pp "%s%!" (Tree_sitter.Node.to_sexp root);
  pp "\nOK\n%!"
