(* let run log_file = Ocaml_lsp_server.run ~log_file

   let () = let open Cmdliner in Printexc.record_backtrace true;

   let log_file = let open Arg in let doc = "Enable logging to file (pass `-'
   for logging to stderr)" in let env = env_var "OCAML_LSP_SERVER_LOG" in value
   & opt (some string) None & info [ "log-file" ] ~docv:"FILE" ~doc ~env in

   let cmd = let doc = "Start OCaml LSP server (only stdio transport is
   supported)" in let version = Version.get () in ( Term.(const run $ log_file)
   , Term.info "ocamllsp" ~version ~doc ~exits:Term.default_exits ) in

   Term.(exit @@ eval cmd) *)
