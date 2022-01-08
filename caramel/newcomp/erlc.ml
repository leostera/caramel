let erlc = Bos.Cmd.v "erlc"

let compile core_files =
  let args = Bos.Cmd.of_list core_files in
  let cmd = Bos.Cmd.(erlc %% args) in
  Bos.OS.Cmd.run cmd
