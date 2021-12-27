open Cmdliner

let no_stdlib =
  Arg.(
    value & flag
    & info [ "no-stdlib" ] ~docv:"NO_STDLIB"
        ~doc:
          "Use this flag to compile sources without opening the Standard \
           Library by default.")

let stdlib_path =
  Arg.(
    value & opt string "./"
    & info [ "stdlib-path" ] ~env:(env_var "CARAMEL_STDLIB_PATH"))

let dump_pass =
  Arg.(value & opt int (-1) & info [ "p"; "dump-pass" ] ~docv:"DUMP_PASS")

let print_time = Arg.(value & flag & info [ "print_time" ] ~docv:"PRINT_TIME")

let dump name =
  Arg.(
    value & flag
    & info [ "dump-" ^ name ] ~docv:("DUMP_" ^ String.uppercase_ascii name))

let debug = Arg.(value & flag & info [ "d"; "debug" ] ~docv:"DEBUG")
