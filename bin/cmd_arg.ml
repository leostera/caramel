open Cmdliner

type t = {
  (* compile option *)
  input_files : string;
}

let options =
  let open Term in
  let t = pure (`Ok { input_files="test" })  in
  Term.ret t
