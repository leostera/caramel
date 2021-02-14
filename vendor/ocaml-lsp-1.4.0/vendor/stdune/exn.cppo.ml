module List = Stdlib.ListLabels
module String = Stdlib.StringLabels

type t = exn

external raise : exn -> _ = "%raise"

external raise_notrace : exn -> _ = "%raise_notrace"

external reraise : exn -> _ = "%reraise"

let protectx x ~f ~finally =
  match f x with
  | y ->
    finally x;
    y
  | exception e ->
    finally x;
    raise e

let protect ~f ~finally = protectx () ~f ~finally

let pp_uncaught ~backtrace fmt exn =
  let s =
    Printf.sprintf "%s\n%s" (Printexc.to_string exn) backtrace
    |> String_split.split_lines
    |> ListLabels.map ~f:(Printf.sprintf "| %s")
    |> String.concat ~sep:"\n"
  in
  let line = String.make 71 '-' in
  Format.fprintf fmt
    "/%s\n| @{<error>Internal error@}: Uncaught exception.\n%s\n\\%s@." line s
    line

let pp fmt exn = Format.pp_print_string fmt (Printexc.to_string exn)

(* 4.06 support for Stdune/exn.ml

 On linux, raise_with_backtrace threw `Error: junk
 `raise_with_backtrace (%rip)' after expression`
 We support 4.06 by rely on reraise on 4.06

 Possibly related:
 https://github.com/ocaml/ocaml/pull/1557
 https://github.com/ocaml/ocaml/pull/1465
 https://github.com/ocaml/ocaml/pull/378
 https://github.com/ocaml/ocaml/issues/7666 *)

#if OCAML_VERSION > (4, 06, 1)
let raise_with_backtrace = Printexc.raise_with_backtrace
#else
let raise_with_backtrace exn _ = reraise exn
#endif

let equal = ( = )

let hash = Stdlib.Hashtbl.hash

let to_dyn exn = Dyn.String (Printexc.to_string exn)
