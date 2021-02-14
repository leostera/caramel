(* {{{ COPYING *(

   This file is part of Merlin, an helper for ocaml editors

   Copyright (C) 2013 - 2015 Frédéric Bour <frederic.bour(_)lakaban.net> Thomas
   Refis <refis.thomas(_)gmail.com> Simon Castellan <simon.castellan(_)iuwt.fr>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   The Software is provided "as is", without warranty of any kind, express or
   implied, including but not limited to the warranties of merchantability,
   fitness for a particular purpose and noninfringement. In no event shall the
   authors or copyright holders be liable for any claim, damages or other
   liability, whether in an action of contract, tort or otherwise, arising from,
   out of or in connection with the software or the use or other dealings in the
   Software.

   )* }}} *)

open Import

module Title = struct
  type t =
    | Error
    | Warning
    | Info
    | Debug
    | LocalDebug
    | Notify
    | Custom of string

  let to_string = function
    | Error -> "error"
    | Warning -> "warn"
    | Info -> "info"
    | Debug -> "debug"
    | LocalDebug -> "debug (local)"
    | Notify -> "notify"
    | Custom s -> s
end

let time = ref 0.0

let delta_time () = Sys.time () -. !time

let destination = ref None

let selected_sections = ref None

let is_section_enabled section =
  match !selected_sections with
  | None -> true
  | Some sections -> Table.mem sections section

let output_section oc section title =
  Printf.fprintf oc "# %2.2f %s - %s\n" (delta_time ()) section
    (Title.to_string title)

let log_flush () =
  match !destination with
  | None -> ()
  | Some oc -> flush oc

let consumer : (string * Title.t * string -> unit) option ref = ref None

let consumer_buffer : (string * Title.t * string) list ref = ref []

let push_to_consumer ~section ~title text =
  match !consumer with
  | None -> consumer_buffer := (section, title, text) :: !consumer_buffer
  | Some f ->
    List.rev !consumer_buffer |> List.iter ~f;
    f (section, title, text)

let register_consumer f = consumer := Some f

let log ~section ~title fmt =
  match !destination with
  | Some oc when is_section_enabled section ->
    Printf.ksprintf
      (fun str ->
        output_section oc section title;
        if str <> "" then (
          output_string oc str;
          push_to_consumer ~section ~title str;
          if str.[String.length str - 1] <> '\n' then output_char oc '\n'
        );
        flush oc)
      fmt
  | None
  | Some _ ->
    Printf.ksprintf
      (fun str -> if str <> "" then push_to_consumer ~section ~title str)
      fmt

let fmt_buffer = Buffer.create 128

let fmt_handle = Format.formatter_of_buffer fmt_buffer

let fmt () f =
  Buffer.reset fmt_buffer;
  (match f fmt_handle with
  | () -> ()
  | exception exn ->
    Format.fprintf fmt_handle "@\nException: %s" (Printexc.to_string exn));
  Format.pp_print_flush fmt_handle ();
  let msg = Buffer.contents fmt_buffer in
  Buffer.reset fmt_buffer;
  msg

let json () f =
  match f () with
  | json -> Yojson.pretty_to_string json
  | exception exn -> Printf.sprintf "Exception: %s" (Printexc.to_string exn)

let exn () exn = Printexc.to_string exn

type notification =
  { section : string
  ; msg : string
  }

let notifications : notification list ref option ref = ref None

let notify ~section =
  let tell msg =
    log ~section ~title:Title.Notify "%s" msg;
    match !notifications with
    | None -> ()
    | Some r -> r := { section; msg } :: !r
  in
  Printf.ksprintf tell

let with_notifications r f = let_ref notifications (Some r) f

let with_sections sections f =
  let sections =
    match sections with
    | [] -> None
    | sections ->
      let table = Table.create (module String) (List.length sections) in
      List.iter sections ~f:(fun section -> Table.set table section ());
      Some table
  in
  let sections0 = !selected_sections in
  selected_sections := sections;
  match f () with
  | result ->
    selected_sections := sections0;
    result
  | exception exn ->
    selected_sections := sections0;
    Exn.reraise exn

let with_log_file file ?(sections = []) f =
  match file with
  | None -> with_sections sections f
  | Some file -> (
    log_flush ();
    let destination', release =
      match file with
      | "" -> (None, ignore)
      | "-" -> (Some stderr, ignore)
      | filename -> (
        match open_out filename with
        | exception exn ->
          Printf.eprintf "cannot open %S for logging: %s" filename
            (Printexc.to_string exn);
          (None, ignore)
        | oc -> (Some oc, fun () -> close_out_noerr oc))
    in
    let destination0 = !destination in
    destination := destination';
    let release () =
      log_flush ();
      destination := destination0;
      release ()
    in
    match with_sections sections f with
    | v ->
      release ();
      v
    | exception exn ->
      release ();
      Exn.reraise exn)

type 'a printf = title:Title.t -> ('a, unit, string, unit) format4 -> 'a

type logger = { log : 'a. 'a printf }

let for_section section = { log = (fun ~title fmt -> log ~section ~title fmt) }
