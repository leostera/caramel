open Import
open Jsonrpc

type t =
  { ic : in_channel
  ; oc : out_channel
  }

let close_in { ic; oc = _ } = close_in_noerr ic

let close_out { ic = _; oc } = close_out_noerr oc

let close t =
  close_in t;
  close_out t

let make ic oc =
  set_binary_mode_in ic true;
  set_binary_mode_out oc true;
  { ic; oc }

let send { oc; ic = _ } (packet : packet) =
  let json = Jsonrpc.yojson_of_packet packet in
  let data = Json.to_string json in
  let content_length = String.length data in
  let header = Header.create ~content_length in
  Header.write header oc;
  output_string oc data;
  flush oc

let read_content ic =
  match Header.read ic with
  | exception Sys_error _ -> None
  | exception End_of_file -> None
  | header ->
    let len = Header.content_length header in
    let buffer = Bytes.create len in
    let rec read_loop read =
      if read < len then
        let n = input ic buffer read (len - read) in
        read_loop (read + n)
    in
    let () = read_loop 0 in
    Some (Bytes.to_string buffer)

let read { ic; oc = _ } : Json.t option =
  read_content ic |> Option.map ~f:Json.of_string

let read (t : t) : packet option =
  let open Option.O in
  let+ json = read t in
  let open Json.O in
  let req json = Message (Jsonrpc.Message.either_of_yojson json) in
  let resp json = Response (Jsonrpc.Response.t_of_yojson json) in
  (req <|> resp) json
