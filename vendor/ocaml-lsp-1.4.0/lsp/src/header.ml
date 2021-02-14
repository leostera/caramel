open Import

(* TODO rewrite all this using faraday/angstrom *)

type t =
  { content_length : int
  ; content_type : string
  }

let content_type t = t.content_type

let content_length t = t.content_length

module Key = struct
  let content_length = "Content-Length"

  let content_type = "Content-Type"

  let content_length_lowercase = String.lowercase_ascii content_length

  let content_type_lowercase = String.lowercase_ascii content_type
end

let crlf = "\r\n"

let to_string { content_length; content_type } =
  let b = Buffer.create 64 in
  let add = Buffer.add_string b in
  let line k v =
    add k;
    add ": ";
    add v;
    add crlf
  in
  line Key.content_length (string_of_int content_length);
  line Key.content_type content_type;
  add crlf;
  Buffer.contents b

(* There's no good way to recover from these errors anyway *)
type error =
  | Invalid_header_line of string
  | Missing_content_length of t

exception Error of error

let empty = { content_length = -1; content_type = "" }

let write t oc = output_string oc (to_string t)

let default_content_type = "application/vscode-jsonrpc; charset=utf-8"

let read ic =
  let rec loop acc mark_content_type =
    match input_line ic with
    | "\r" -> (acc, mark_content_type)
    | line -> (
      match String.split_on_char ~sep:':' line with
      | [ k; v ] ->
        let k = String.lowercase_ascii (String.trim k) in
        if
          k = Key.content_length_lowercase
          && acc.content_length = empty.content_length
        then
          loop
            { acc with
              content_length =
                (match Int.of_string (String.trim v) with
                | Some i -> i
                | None -> raise (Error (Invalid_header_line line)))
            }
            mark_content_type
        else if
          k = Key.content_type_lowercase
          && acc.content_type = empty.content_type
        then
          loop { acc with content_type = String.trim v } true
        else
          loop acc mark_content_type
      | _ -> loop acc mark_content_type)
  in
  let t, mark_content_type = loop empty false in
  if t.content_length = empty.content_length then
    raise (Error (Missing_content_length t))
  else if mark_content_type then
    t
  else
    { t with content_type = default_content_type }

let create ~content_length =
  { content_length; content_type = default_content_type }
