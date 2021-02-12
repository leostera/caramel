(***********************************************************************)
(* OMD: Markdown tool in OCaml                                         *)
(* (c) 2014 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence:  ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

type html = html_node list

and html_node =
  | Node of nodename * attributes * html
  | Data of string
  | Rawdata of string
  | Comment of string

and nodename = string

and attributes = attribute list

and attribute = string * string option

let to_string html =
  let b = Buffer.create 1024 in
  let pp f = Printf.bprintf b f in
  let rec loop = function
    | Node(nodename, attributes, html) ->
      pp "<%s" nodename;
      ppa attributes;
      pp ">";
      List.iter loop html;
      pp "</%s>" nodename
    | Data s -> pp "%s" s
    | Rawdata s -> pp "%s" s
    | Comment c -> pp "<!-- %s -->" c
  and ppa attrs =
    List.iter
      (function
        | (a, Some v) ->
          if not (String.contains v '\'') then
            pp " %s='%s'" a v
          else if not (String.contains v '"') then
            pp " %s=\"%s\"" a v
          else
            (
              pp " %s=\"" a;
              for i = 0 to String.length v - 1 do
                match v.[i] with
                | '"' -> pp "&quot;"
                | c    -> pp "%c" c
              done;
              pp "\""
            )
        | a, None ->
          Printf.bprintf b " %s=''" a (* HTML5 *)
    )
    attrs
  in
  List.iter loop html;
  Buffer.contents b


