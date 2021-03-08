(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

module type CONFIG = sig
  type config

  val profile_option_names : string list

  val warn : config -> ('a, Format.formatter, unit, unit) format4 -> 'a
end

module Make (C : CONFIG) = struct
  open Cmdliner

  type config = C.config

  type parsed_from = [`File of Fpath.t * int | `Attribute]

  type updated_from = [`Env | `Commandline | `Parsed of parsed_from]

  type from =
    [`Default | `Profile of string * updated_from | `Updated of updated_from]

  type deprecated = {msg: string; since_version: string}

  type 'a t =
    { names: string list
    ; parse: string -> ('a, [`Msg of string]) Result.t
    ; update: config -> 'a -> config
    ; allow_inline: bool
    ; cmdline_get: unit -> 'a option
    ; to_string: 'a -> string
    ; default: 'a
    ; get_value: config -> 'a
    ; from: from
    ; removed: bool
    ; deprecated: deprecated option }

  type 'a option_decl =
       names:string list
    -> doc:string
    -> section:[`Formatting | `Operational]
    -> ?allow_inline:bool
    -> ?deprecated:deprecated
    -> (config -> 'a -> config)
    -> (config -> 'a)
    -> 'a t

  type pack = Pack : 'a t -> pack

  let store = ref []

  let deprecated ~since_version msg = {msg; since_version}

  let in_attributes ~section cond =
    if cond || Poly.(section = `Operational) then ""
    else " Cannot be set in attributes."

  let pp_deprecated ppf {msg; since_version} =
    Format.fprintf ppf "This option is deprecated since version %s. %s"
      since_version msg

  let deprecated_doc ppf deprecated =
    match deprecated with
    | Some deprecated ->
        Format.fprintf ppf " Warning: %a" pp_deprecated deprecated
    | None -> ()

  let generated_flag_doc ~allow_inline ~doc ~section ~default ~deprecated =
    let default = if default then "set" else "unset" in
    Format.asprintf "%s The flag is $(b,%s) by default.%s%a" doc default
      (in_attributes ~section allow_inline)
      deprecated_doc deprecated

  let generated_doc conv ~allow_inline ~doc ~section ~default ~deprecated =
    let default = Format.asprintf "%a" (Arg.conv_printer conv) default in
    let default = if String.is_empty default then "none" else default in
    Format.asprintf "%s The default value is $(b,%s).%s%a" doc default
      (in_attributes ~section allow_inline)
      deprecated_doc deprecated

  let section_name = function
    | `Formatting -> Cmdliner.Manpage.s_options ^ " (CODE FORMATTING STYLE)"
    | `Operational -> Cmdliner.Manpage.s_options
    | `Removed -> Cmdliner.Manpage.s_options ^ " (REMOVED OPTIONS)"

  let from = `Default

  let flag ~default ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) ?deprecated update
      get_value =
    let open Cmdliner in
    let invert_names =
      List.filter_map names ~f:(fun n ->
          if String.length n = 1 then None else Some ("no-" ^ n) )
    in
    let doc =
      generated_flag_doc ~allow_inline ~doc ~section ~default ~deprecated
    in
    let invert_doc = "Unset $(b," ^ List.last_exn names ^ ")." in
    let docs = section_name section in
    let term =
      Arg.(
        value
        & vflag None
            [ (Some true, info names ~doc ~docs)
            ; (Some false, info invert_names ~doc:invert_doc ~docs) ])
    in
    let parse = Arg.(conv_parser bool) in
    let r = mk ~default:None term in
    let to_string = Bool.to_string in
    let cmdline_get () = !r in
    let opt =
      { names
      ; parse
      ; update
      ; cmdline_get
      ; allow_inline
      ; default
      ; to_string
      ; get_value
      ; from
      ; removed= false
      ; deprecated }
    in
    store := Pack opt :: !store ;
    opt

  let any converter ~default ~docv ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) ?deprecated update
      get_value =
    let open Cmdliner in
    let doc =
      generated_doc converter ~allow_inline ~doc ~section ~default
        ~deprecated
    in
    let docs = section_name section in
    let term =
      Arg.(value & opt (some converter) None & info names ~doc ~docs ~docv)
    in
    let parse s = Arg.conv_parser converter s in
    let r = mk ~default:None term in
    let to_string = Format.asprintf "%a%!" (Arg.conv_printer converter) in
    let cmdline_get () = !r in
    let opt =
      { names
      ; parse
      ; update
      ; cmdline_get
      ; allow_inline
      ; default
      ; to_string
      ; get_value
      ; from
      ; removed= false
      ; deprecated }
    in
    store := Pack opt :: !store ;
    opt

  type removed_value = {name: string; version: string; msg: string}

  let removed_value ~name ~version ~msg = {name; version; msg}

  let removed_values ~names ~version ~msg =
    List.map names ~f:(fun name -> removed_value ~name ~version ~msg)

  let add_removed_values values conv =
    let parse s =
      match List.find values ~f:(fun {name; _} -> String.equal name s) with
      | Some {name; version; msg} ->
          Format.kasprintf
            (fun s -> Error (`Msg s))
            "value `%s` has been removed in version %s. %s" name version msg
      | None -> Arg.conv_parser conv s
    in
    Arg.conv (parse, Arg.conv_printer conv)

  let choice ~all ?(removed_values = []) ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) ?deprecated =
    let _, default, _ = List.hd_exn all in
    let opt_names = List.map all ~f:(fun (x, y, _) -> (x, y)) in
    let conv = add_removed_values removed_values (Arg.enum opt_names) in
    let doc =
      let open Format in
      asprintf "%s %a" doc
        (pp_print_list
           ~pp_sep:(fun fs () -> fprintf fs "@,")
           (fun fs (_, _, d) -> fprintf fs "%s" d) )
        all
    in
    let docv =
      let open Format in
      asprintf "@[<1>{%a}@]"
        (pp_print_list
           ~pp_sep:(fun fs () -> fprintf fs "@,|")
           (fun fs (v, _, _) -> fprintf fs "%s" v) )
        all
    in
    any conv ~default ~docv ~names ~doc ~section ~allow_inline ?deprecated

  let removed_option ~names ~version ~msg =
    let msg =
      Format.asprintf "This option has been removed in version %s. %s"
        version msg
    in
    let parse _ = Error (`Msg msg) in
    let converter = Arg.conv (parse, fun _ () -> ()) in
    let update conf _ = conf and get_value _ = () in
    let docs = section_name `Removed in
    let term =
      Arg.(value & opt (some converter) None & info names ~doc:msg ~docs)
    in
    let r = mk ~default:None term in
    let to_string _ = "" in
    let cmdline_get () = !r in
    let opt =
      { names
      ; parse
      ; update
      ; cmdline_get
      ; allow_inline= true
      ; default= ()
      ; to_string
      ; get_value
      ; from
      ; removed= true
      ; deprecated= None }
    in
    store := Pack opt :: !store

  let update_from config name from =
    let is_profile_option_name x =
      List.exists C.profile_option_names ~f:(String.equal x)
    in
    let on_pack (Pack {names; get_value; to_string; _}) =
      if is_profile_option_name (List.hd_exn names) then
        Some (to_string (get_value config))
      else None
    in
    let on_pack (Pack ({names; deprecated; _} as p)) =
      if is_profile_option_name name then
        if is_profile_option_name (List.hd_exn names) then
          (* updating --profile option *)
          Pack {p with from= `Updated from}
        else
          let profile_name = List.find_map_exn !store ~f:on_pack in
          (* updating other options when --profile is set *)
          Pack {p with from= `Profile (profile_name, from)}
      else if List.exists names ~f:(String.equal name) then (
        (* updating a single option (without setting a profile) *)
        Option.iter deprecated ~f:(C.warn config "%s: %a" name pp_deprecated) ;
        Pack {p with from= `Updated from} )
      else Pack p
    in
    store := List.map !store ~f:on_pack

  let update ~config ~from ~name ~value ~inline =
    List.find_map !store
      ~f:(fun (Pack {names; parse; update; allow_inline; _}) ->
        if List.exists names ~f:(String.equal name) then
          if inline && not allow_inline then
            Some (Error (`Misplaced (name, value)))
          else
            match parse value with
            | Ok packed_value ->
                let config = update config packed_value in
                update_from config name from ;
                Some (Ok config)
            | Error (`Msg error) -> Some (Error (`Bad_value (name, error)))
        else None )
    |> Option.value ~default:(Error (`Unknown (name, value)))

  let default {default; _} = default

  let update_using_cmdline config =
    let on_pack config (Pack {cmdline_get; update; names; _}) =
      match cmdline_get () with
      | None -> config
      | Some x ->
          let config = update config x in
          update_from config (List.hd_exn names) `Commandline ;
          config
    in
    List.fold !store ~init:config ~f:on_pack

  let print_config c =
    let longest =
      let compare x y = compare (String.length x) (String.length y) in
      List.max_elt ~compare
    in
    let on_pack (Pack {names; to_string; get_value; from; removed; _}) =
      let name = Option.value_exn (longest names) in
      let value = to_string (get_value c) in
      let aux_from = function
        | `Parsed (`File (p, i)) ->
            Format.sprintf " (file %s:%i)"
              (Fpath.to_string ~relativize:true p)
              i
        | `Parsed `Attribute -> " (attribute)"
        | `Env -> " (environment variable)"
        | `Commandline -> " (command line)"
      in
      let aux_from = function
        | `Default -> ""
        | `Profile (s, p) -> " (profile " ^ s ^ aux_from p ^ ")"
        | `Updated x -> aux_from x
      in
      if not removed then
        Format.eprintf "%s=%s%s\n%!" name value (aux_from from)
    in
    List.iter !store ~f:on_pack
end
