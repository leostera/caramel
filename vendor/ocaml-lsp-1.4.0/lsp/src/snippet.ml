open! Import

module Var = struct
  type t =
    | TM_SELECTED_TEXT
    | TM_CURRENT_LINE
    | TM_CURRENT_WORD
    | TM_LINE_INDEX
    | TM_LINE_NUMBER
    | TM_FILENAME
    | TM_FILENAME_BASE
    | TM_DIRECTORY
    | TM_FILEPATH

  let to_string = function
    | TM_SELECTED_TEXT -> "TM_SELECTED_TEXT"
    | TM_CURRENT_LINE -> "TM_CURRENT_LINE"
    | TM_CURRENT_WORD -> "TM_CURRENT_WORD"
    | TM_LINE_INDEX -> "TM_LINE_INDEX"
    | TM_LINE_NUMBER -> "TM_LINE_NUMBER"
    | TM_FILENAME -> "TM_FILENAME"
    | TM_FILENAME_BASE -> "TM_FILENAME_BASE"
    | TM_DIRECTORY -> "TM_DIRECTORY"
    | TM_FILEPATH -> "TM_FILEPATH"
end

type variable_transform =
  { regex : string
  ; format_string : string
  ; regex_options : string option
  }

type t =
  | Tabstop of
      int option * [ `Placeholder of t | `Choice of string list | `None ]
  | Variable of
      Var.t * [ `Placeholder of t | `Transform of variable_transform | `None ]
  | Text of string
  | Concat of t * t

let tabstop index = Tabstop (Some index, `None)

let placeholder ?index content = Tabstop (index, `Placeholder content)

let choice ?index values =
  if List.is_empty values then
    Code_error.raise "choice must have non empty values" []
  else
    Tabstop (index, `Choice values)

let variable ?(opt = `None) var = Variable (var, opt)

let variable_transform ~regex ?regex_options ~format_string () =
  { regex; regex_options; format_string }

let text str = Text str

module O = struct
  let ( ^^ ) lhs rhs = Concat (lhs, rhs)

  let ( @+ ) lhs_str rhs = Concat (Text lhs_str, rhs)

  let ( +@ ) lhs rhs_str = Concat (lhs, Text rhs_str)
end

let concat ?sep ts =
  let rec go = function
    | [] -> text ""
    | [ x ] -> x
    | x :: rest -> (
      match sep with
      | None -> Concat (x, go rest)
      | Some s -> Concat (x, Concat (s, go rest)))
  in
  go ts

let escape ?(in_choice = false) str =
  let str =
    str
    |> String.replace_all ~pattern:"$" ~with_:"\\$"
    |> String.replace_all ~pattern:"}" ~with_:"\\}"
    |> String.replace_all ~pattern:"\\" ~with_:"\\\\"
  in
  if not in_choice then
    str
  else
    str
    |> String.replace_all ~pattern:"," ~with_:"\\,"
    |> String.replace_all ~pattern:"|" ~with_:"\\|"

type ctx = int * int Int.Map.t

let pp_impl add_string (snippet : t) : unit =
  let open Format in
  let with_ctx ctx i_opt f : ctx =
    let n, m = ctx in
    match i_opt with
    | None ->
      add_string (f n);
      (n + 1, m)
    | Some i -> (
      match Int.Map.find m i with
      | Some j ->
        add_string (f j);
        (n, m)
      | None ->
        add_string (f n);
        (n + 1, Int.Map.set m i n))
  in
  let rec go ctx = function
    | Text s ->
      add_string s;
      ctx
    | Concat (l, r) -> go (go ctx l) r
    | Tabstop (i, `None) -> with_ctx ctx i (fun i -> sprintf "$%d" i)
    | Tabstop (i, `Placeholder s) ->
      let ctx = go (with_ctx ctx i (fun i -> sprintf "${%d:" i)) s in
      add_string "}";
      ctx
    | Tabstop (i, `Choice values) ->
      with_ctx ctx i (fun i ->
          sprintf "${%d|%s|}" i
            (values
            |> List.map ~f:(escape ~in_choice:true)
            |> String.concat ~sep:","))
    | Variable (var, opt) -> (
      let var = Var.to_string var in
      match opt with
      | `None ->
        add_string (sprintf "$%s" var);
        ctx
      | `Placeholder s ->
        add_string (sprintf "${%s:" var);
        let ctx = go ctx s in
        add_string "}";
        ctx
      | `Transform t ->
        sprintf "${%s/%s/%s/%s}" var t.regex t.format_string
          (Option.value ~default:"" t.regex_options)
        |> add_string;
        ctx)
  in
  go (1, Int.Map.empty) snippet |> ignore

let to_string t =
  let buf = Buffer.create 0 in
  pp_impl (Buffer.add_string buf) t;
  Buffer.contents buf

let pp (chan : Format.formatter) (t : t) : unit =
  pp_impl (Format.pp_print_string chan) t
