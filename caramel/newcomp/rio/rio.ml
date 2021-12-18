open Caramel_sugarcane
open Sugarcane.B

module Pp_utils = struct
  let sep_by char ppf () = Format.fprintf ppf char

  let newline = sep_by "\n"

  let comma = sep_by ","

  let wrap l r ppf fn =
    Format.fprintf ppf "%s" l;
    fn ppf;
    Format.fprintf ppf "%s" r
end

let rec pp ppf b =
  match b with
  | Module { name; defs; _ } ->
      Format.fprintf ppf "module '%s' [] attributes []\n" name;
      Format.pp_print_list ~pp_sep:Pp_utils.newline pp_def ppf defs;
      Format.fprintf ppf "end\n"
  | Apply { fn; args } -> pp_apply ppf fn args
  | Call { mod_; fun_; args } -> pp_call ppf mod_ fun_ args
  | Case { cond; cases } -> pp_case ppf cond cases
  | Fun fn -> pp_fun ppf fn
  | Let { value_list; expr; body } -> pp_let ppf value_list expr body
  | Literal lit -> pp_lit ppf lit
  | Map fields -> pp_map ppf fields
  | Var name -> Format.fprintf ppf "(_%s -| [])" name
  | _ ->
      Format.fprintf Format.std_formatter "%a\n" Sugarcane.B.pp b;
      Error.todo "unsupported expression"

and pp_pat ppf pat =
  match pat with
  | Pat_ignore -> Format.fprintf ppf "_"
  | _ -> Format.fprintf ppf "_"

and pp_apply ppf fn args =
  Format.fprintf ppf "call %a" pp fn;
  Format.fprintf ppf "(";
  Format.pp_print_list ~pp_sep:Pp_utils.comma
    (fun ppf arg -> Format.fprintf ppf "%a" pp arg)
    ppf args;
  Format.fprintf ppf ")"

and pp_case ppf cond cases =
  Format.fprintf ppf "case %a of\n" pp cond;
  Format.pp_print_list ~pp_sep:Pp_utils.newline pp_branch ppf cases;
  Format.fprintf ppf "end\n"

and pp_branch ppf (pat, t) =
  Format.fprintf ppf "< %a > when 'true' -> %a\n" pp_pat pat pp t

and pp_value_list ppf value_list =
  Format.fprintf ppf "<";
  Format.pp_print_list ~pp_sep:Pp_utils.comma
    (fun ppf v -> Format.fprintf ppf "%s" v)
    ppf value_list;
  Format.fprintf ppf ">"

and pp_let ppf value_list expr body =
  Format.fprintf ppf "let %a = %a in %a" pp_value_list value_list pp expr pp
    body

and pp_call ppf mod_ fun_ args =
  Format.fprintf ppf "call '%s':'%s'" mod_ fun_;
  Format.fprintf ppf "(";
  Format.pp_print_list ~pp_sep:Pp_utils.comma
    (fun ppf arg -> Format.fprintf ppf "%a" pp arg)
    ppf args;
  Format.fprintf ppf ")"

and pp_lit ppf lit =
  match lit with
  | Lit_atom s | Lit_char s -> Format.fprintf ppf "'%s'" s
  | Lit_cons (h, t) -> Format.fprintf ppf "[ %a | %a ]" pp_lit h pp_lit t
  | Lit_nil -> Format.fprintf ppf "[]"
  | Lit_int s | Lit_float s -> Format.fprintf ppf "%s" s
  | Lit_tuple parts ->
      Format.fprintf ppf "{";
      Format.pp_print_list ~pp_sep:Pp_utils.comma
        (fun ppf v -> Format.fprintf ppf "%a" pp_lit v)
        ppf parts;
      Format.fprintf ppf "}"

and pp_map ppf fields =
  Format.fprintf ppf "~{ ";
  Format.pp_print_list ~pp_sep:Pp_utils.comma
    (fun ppf (k, v) -> Format.fprintf ppf "%a=>%a" pp k pp v)
    ppf fields;
  Format.fprintf ppf " }~"

and pp_fun ppf { args; body } =
  Format.fprintf ppf "( fun (";
  Format.pp_print_list ~pp_sep:Pp_utils.comma
    (fun ppf s -> Format.fprintf ppf "_%s" s)
    ppf args;
  Format.fprintf ppf ") -> %a -| [])" pp body

and pp_def ppf { df_name; df_body } =
  Format.fprintf ppf "%a = %a" pp_fn_name df_name pp df_body

and pp_fn_name ppf (name, arity) = Format.fprintf ppf "'%s'/%d" name arity

let codegen ~tunit:_ ~b =
  List.iter
    (fun e ->
      match e with
      | Module { name; _ } ->
          let filename = name ^ ".core" in
          Logs.debug (fun f -> f "Writing %s" filename);
          let oc = open_out_bin filename in
          (try
             let ppf = Format.formatter_of_out_channel oc in
             Format.fprintf ppf "%% Source code generated with Caramel.\n";
             Format.fprintf ppf "%a\n%!" pp e
           with _ -> Sys.remove filename);
          close_out oc;
          Logs.debug (fun f -> f "OK")
      | _ -> Error.panic "we can't codegen without a module!")
    b
