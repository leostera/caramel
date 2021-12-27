open Caramel_sugarcane
open Sugarcane.B

module Pp_utils = struct
  let sep_by char ppf () = Format.fprintf ppf char

  let newline = sep_by "@;"

  let comma = sep_by ",@ "

  let wrap l r ppf fn =
    Format.fprintf ppf "%s" l;
    fn ppf;
    Format.fprintf ppf "%s" r

  let list ppf (pp' : Format.formatter -> 'a -> unit) (head : 'a) (tail : 'a) =
    Format.fprintf ppf "@[<hov 0>[@[<hov 0>@;";
    Format.fprintf ppf "@[%a@]@,|@,@[%a@]" pp' head pp' tail;
    Format.fprintf ppf "@]@;]@]"

  let tuple ppf (pp' : Format.formatter -> 'a -> unit) (parts : 'a list) =
    Format.fprintf ppf "@[<hov 0>{@[<hov 0>@,";
    Format.pp_print_list ~pp_sep:comma pp' ppf parts;
    Format.fprintf ppf "@]@,}@]"
end

let rec pp ppf b =
  match b with
  | Module { name; defs; exports; _ } ->
      Format.fprintf ppf "@[module '%s'@]@;" name;

      Format.fprintf ppf "@[<v 0>";
      pp_exports ppf exports;
      Format.fprintf ppf "@;@]";

      Format.fprintf ppf "@[<v 0>attributes []@;@]";

      Format.fprintf ppf "@[<v 0>";
      Format.pp_print_list ~pp_sep:Pp_utils.newline pp_def ppf defs;
      Format.fprintf ppf "@;@]";

      Format.fprintf ppf "@[end@."
  | Apply { fn; args } -> pp_apply ppf fn args
  | Call { mod_; fun_; args } -> pp_call ppf mod_ fun_ args
  | Case { cond; cases } -> pp_case ppf cond cases
  | Fun fn -> pp_fun ppf fn
  | Fun_ref fn_name -> pp_fn_name ppf fn_name
  | Let { value_list; expr; body } -> pp_let ppf value_list expr body
  | Literal lit -> pp_lit ppf lit
  | Map fields -> pp_map ppf fields
  | Var name -> pp_var ppf name
  | Seq (a, b) -> pp_seq ppf a b
  | Binary bin -> pp_binary ppf bin
  | Tuple parts -> Pp_utils.tuple ppf pp parts
  | List (h, t) -> Pp_utils.list ppf pp h t
  | Catch t -> pp_catch ppf t
  | Try { expr; try_value_list; body; catch_value_list; catch_expr } ->
      pp_try ppf expr try_value_list body catch_value_list catch_expr
  | _ ->
      Format.fprintf Format.std_formatter "%a\n" Sugarcane.B.pp b;
      Error.todo "unsupported expression"

and pp_pat_wrap ppf pat =
  Format.fprintf ppf "@[<hov 0><@[<hov 0>@,";
  pp_pat ppf pat;
  Format.fprintf ppf "@]@,>@]"

and pp_pat ppf pat =
  match pat with
  | Pat_char c -> pp_char ppf c
  | Pat_ignore -> Format.fprintf ppf "@[_@]"
  | Pat_atom a -> pp_atom ppf a
  | Pat_bind v -> pp_var ppf v
  | Pat_nil -> Format.fprintf ppf "@[[]@]"
  | Pat_binary bin -> pp_binary ppf bin
  | Pat_tuple parts -> Pp_utils.tuple ppf pp_pat parts
  | Pat_cons (h, t) -> Pp_utils.list ppf pp_pat h t
  | Pat_int i -> Format.fprintf ppf "%s" i
  | Pat_float f -> Format.fprintf ppf "%s" f

and pp_lit ppf lit =
  match lit with
  | Lit_atom s -> pp_atom ppf s
  | Lit_char c -> pp_char ppf c
  | Lit_cons (h, t) -> Pp_utils.list ppf pp_lit h t
  | Lit_nil -> Format.fprintf ppf "@[[]@]"
  | Lit_int s | Lit_float s -> Format.fprintf ppf "%s" s
  | Lit_tuple parts -> Pp_utils.tuple ppf pp_lit parts

and pp_apply ppf fn args =
  Format.fprintf ppf "@[<hov 2>@[<h 2>apply @[%a@](@]@,@[<hov 2>" pp fn;
  Format.pp_print_list ~pp_sep:Pp_utils.comma
    (fun ppf arg -> Format.fprintf ppf "@[%a@]" pp arg)
    ppf args;
  Format.fprintf ppf "@])@]"

and pp_atom ppf a = Format.fprintf ppf "'%s'" a

and pp_binary ppf bin =
  Format.fprintf ppf "@[<v 0>#{@[<v 0>@;";
  Format.pp_print_list ~pp_sep:Pp_utils.comma
    (fun ppf c ->
      Format.fprintf ppf "@[#<%d>(8,1,'integer',['unsigned'|['big']])@]"
        (Char.code c))
    ppf
    (List.of_seq (String.to_seq bin));
  Format.fprintf ppf "@]@,}#@]"

and pp_branch ppf (pat, t) =
  (* NOTE: lift guards to B.ml *)
  Format.fprintf ppf
    "@[<hov 2>@[<h 2>@[%a@] when 'true'@[@ ->@]@]@ @[<hov 2>%a@]@]" pp_pat_wrap
    pat pp t

and pp_call ppf mod_ fun_ args =
  Format.fprintf ppf "@[<hov 2>@[<h 0>call@ '%s':'%s'(@]@,@[<hov 0>" mod_ fun_;
  Format.pp_print_list ~pp_sep:Pp_utils.comma
    (fun ppf arg -> Format.fprintf ppf "@[%a@]" pp arg)
    ppf args;
  Format.fprintf ppf "@,@])@]@,"

and pp_case ppf cond cases =
  Format.fprintf ppf "@[<v 0>@[<h 0>case@ @[%a@]@ of@]@;@[" pp cond;
  Format.pp_print_list ~pp_sep:Pp_utils.newline pp_branch ppf cases;
  Format.fprintf ppf "@]@,end@]"

and pp_catch ppf t = Format.fprintf ppf "@[catch@,@[%a@]" pp t

and pp_char ppf c = Format.fprintf ppf "%d" (Char.code c)

and pp_def ppf { df_name; df_body } =
  (* NOTE(@ostera): after the = can we get the line break to have 2 spaces? *)
  Format.fprintf ppf "@;@[<hov 1>%a =@;%a@]" pp_fn_name df_name pp df_body

and pp_exports ppf exports =
  Format.fprintf ppf "@[<v 1>[@;";
  Format.pp_print_list ~pp_sep:Pp_utils.comma pp_fn_name ppf exports;
  Format.fprintf ppf "@]@;]"

and pp_fn_name ppf (name, arity) = Format.fprintf ppf "@['%s'/%d@]" name arity

and pp_fun ppf { args; body } =
  Format.fprintf ppf "@[(fun @[<hov 2>(";
  Format.pp_print_list ~pp_sep:Pp_utils.comma pp_var ppf args;
  Format.fprintf ppf ")@]";
  Format.fprintf ppf "@[ ->@]@;";
  Format.fprintf ppf "@[<hov 2>@,%a@]" pp body;
  Format.fprintf ppf " -| [])@]"

and pp_let ppf value_list expr body =
  Format.fprintf ppf "@[<hov 1>@[let %a = %a@;@]in@]@;@[%a@]" pp_value_list
    value_list pp expr pp body

and pp_map ppf fields =
  Format.fprintf ppf "@[<v 0>~{@[<v 0>@;";
  Format.pp_print_list ~pp_sep:Pp_utils.comma
    (fun ppf (k, v) -> Format.fprintf ppf "@[%a@] => @[%a@]" pp k pp v)
    ppf fields;
  Format.fprintf ppf "@]@,}~@]"

and pp_value_list ppf value_list =
  Format.fprintf ppf "@[<hov 0><@[<hov 0>@,";
  if value_list = [] then Format.fprintf ppf "_"
  else Format.pp_print_list ~pp_sep:Pp_utils.comma pp_var ppf value_list;
  Format.fprintf ppf "@]@,>@]"

and pp_var ppf s = Format.fprintf ppf "@[%s@]" (String.capitalize_ascii s)

and pp_seq ppf a b =
  Format.fprintf ppf "@[<hov 2>do@ @[<hov 0>@[%a@]@;@[%a@]@]@]" pp a pp b

and pp_try ppf expr try_value_list body catch_value_list catch_expr =
  Format.fprintf ppf "@[<v 2>";
  Format.fprintf ppf "@[<v 2>@[try@]@;@[%a@]@;@]@[of@]@;" pp expr;
  Format.fprintf ppf "@[<v 2>@[<h 2>@[%a@]@[@ ->@ @]@;@[%a@]@]@;@]"
    pp_value_list try_value_list pp body;
  Format.fprintf ppf "@[<v 2>@[catch@]@,@[<h 2>@[%a@]@[@ ->@ @]@;@]@]"
    pp_value_list catch_value_list;
  Format.fprintf ppf "@[<hov 2>%a@]" pp catch_expr;
  Format.fprintf ppf "@]"

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
