module P = Parser
module I = P.MenhirInterpreter
open Compat
open Migrate_ast

module R =
  Merlin_recovery.Make
    (I)
    (struct
      include Parser_recover

      let default_value loc x =
        Default.default_loc := loc;
        default_value x

      let guide _ = false
    end)

type 'a positioned = 'a * Lexing.position * Lexing.position

module type PARSE_INTF = sig
  type 'a parser

  val initial :
    (Lexing.position -> 'a I.checkpoint) -> Lexing.position -> 'a parser

  type 'a step = Intermediate of 'a parser | Success of 'a | Error

  val step : 'a parser -> P.token positioned -> 'a step
end

module type RECOVER_INTF = sig
  include PARSE_INTF

  (* Interface for recovery *)
  val recover : 'a I.checkpoint -> 'a parser

  val recovery_env : 'a parser -> 'a I.env
end

module Without_recovery : RECOVER_INTF = struct
  type 'a parser = 'a I.checkpoint

  let initial entrypoint position = entrypoint position

  type 'a step = Intermediate of 'a parser | Success of 'a | Error

  let rec normalize = function
    | I.InputNeeded _ as cp -> Intermediate cp
    | I.Accepted x -> Success x
    | I.HandlingError _ | I.Rejected -> Error
    | (I.Shifting (_, _, _) | I.AboutToReduce (_, _)) as cp ->
        normalize (I.resume cp)

  let step cp token = normalize (I.offer cp token)

  let recover = function I.InputNeeded _ as cp -> cp | _ -> assert false

  let recovery_env = function I.InputNeeded env -> env | _ -> assert false
end

module With_recovery : PARSE_INTF = struct
  module M = Without_recovery

  type 'a parser = Correct of 'a M.parser | Recovering of 'a R.candidates

  let initial entry_point position = Correct (M.initial entry_point position)

  type 'a step = Intermediate of 'a parser | Success of 'a | Error

  let step parser token =
    match parser with
    | Correct parser -> (
        match M.step parser token with
        | M.Intermediate parser -> Intermediate (Correct parser)
        | M.Success x -> Success x
        | M.Error ->
            let env = M.recovery_env parser in
            Intermediate (Recovering (R.generate env)) )
    | Recovering candidates -> (
        match R.attempt candidates token with
        | `Ok (cp, _) -> Intermediate (Correct (M.recover cp))
        | `Accept x -> Success x
        | `Fail -> (
            match token with
            | Parser.EOF, _, _ -> (
                match candidates.final with
                | None -> Error
                | Some x -> Success x )
            | _ -> Intermediate parser ) )
end

let entrypoint (type o p) : (o, p) Mapper.fragment -> _ -> o I.checkpoint =
  function
  | Mapper.Structure -> P.Incremental.implementation
  | Mapper.Signature -> P.Incremental.interface
  | Mapper.Use_file -> P.Incremental.use_file

let parse_with_recovery fragment tokens =
  let module P = With_recovery in
  let rec step tokens = function
    | P.Error -> failwith "Parsing failed"
    | P.Success x -> x
    | P.Intermediate p -> offer p tokens
  and offer p tokens =
    let token, rest =
      match tokens with
      | [] -> ((Parser.EOF, Lexing.dummy_pos, Lexing.dummy_pos), [])
      | token :: rest -> (token, rest)
    in
    step rest (P.step p token)
  in
  offer (P.initial (entrypoint fragment) Lexing.dummy_pos) tokens

let lex_buf lexbuf =
  Lexer.init ();
  let rec loop acc =
    match Lexer.token lexbuf with
    | exception Lexer.Error _ -> loop acc
    | token -> (
        let acc = (token, lexbuf.lex_start_p, lexbuf.lex_curr_p) :: acc in
        match token with Parser.EOF -> List.rev acc | _ -> loop acc )
  in
  loop []

let merge_adj merge l =
  List.fold_left
    (fun acc x ->
      match acc with
      | h :: t -> (
          match merge h x with Some m -> m :: t | None -> x :: h :: t )
      | [] -> x :: acc)
    [] l
  |> List.rev

let normalize_locs locs =
  List.sort_uniq Location.compare locs |> merge_adj Location.merge

module State = struct
  type t =
    { result : Location.t list
    ; stack : Location.t list
    }

  let result s = s.result

  let empty =
    { result = []
    ; stack = []
    }

  let add s =
    { s with result = List.hd s.stack :: s.result }

  let add_if cond s =
    if cond then
      add s
    else
      s

  let push_on_stack loc s =
    {s with stack = loc::s.stack}

  let pop_stack s =
    {s with stack = List.tl s.stack}

  let wrap loc f x s =
    match s.stack with
    | [] ->
      push_on_stack loc s
      |> f x
      |> pop_stack
    | _ -> f x s
end

let invalid_locs_fold =
  object
    inherit [State.t] Ppxlib.Ast_traverse.fold as super

    method! attribute x s =
      State.add_if (Annot.Attr.is_generated x) s
      |> super#attribute x

    method! expression e s =
      State.add_if (Annot.Exp.is_generated e) s
      |> super#expression e

    method! class_expr x s =
      State.add_if (Annot.Class_exp.is_generated x) s
      |> super#class_expr x

    method! structure_item x s =
      State.wrap x.pstr_loc super#structure_item x s

    method! signature_item x s =
      State.wrap x.psig_loc super#signature_item x s
  end

let invalid_locs fragment lexbuf =
  lex_buf lexbuf
  |> parse_with_recovery fragment
  |> Mapper.to_ppxlib fragment
  |> Mapper.fold_ast fragment invalid_locs_fold State.empty
  |> State.result
  |> normalize_locs

let mk_parsable fragment source =
  let lexbuf = Lexing.from_string source in
  match invalid_locs fragment lexbuf with
  | [] -> source
  | invalid_locs -> (
      (* we could be smarter here and choose a [delim] that does not appear in the
         source. *)
      let delim = "_i_n_v_a_l_i_d_" in
      let extension_name = "%%invalid.ast.node" in
      let wrapper_opn, wrapper_cls =
        (Printf.sprintf "[%s {%s|" extension_name delim, Printf.sprintf "|%s}]" delim)
      in
      let wrapper_len = String.length wrapper_opn + String.length wrapper_cls in
      let source_len = String.length source in
      let len =
        source_len + (List.length invalid_locs * wrapper_len)
      in
      let buffer = Buffer.create len in
      let rec loop pos locs =
        match locs with
        | [] ->
          Buffer.add_substring buffer source pos (source_len - pos);
          Buffer.contents buffer
        | (h : Location.t) :: t ->
          let col_start = h.loc_start.pos_cnum in
          let col_end = h.loc_end.pos_cnum in
          assert (pos <= col_start);
          assert (col_start < col_end);
          assert (col_end <= source_len);
          assert (col_end < source_len || List.length t = 0);
          Buffer.add_substring buffer source pos (col_start - pos);
          Buffer.add_string buffer wrapper_opn;
          Buffer.add_substring buffer source col_start (col_end - col_start);
          Buffer.add_string buffer wrapper_cls;
          loop col_end t
      in
      loop 0 invalid_locs
    )

module Invalid_locations = struct
  let structure = invalid_locs Structure

  let signature = invalid_locs Signature

  let use_file = invalid_locs Use_file
end

module Make_parsable = struct
  let structure = mk_parsable Structure

  let signature = mk_parsable Signature

  let use_file = mk_parsable Use_file
end
