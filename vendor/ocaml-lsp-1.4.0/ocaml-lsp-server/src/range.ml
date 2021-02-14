open Import
include Lsp.Types.Range

(* Compares ranges by their lengths*)
let compare_size (x : t) (y : t) =
  let dx = Position.(x.end_ - x.start) in
  let dy = Position.(y.end_ - y.start) in
  Stdune.Tuple.T2.compare Int.compare Int.compare (dx.line, dy.line)
    (dx.character, dy.character)

let first_line =
  let start = { Position.line = 1; character = 0 } in
  let end_ = { Position.line = 2; character = 0 } in
  { start; end_ }

let of_loc (loc : Loc.t) : t =
  (let open Option.O in
  let* start = Position.of_lexical_position loc.loc_start in
  let+ end_ = Position.of_lexical_position loc.loc_end in
  { start; end_ })
  |> Option.value ~default:first_line
