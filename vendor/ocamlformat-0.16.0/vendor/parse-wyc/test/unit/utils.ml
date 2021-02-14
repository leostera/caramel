module Position = struct
  open Lexing

  let repr p = (p.pos_lnum, p.pos_cnum - p.pos_bol)
end

module Location = struct
  open Location

  let repr l = (Position.repr l.loc_start, Position.repr l.loc_end)
end

let check_locs f ~name ~input ~locs =
  let lexbuf = Lexing.from_string input in
  let actual = List.map Location.repr (f lexbuf) in
  let ty = Alcotest.(list (pair (pair int int) (pair int int))) in
  Alcotest.check ty name locs actual

let check_use_file = check_locs Parse_wyc.Invalid_locations.use_file

let check_impl = check_locs Parse_wyc.Invalid_locations.structure

let check_intf = check_locs Parse_wyc.Invalid_locations.signature
