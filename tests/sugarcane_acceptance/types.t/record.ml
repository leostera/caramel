type 'a record = { author : 'a list; year : int; related : 'a record option }

type inlined_record =
  | Simpler of bool
  | Many of bool * bool
  | Compound of { ir_a : float; ir_b : bool }

type small_record = { a : string }

type large_record = {
  lr_a : string;
  lr_b : string;
  lr_c : string;
  lr_d : string;
}
