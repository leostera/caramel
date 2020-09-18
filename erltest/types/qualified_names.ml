type compound = {
  c_a: Record.inlined_record ;
  c_b: bool Type_args.phantom ;
  c_c: (int, bool) Type_args.triplet ;
  c_d: Record.large_record ;
  c_fn: Record.small_record * int Fn.defer;
}

