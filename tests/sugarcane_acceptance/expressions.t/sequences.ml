let direct_sequence x =
  x;
  x;
  x
let nested x =
  ( ( x; x ); x );
  begin begin x; x end; x end;
  x
