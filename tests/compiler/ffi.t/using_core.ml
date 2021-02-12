let print thing = Io.format "~0tp~n" [ thing ]
let main _ =
  print (1 = 1) ;
  print (1 <> 1) ;
  print (1 < 1) ;
  print (1 > 1) ;
  print (1 <= 1) ;
  print (1 >= 1) ;
  print (1 == 1) ;
  print (1 != 1) ;
  print (not true) ;
  print (true && false) ;
  print (true || false) ;
  print ([1] @ [1]) ;
  print (1 + 1) ;
  print (1 - 1) ;
  print (1 * 1) ;
  print (1 / 1) ;
  print (1 mod 1) ;
  print (1.0 +. 1.0) ;
  print (1.0 -. 1.0) ;
  print (1.0 *. 1.0) ;
  print (1.0 /. 1.0) ;
  print (ceil 1.1) ;
  print (floor 1.1) ;
  print (abs_float 1.1) ;
  print (float 1) ;
  print (float_of_int 1) ;
  print (truncate 1.1) ;
  print (int_of_float 1.1) ;
  ()