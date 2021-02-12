let print thing = Io.format "~0tp~n" [ thing ]
let main _ =
  print (1 + 1) ;
  print (1 - 1) ;
  print (1 * 1) ;
  print (1 / 1) ;
  print (1.0 +. 1.0) ;
  print (1.0 -. 1.0) ;
  print (1.0 *. 1.0) ;
  print (1.0 /. 1.0) ;
  ()