let print thing = Io.format "~0tp~n" [ thing ]
let main _ =
  print (Math.sqrt 4.0) ;
  print (Math.exp 4.0) ;
  print (Math.log 4.0) ;
  print (Math.log10 4.0) ;
  print (Math.cos 4.0) ;
  print (Math.sin 4.0) ;
  print (Math.tan 4.0) ;
  print (Math.acos 1.0) ;
  print (Math.asin 1.0) ;
  print (Math.atan 4.0) ;
  print (Math.atan2 4.0 4.0) ;
  print (Math.cosh 4.0) ;
  print (Math.sinh 4.0) ;
  print (Math.tanh 4.0) ;
