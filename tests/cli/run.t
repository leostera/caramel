Calling compile without inputs will error out.
  $ caramel compile
  caramel: required argument SOURCES is missing
  Usage: caramel compile [OPTION]... SOURCES...
  Try `caramel compile --help' or `caramel --help' for more information.
  [1]

Calling compile with uncompilable files will error out.
  $ caramel compile dummy.txt
  Attempted to compile dummy.txt, but .txt files are not supported with the target flag: --target=erlang
  [1]

  $ cat >to_format.ml <<EOF
  > 
  > let 
  > 
  >  () = 1
  > 
  >   + 2
  >   
  > 
  > ;;
  > 
  > EOF
  $ cat to_format.ml
  
  let 
  
   () = 1
  
    + 2
    
  
  ;;
  
  $ caramel fmt to_format.ml
  $ cat to_format.ml
  let () = 1 + 2
