  $ cat a.ml
  (* ocaml module *)
  let f x = x
  
  $ caramelc compile a.ml
  Compiling a.erl	OK
  $ cat a.erl
  % Source code generated with Caramel.
  -module(a).
  
  -export([f/1]).
  
  -spec f(A) -> A.
  f(X) -> X.
  
  
  $ caramelc check a.erl
  $ caramelc verify a.erl a.ml
  [
    structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
      Pstr_value Rec
      [
        <def>
          pattern (_none_[0,0+-1]..[0,0+-1]) ghost
            Ppat_var "f" (_none_[0,0+-1]..[0,0+-1]) ghost
          expression (_none_[0,0+-1]..[0,0+-1]) ghost
            Pexp_fun
            Nolabel
            None
            pattern (_none_[0,0+-1]..[0,0+-1]) ghost
              Ppat_var "x" (_none_[0,0+-1]..[0,0+-1]) ghost
            expression (_none_[0,0+-1]..[0,0+-1]) ghost
              Pexp_ident "x" (_none_[0,0+-1]..[0,0+-1]) ghost
      ]
  ]
  
  
  [
    structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
      Pstr_value Rec
      [
        <def>
          pattern (_none_[0,0+-1]..[0,0+-1]) ghost
            Ppat_var "f" (_none_[0,0+-1]..[0,0+-1]) ghost
          expression (_none_[0,0+-1]..[0,0+-1]) ghost
            Pexp_fun
            Nolabel
            None
            pattern (_none_[0,0+-1]..[0,0+-1]) ghost
              Ppat_var "x" (_none_[0,0+-1]..[0,0+-1]) ghost
            expression (_none_[0,0+-1]..[0,0+-1]) ghost
              Pexp_ident "x" (_none_[0,0+-1]..[0,0+-1]) ghost
      ]
  ]
  
  
  $ echo $?
  0
