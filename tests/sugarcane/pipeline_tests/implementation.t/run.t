================================================================================

Test that the implementation pipeline works:

* We get a good parsetree
* We get a good lambda
* We get a good IR
* We get a good .core

  $ caramel compile --debug a.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (a.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file a.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing a.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing a.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing a.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.B.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.B.C.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done


  $ cat a.ml.parsetree
  [
    structure_item (a.ml[1,0+0]..[7,84+3])
      Pstr_module
      "B" (a.ml[1,0+7]..[1,0+8])
        module_expr (a.ml[1,0+11]..[7,84+3])
          Pmod_structure
          [
            structure_item (a.ml[2,18+2]..[4,57+5])
              Pstr_module
              "C" (a.ml[2,18+9]..[2,18+10])
                module_expr (a.ml[2,18+13]..[4,57+5])
                  Pmod_structure
                  [
                    structure_item (a.ml[3,38+4]..[3,38+18])
                      Pstr_value Nonrec
                      [
                        <def>
                          pattern (a.ml[3,38+8]..[3,38+9])
                            Ppat_var "z" (a.ml[3,38+8]..[3,38+9])
                          expression (a.ml[3,38+10]..[3,38+18]) ghost
                            Pexp_fun
                            Nolabel
                            None
                            pattern (a.ml[3,38+10]..[3,38+12])
                              Ppat_construct "()" (a.ml[3,38+10]..[3,38+12])
                              None
                            expression (a.ml[3,38+15]..[3,38+18])
                              Pexp_constant PConst_int (900,None)
                      ]
                  ]
            structure_item (a.ml[6,64+2]..[6,64+19])
              Pstr_value Nonrec
              [
                <def>
                  pattern (a.ml[6,64+6]..[6,64+7])
                    Ppat_var "y" (a.ml[6,64+6]..[6,64+7])
                  expression (a.ml[6,64+8]..[6,64+19]) ghost
                    Pexp_fun
                    Nolabel
                    None
                    pattern (a.ml[6,64+8]..[6,64+10])
                      Ppat_construct "()" (a.ml[6,64+8]..[6,64+10])
                      None
                    expression (a.ml[6,64+13]..[6,64+19])
                      Pexp_apply
                      expression (a.ml[6,64+13]..[6,64+16])
                        Pexp_ident "C.z" (a.ml[6,64+13]..[6,64+16])
                      [
                        <arg>
                        Nolabel
                          expression (a.ml[6,64+17]..[6,64+19])
                            Pexp_construct "()" (a.ml[6,64+17]..[6,64+19])
                            None
                      ]
              ]
          ]
    structure_item (a.ml[9,89+0]..[9,89+17])
      Pstr_value Nonrec
      [
        <def>
          pattern (a.ml[9,89+4]..[9,89+5])
            Ppat_var "x" (a.ml[9,89+4]..[9,89+5])
          expression (a.ml[9,89+6]..[9,89+17]) ghost
            Pexp_fun
            Nolabel
            None
            pattern (a.ml[9,89+6]..[9,89+8])
              Ppat_construct "()" (a.ml[9,89+6]..[9,89+8])
              None
            expression (a.ml[9,89+11]..[9,89+17])
              Pexp_apply
              expression (a.ml[9,89+11]..[9,89+14])
                Pexp_ident "B.y" (a.ml[9,89+11]..[9,89+14])
              [
                <arg>
                Nolabel
                  expression (a.ml[9,89+15]..[9,89+17])
                    Pexp_construct "()" (a.ml[9,89+15]..[9,89+17])
                    None
              ]
      ]
    structure_item (a.ml[11,108+0]..[11,108+15])
      Pstr_value Nonrec
      [
        <def>
          pattern (a.ml[11,108+4]..[11,108+5])
            Ppat_var "w" (a.ml[11,108+4]..[11,108+5])
          expression (a.ml[11,108+6]..[11,108+15]) ghost
            Pexp_fun
            Nolabel
            None
            pattern (a.ml[11,108+6]..[11,108+8])
              Ppat_construct "()" (a.ml[11,108+6]..[11,108+8])
              None
            expression (a.ml[11,108+11]..[11,108+15])
              Pexp_apply
              expression (a.ml[11,108+11]..[11,108+12])
                Pexp_ident "x" (a.ml[11,108+11]..[11,108+12])
              [
                <arg>
                Nolabel
                  expression (a.ml[11,108+13]..[11,108+15])
                    Pexp_construct "()" (a.ml[11,108+13]..[11,108+15])
                    None
              ]
      ]
  ]

  $ cat a.ml.lambda
  (let
    (B/10 =
       (module-defn(B/10) A a.ml(1):0-87
         (let
           (C/6 =
              (module-defn(C/6) A.B a.ml(2):20-62
                (let (z/3 = (function param/5 : int 900)) (makeblock 0 z/3)))
            y/7 = (function param/9 : int (apply (field 0 C/6) 0)))
           (makeblock 0 C/6 y/7)))
     x/11 = (function param/13 : int (apply (field 1 B/10) 0))
     w/14 = (function param/16 : int (apply x/11 0)))
    (makeblock 0 B/10 x/11 w/14))

  $ cat Caramel.A.b
  cat: Caramel.A.b: No such file or directory
  [1]

  $ cat Caramel.A.core
  % Source code generated with Caramel.
  module 'Caramel.A'
  [
   'w'/1,
   'x'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.A') -| [])
  
  'module_info'/1 =
   (fun (Opts) -> call 'erlang':'get_module_info'('Caramel.A', Opts) -| [])
  
  'w'/1 = (fun (Param) -> apply 'x'/1('unit') -| [])
  
  'x'/1 = (fun (Param) -> call 'Caramel.A.B':'y'('unit') -| [])
  end
  
