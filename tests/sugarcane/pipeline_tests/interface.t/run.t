  $ caramel compile --sugarcane --debug a.mli
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (a.mli))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file a.mli) (source_kind intf))
  
  caramel: [DEBUG] Writing a.mli.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.mli.typedtree
  caramel: [DEBUG] OK

  $ cat a.mli.parsetree
  [
    signature_item (a.mli[1,0+0]..[1,0+19])
      Psig_value
      value_description "x" (a.mli[1,0+4]..[1,0+5]) (a.mli[1,0+0]..[1,0+19])
        core_type (a.mli[1,0+8]..[1,0+19])
          Ptyp_arrow
          Nolabel
          core_type (a.mli[1,0+8]..[1,0+12])
            Ptyp_constr "unit" (a.mli[1,0+8]..[1,0+12])
            []
          core_type (a.mli[1,0+16]..[1,0+19])
            Ptyp_constr "int" (a.mli[1,0+16]..[1,0+19])
            []
        []
    signature_item (a.mli[3,21+0]..[13,110+3])
      Psig_module "B" (a.mli[3,21+7]..[3,21+8])
      module_type (a.mli[3,21+11]..[13,110+3])
        Pmty_signature
        [
          signature_item (a.mli[5,37+2]..[5,37+21])
            Psig_value
            value_description "y" (a.mli[5,37+6]..[5,37+7]) (a.mli[5,37+2]..[5,37+21])
              core_type (a.mli[5,37+10]..[5,37+21])
                Ptyp_arrow
                Nolabel
                core_type (a.mli[5,37+10]..[5,37+14])
                  Ptyp_constr "unit" (a.mli[5,37+10]..[5,37+14])
                  []
                core_type (a.mli[5,37+18]..[5,37+21])
                  Ptyp_constr "int" (a.mli[5,37+18]..[5,37+21])
                  []
              []
          signature_item (a.mli[7,60+2]..[11,103+5])
            Psig_module "C" (a.mli[7,60+9]..[7,60+10])
            module_type (a.mli[7,60+13]..[11,103+5])
              Pmty_signature
              [
                signature_item (a.mli[9,78+4]..[9,78+23])
                  Psig_value
                  value_description "z" (a.mli[9,78+8]..[9,78+9]) (a.mli[9,78+4]..[9,78+23])
                    core_type (a.mli[9,78+12]..[9,78+23])
                      Ptyp_arrow
                      Nolabel
                      core_type (a.mli[9,78+12]..[9,78+16])
                        Ptyp_constr "unit" (a.mli[9,78+12]..[9,78+16])
                        []
                      core_type (a.mli[9,78+20]..[9,78+23])
                        Ptyp_constr "int" (a.mli[9,78+20]..[9,78+23])
                        []
                    []
              ]
        ]
  ]

  $ cat a.mli.typedtree
  [
    signature_item (a.mli[1,0+0]..a.mli[1,0+19])
      Tsig_value
      value_description x/3 (a.mli[1,0+0]..a.mli[1,0+19])
        core_type (a.mli[1,0+8]..a.mli[1,0+19])
          Ttyp_arrow
          Nolabel
          core_type (a.mli[1,0+8]..a.mli[1,0+12])
            Ttyp_constr "unit/6!"
            []
          core_type (a.mli[1,0+16]..a.mli[1,0+19])
            Ttyp_constr "int/1!"
            []
        []
    signature_item (a.mli[3,21+0]..a.mli[13,110+3])
      Tsig_module "B/7"
      module_type (a.mli[3,21+11]..a.mli[13,110+3])
        Tmty_signature
        [
          signature_item (a.mli[5,37+2]..a.mli[5,37+21])
            Tsig_value
            value_description y/4 (a.mli[5,37+2]..a.mli[5,37+21])
              core_type (a.mli[5,37+10]..a.mli[5,37+21])
                Ttyp_arrow
                Nolabel
                core_type (a.mli[5,37+10]..a.mli[5,37+14])
                  Ttyp_constr "unit/6!"
                  []
                core_type (a.mli[5,37+18]..a.mli[5,37+21])
                  Ttyp_constr "int/1!"
                  []
              []
          signature_item (a.mli[7,60+2]..a.mli[11,103+5])
            Tsig_module "C/6"
            module_type (a.mli[7,60+13]..a.mli[11,103+5])
              Tmty_signature
              [
                signature_item (a.mli[9,78+4]..a.mli[9,78+23])
                  Tsig_value
                  value_description z/5 (a.mli[9,78+4]..a.mli[9,78+23])
                    core_type (a.mli[9,78+12]..a.mli[9,78+23])
                      Ttyp_arrow
                      Nolabel
                      core_type (a.mli[9,78+12]..a.mli[9,78+16])
                        Ttyp_constr "unit/6!"
                        []
                      core_type (a.mli[9,78+20]..a.mli[9,78+23])
                        Ttyp_constr "int/1!"
                        []
                    []
              ]
        ]
  ]
