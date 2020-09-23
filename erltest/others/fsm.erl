-module(fsm).

-compile([export_all]).

    Tstr_type Rec
      type_declaration valid/81 (fsm.ml[1,0+0]..fsm.ml[1,0+10])
        ptype_params =
        ptype_cstrs =
        ptype_kind =
          Ttype_abstract
        ptype_private = Public
        ptype_manifest =
          None
    Tstr_type Rec
      type_declaration invalid/82 (fsm.ml[2,11+0]..fsm.ml[2,11+12])
        ptype_params =
        ptype_cstrs =
        ptype_kind =
          Ttype_abstract
        ptype_private = Public
        ptype_manifest =
          None
    Tstr_type Rec
      type_declaration state/83 (fsm.ml[4,25+0]..fsm.ml[4,25+44])
        ptype_params =
            core_type (fsm.ml[4,25+5]..fsm.ml[4,25+7])
              Ttyp_var a
        ptype_cstrs =
        ptype_kind =
          Ttype_record
              (fsm.ml[4,25+18]..fsm.ml[4,25+31])
                Immutable
                counter/84                core_type (fsm.ml[4,25+27]..fsm.ml[4,25+30])
                  Ttyp_poly
                  core_type (fsm.ml[4,25+27]..fsm.ml[4,25+30])
                    Ttyp_constr Int
              (fsm.ml[4,25+32]..fsm.ml[4,25+42])
                Immutable
                flag/85                core_type (fsm.ml[4,25+38]..fsm.ml[4,25+42])
                  Ttyp_poly
                  core_type (fsm.ml[4,25+38]..fsm.ml[4,25+42])
                    Ttyp_constr Bool
        ptype_private = Public
        ptype_manifest =
          None
          Texp_constraint
          core_type (fsm.ml[6,71+12]..fsm.ml[6,71+20])
            Ttyp_constr State
              core_type (fsm.ml[6,71+12]..fsm.ml[6,71+14])
                Ttyp_var a
            Texp_record
              fields =
                [
                  "counter"
                      Texp_constant Const_int 0
                  "flag"
                      Texp_construct "false"
                ]
              representation =
                Record_regular
              extended_expression =
                None
          Texp_constraint
          core_type (fsm.ml[8,125+11]..fsm.ml[8,125+32])
            Ttyp_arrow
            Nolabel
            core_type (fsm.ml[8,125+11]..fsm.ml[8,125+15])
              Ttyp_constr Unit
            core_type (fsm.ml[8,125+19]..fsm.ml[8,125+32])
              Ttyp_constr State
                core_type (fsm.ml[8,125+19]..fsm.ml[8,125+26])
                  Ttyp_constr Invalid
make ->   Empty.
          Texp_constraint
          core_type (fsm.ml[10,177+15]..fsm.ml[10,177+60])
            Ttyp_arrow
            Nolabel
            core_type (fsm.ml[10,177+15]..fsm.ml[10,177+28])
              Ttyp_constr State
                core_type (fsm.ml[10,177+15]..fsm.ml[10,177+22])
                  Ttyp_constr Invalid
            core_type (fsm.ml[10,177+32]..fsm.ml[10,177+60])
              Ttyp_constr erlang:result
                core_type (fsm.ml[10,177+33]..fsm.ml[10,177+44])
                  Ttyp_constr State
                    core_type (fsm.ml[10,177+33]..fsm.ml[10,177+38])
                      Ttyp_constr Valid
                core_type (fsm.ml[10,177+46]..fsm.ml[10,177+52])
                  Ttyp_constr String
validate ->   Texp_construct "Ok"
      Texp_record
        fields =
          [
            "counter"
                Counter            "flag"
                Flag          ]
        representation =
          Record_regular
        extended_expression =
          None
.
          Texp_constraint
          core_type (fsm.ml[13,289+10]..fsm.ml[13,289+29])
            Ttyp_arrow
            Nolabel
            core_type (fsm.ml[13,289+10]..fsm.ml[13,289+21])
              Ttyp_constr State
                core_type (fsm.ml[13,289+10]..fsm.ml[13,289+15])
                  Ttyp_constr Valid
            core_type (fsm.ml[13,289+25]..fsm.ml[13,289+29])
              Ttyp_constr Unit
run ->   Texp_sequence
  erlang:print_string(_)    Texp_sequence
  erlang:print_int(Counter)      Texp_sequence
  erlang:print_newline(_)        Texp_construct "()"
.
  erlang:|>(_, _)
