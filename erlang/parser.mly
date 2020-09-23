/* The parser definition */
%{
open Asttypes
open Ast_helper
open Parsetree
open Longident

let unit =  Location.mkloc (Lident "()") Location.none

let main_call = {
  pstr_desc = Pstr_value (Nonrecursive, [Vb.mk (Pat.any ()) {
    pexp_desc = Pexp_apply (
      {   pexp_desc = Pexp_ident (Location.mkloc (Lident "main") Location.none);
          pexp_loc = Location.none;
          pexp_loc_stack = [];
          pexp_attributes = [];
      },
        [
          Nolabel,
          Exp.construct unit None
          ]
      );
   pexp_loc = Location.none;
   pexp_loc_stack = [];
   pexp_attributes = [];
  }]);
  pstr_loc = Location.none
  }

%}

/* Tokens */

%token DOT
%token COMMA
%token SEMICOLON
%token LEFT_PARENS
%token RIGHT_PARENS
%token ARROW
%token EQUAL_ARROW
%token BINARY_OPEN
%token BINARY_CLOSE
%token <string> STRING
%token <string> ATOM
%token <string> BINARY_STRING

%token EOL
%token EOF

/* Entry points */

%start implementation           /* for implementation files */
%type <_> implementation
%%

(* An .erl file. *)
implementation:
  module_item
    {
      let pstr_loc = Location.none in
      let pstr_desc = Pstr_value (Recursive, [$1]) in
      [{ pstr_desc; pstr_loc }; main_call]
    }
;

module_item:
  name = ATOM
  LEFT_PARENS
  RIGHT_PARENS
  ARROW
  expr
  DOT
    { { pvb_pat = { ppat_desc = Ppat_var (Location.mknoloc name);
                    ppat_loc = Location.none;
                    ppat_loc_stack = [];
                    ppat_attributes = [];
                  };
        pvb_expr = { pexp_desc = Pexp_function [
                      { pc_lhs = Pat.any ();
                        pc_guard = None;
                        pc_rhs = { pexp_desc = $5;
                                   pexp_loc = Location.none;
                                   pexp_loc_stack = [];
                                   pexp_attributes = [];
                                 }
                      }
                     ];
                     pexp_loc = Location.none;
                     pexp_loc_stack = [];
                     pexp_attributes = [];
                   };
        pvb_attributes = [];
        pvb_loc = Location.none
      }
    }
;

expr:
  function_call { $1 }
| binary { $1 }
;

binary :
  BINARY_OPEN
  str = STRING
  BINARY_CLOSE
  { Pexp_constant (Pconst_string (str, Location.none, None)) }
;

function_call:
  name = ATOM
  LEFT_PARENS
  binary
  RIGHT_PARENS
  { Pexp_apply (
    {   pexp_desc = Pexp_ident (Location.mkloc (Lident name) Location.none);
        pexp_loc = Location.none;
        pexp_loc_stack = [];
        pexp_attributes = [];
      },
      [
        Nolabel,
        { pexp_desc = $3;
          pexp_loc = Location.none;
          pexp_loc_stack = [];
          pexp_attributes = [];
        }
      ]
    )
  }
;
