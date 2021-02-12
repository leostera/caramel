%{
  open Ts_types
  open Ts_types.Unresolved
  open! Import
%}

%token Const
%token Enum
%token Extends
%token Interface
%token Namespace
%token Readonly
%token Type
%token L_curly R_curly
%token L_square R_square
%token L_angle R_angle
%token R_paren L_paren
%token Semicolon
%token Comma
%token<string> Ident
%token Colon
%token Alt
%token <float> Float
%token <int> Int
%token <string> String
%token Array_type
%token Question
%token Equal
%token Eof

%type <Ts_types.Literal.t> lit
%type <Ts_types.Unresolved.typ> typ

%start <Ts_types.Unresolved.t list> main

%%

let field_name :=
  | i = Ident; { i }
  | Type; { "type" }

let field :=
  | Readonly?; name = field_name; q = Question?; Colon; t = toplevel_typ; Semicolon?;
    { let optional = Option.is_some q in
      named_field ~optional t name }
  | L_square
    ; name = field_name; Colon; pat = toplevel_typ
    ; R_square; Colon; typ = toplevel_typ ; Semicolon?;
    { pattern_field ~name ~pat ~typ
    }

let lit :=
  | lit = String; { Literal.String lit }
  | lit = Int; { Literal.Int lit }
  | lit = Float; {Literal.Float lit }

let fields :=
  | L_curly
    ; fields = list(field)
    ; Semicolon?
    ; R_curly ; { fields }

let typ :=
  | l = lit; { Literal l }
  | ident = Ident; { Ident ident }
  | sum = delimited(L_paren, separated_nonempty_list(Alt, typ), R_paren);
    { Sum sum }
  | t = typ ; Array_type; { List t }
  | ~ = fields; { Record fields }
  | t = typ ; a = delimited (L_angle, typ, R_angle); { App (t, a) }
  | typs = delimited(L_square, separated_nonempty_list(Comma, typ), R_square);
    { Tuple typs }

let toplevel_typ ==
  | types = separated_nonempty_list(Alt, typ); { Sum types }
  | ~ = typ; { typ }

let extends := Extends; separated_list(Comma, Ident)

let params := idents = delimited(L_angle, separated_list(Comma, Ident), R_angle); { idents }

let interface :=
  | Interface; name = Ident
    ; params = params?
    ; extends = extends?
    ; ~ = fields;
    { let extends = match extends with None -> [] | Some xs -> xs in
      let params = match params with None -> [] | Some xs -> xs in
      interface ~name ~extends ~fields ~params
    }

let const_constr :=
  | Const; name = Ident; Equal; ~ = lit; Semicolon;
    { (name, lit)
    }
  | Const; name = Ident; Colon; Ident; Equal; ~ = lit; Semicolon;
    { (name, lit)
    }
  | Const; name = Ident; Colon; lit; Equal; ~ = lit; Semicolon;
    { (name, lit)
    }

let enum_constr := name = Ident; Equal; v = lit; { (name, v) }
let enum_constrs := separated_nonempty_list(Comma, enum_constr)

let enum :=
  | Enum; name = Ident
    ; L_curly; constrs = enum_constrs ; R_curly;
    { enum ~name ~constrs
    }
  | Namespace; name = Ident ; L_curly
    ; constrs = list(const_constr)
    ; R_curly ;
    { enum ~name ~constrs
    }

let type_decl :=
  | Type; name = Ident; Equal; typ = toplevel_typ; Semicolon?;
    { Named.make ~name typ }

let definition :=
  | i = interface; { { i with Named.data = Interface i.Named.data } }
  | i = enum; { { i with Named.data = Enum_anon i.Named.data } }
  | i = type_decl; { { i with Named.data = Type i.Named.data } }

let main :=
  | defs = definition*; Eof; { defs }
