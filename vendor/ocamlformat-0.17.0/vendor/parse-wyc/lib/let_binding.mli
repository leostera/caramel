open Migrate_ast

type let_binding = {
  lb_pattern : Parsetree.pattern;
  lb_expression : Parsetree.expression;
  lb_attributes : Parsetree.attributes;
  lb_docs : Docstrings.docs Lazy.t;
  lb_text : Docstrings.text Lazy.t;
  lb_loc : Location.t;
}

type let_bindings = {
  lbs_bindings : let_binding list;
  lbs_rec : Asttypes.rec_flag;
  lbs_extension : string Asttypes.loc option;
  lbs_loc : Location.t;
}
