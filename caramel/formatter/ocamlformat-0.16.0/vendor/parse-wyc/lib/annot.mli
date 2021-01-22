module Exp : sig
  val mk : unit -> Migrate_ast.Parsetree.expression

  val is_generated : Ppxlib.Parsetree.expression -> bool
end

module Attr : sig
  val mk : unit -> Migrate_ast.Parsetree.attribute

  val is_generated : Ppxlib.Parsetree.attribute -> bool
end

module Class_exp : sig
  val mk : unit -> Migrate_ast.Parsetree.class_expr

  val is_generated : Ppxlib.Parsetree.class_expr -> bool
end
