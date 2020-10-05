module Ast_transl : sig
  val to_parsetree : Erlang.Ast.t -> Parsetree.structure
end

val compile :
  source_file:string -> output_prefix:string -> opts:Comp_misc.Opts.t -> unit
