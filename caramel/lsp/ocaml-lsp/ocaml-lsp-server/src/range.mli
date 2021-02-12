open Import

include module type of Lsp.Types.Range with type t = Lsp.Types.Range.t

val compare_size : t -> t -> Ordering.t

val of_loc : Loc.t -> t
