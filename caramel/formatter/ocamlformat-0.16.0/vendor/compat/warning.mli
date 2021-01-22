val with_warning_filter :
  filter:(Location.t -> Warnings.t -> bool) -> f:(unit -> 'a) -> 'a

val print_warning : Location.t -> Warnings.t -> unit

val is_unexpected_docstring : Warnings.t -> bool
