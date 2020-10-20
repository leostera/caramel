type ('key, 'value) t

external filter : ('k * 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t = ""

external find : 'k -> ('k, 'v) t -> [ `ok of 'v | `error ] = ""

external fold : ('k -> 'v -> 'acc -> 'acc) -> 'acc -> ('k, 'v) t -> 'acc = ""

external from_list : ('k * 'v) list -> ('k, 'v) t = ""

external get : 'k -> ('k, 'v) t -> 'v -> 'v = ""

external is_key : 'k -> ('k, 'v) t -> bool = ""

external keys : ('k, 'v) t -> 'k list = ""

external map : ('k * 'v1 -> 'v2) -> ('k, 'v1) t -> ('k, 'v2) t = ""

external merge : ('k, 'v) t -> ('k, 'v) t -> ('k, 'v) t = ""

external empty : unit -> ('key, 'value) t = "new"

external put : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t = ""

external size : ('k, 'v) t -> int = ""

external to_list : ('k, 'v) t -> ('k * 'v) list = ""

external update : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t = ""

external values : ('k, 'v) t -> 'v list = ""

external with_keys : 'a list -> ('a, 'b) t -> ('a, 'b) t = "with"

external without : 'a list -> ('a, 'b) t -> ('a, 'b) t = ""
