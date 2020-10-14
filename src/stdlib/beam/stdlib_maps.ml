type ('key, 'value) map

external filter : ('k * 'v -> bool) -> ('k, 'v) map -> ('k, 'v) map = ""

external find : 'k -> ('k, 'v) map -> [ `ok of 'v | `error ] = ""

external fold : ('k -> 'v -> 'acc -> 'acc) -> 'acc -> ('k, 'v) map -> 'acc = ""

external from_list : ('k * 'v) list -> ('k, 'v) map = ""

external get : 'k -> ('k, 'v) map -> 'v -> 'v = ""

external is_key : 'k -> ('k, 'v) map -> bool = ""

external keys : ('k, 'v) map -> 'k list = ""

external map : ('k * 'v1 -> 'v2) -> ('k, 'v1) map -> ('k, 'v2) map = ""

external merge : ('k, 'v) map -> ('k, 'v) map -> ('k, 'v) map = ""

external empty : unit -> ('key, 'value) map = "new"

external put : 'k -> 'v -> ('k, 'v) map -> ('k, 'v) map = ""

external size : ('k, 'v) map -> int = ""

external to_list : ('k, 'v) map -> ('k * 'v) list = ""

external update : 'k -> 'v -> ('k, 'v) map -> ('k, 'v) map = ""

external values : ('k, 'v) map -> 'v list = ""

external with_keys : 'a list -> ('a, 'b) map -> ('a, 'b) map = "with"

external without : 'a list -> ('a, 'b) map -> ('a, 'b) map = ""
