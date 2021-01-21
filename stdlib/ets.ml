type ('k, 'v) t

type table_type = [ `Set | `Ordered_set | `Bag | `Duplicate_bag ]

type access = [ `Public | `Protected | `Private ]

type concurrency = [ `Write_concurrency of bool | `Read_concurrency of bool ]

type make_opt =
  [ access
  | concurrency
  | table_type
  | `Named_table
  | `Decentralized_counters of bool
  | `Compressed ]

external make : 'a -> make_opt list -> ('k, 'v) t = "new"

external lookup : ('k, 'v) t -> 'k -> 'v list = ""

external insert_one : ('k, 'v) t -> 'k * 'v -> unit = "insert"

external insert_many : ('k, 'v) t -> ('k * 'v) list -> unit = "insert"

external foldl : ('k * 'v -> 'acc -> 'acc) -> 'acc -> ('k, 'v) t -> 'acc = ""

external foldr : ('k * 'v -> 'acc -> 'acc) -> 'acc -> ('k, 'v) t -> 'acc = ""
