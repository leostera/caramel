type endianness = Big | Little

external at : string -> int -> char list = ""

external bin_to_list : string -> char list = ""

external copy : string -> int -> char list = ""

external decode_unsigned : string -> endianness -> int = ""

external encode_unsigned : int -> endianness -> string = ""

external first : string -> char = ""

external last : string -> char = ""

external list_to_bin : char list -> string = ""

type replace_opt = Global | Scope of (int * int) | Insert_replaced of int list

external replace : string -> string -> string -> replace_opt list -> string = ""

external split : string -> string list -> string list = ""
