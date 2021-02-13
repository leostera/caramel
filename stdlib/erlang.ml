external length : 'a list -> int = ""

external throw : 'a -> 'a = ""

(************** processes *****************)

type 'm pid

external register : 'a -> 'm pid -> bool = ""

external self : unit -> 'm pid = ""

external send : 'm pid -> 'm -> unit = ""

external spawn : (unit -> 'a) -> 'm pid = ""

external where_is : 'a -> 'm pid = "whereis"

(************** is_* guards *****************)

external is_atom : 'a -> bool = ""

external is_binary : 'a -> bool = ""

external is_bitstring : 'a -> bool = ""

external is_boolean : 'a -> bool = ""

external is_float : 'a -> bool = ""

external is_integer : 'a -> bool = ""

external is_list : 'a -> bool = ""

external is_map : 'a -> bool = ""

external is_number : 'a -> bool = ""

external is_pid : 'a -> bool = ""

external is_port : 'a -> bool = ""

external is_process_alive : 'a -> bool = ""

external is_reference : 'a -> bool = ""

external is_tuple : 'a -> bool = ""

(************** *_to_* parsers *****************)

external list_to_integer : string -> int = ""

external list_to_float : string -> float = ""

external integer_to_list : int -> string = ""

external float_to_list : string -> float = ""

(************** math functions *****************)

external floor : float -> int = ""

external ceil : float -> int = ""

external abs : int -> int = ""

external abs_float : float -> float = "abs"
