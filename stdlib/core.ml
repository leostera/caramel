type ('ok, 'err) result = Ok of 'ok | Error of 'err

exception Exit

external ( = ) : 'a -> 'a -> bool = ""

external ( <> ) : 'a -> 'a -> bool = ""

external ( < ) : 'a -> 'a -> bool = ""

external ( > ) : 'a -> 'a -> bool = ""

external ( <= ) : 'a -> 'a -> bool = ""

external ( >= ) : 'a -> 'a -> bool = ""

external ( == ) : 'a -> 'a -> bool = ""

external ( != ) : 'a -> 'a -> bool = ""

external not : bool -> bool = ""

external ( && ) : bool -> bool -> bool = ""

external ( || ) : bool -> bool -> bool = ""

external ( @ ) : 'a list -> 'a list -> 'a list = ""

external ( ~- ) : int -> int = ""

external ( ~+ ) : int -> int = ""

external ( + ) : int -> int -> int = ""

external ( - ) : int -> int -> int = ""

external ( * ) : int -> int -> int = ""

external ( / ) : int -> int -> int = ""

external ( mod ) : int -> int -> int = ""

external ( land ) : int -> int -> int = ""

external ( lor ) : int -> int -> int = ""

external ( lxor ) : int -> int -> int = ""

external lnot : int -> int = ""

external ( lsl ) : int -> int -> int = ""

external ( asr ) : int -> int -> int = ""

external ( ~-. ) : float -> float = ""

external ( ~+. ) : float -> float = ""

external ( +. ) : float -> float -> float = ""

external ( -. ) : float -> float -> float = ""

external ( *. ) : float -> float -> float = ""

external ( /. ) : float -> float -> float = ""
