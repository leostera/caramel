type ('ok, 'err) result = Ok of 'ok | Error of 'err

external raise : exn -> 'a = ""

external raise_notrace : exn -> 'a = ""

exception Exit

external print_int : int -> unit = ""

external print_string : string -> unit = ""

external ( = ) : 'a -> 'a -> bool = ""

external ( <> ) : 'a -> 'a -> bool = ""

external ( < ) : 'a -> 'a -> bool = ""

external ( > ) : 'a -> 'a -> bool = ""

external ( <= ) : 'a -> 'a -> bool = ""

external ( >= ) : 'a -> 'a -> bool = ""

external compare : 'a -> 'a -> int = ""

external ( == ) : 'a -> 'a -> bool = ""

external ( != ) : 'a -> 'a -> bool = ""

external not : bool -> bool = ""

external ( && ) : bool -> bool -> bool = ""

external ( & ) : bool -> bool -> bool = ""

external ( || ) : bool -> bool -> bool = ""

external ( or ) : bool -> bool -> bool = ""

external ( |> ) : 'a -> ('a -> 'b) -> 'b = ""

external ( @@ ) : ('a -> 'b) -> 'a -> 'b = ""

external ( @ ) : 'a list -> 'a list -> 'a list = ""

external ( ~- ) : int -> int = ""

external ( ~+ ) : int -> int = ""

external succ : int -> int = ""

external pred : int -> int = ""

external ( + ) : int -> int -> int = ""

external ( - ) : int -> int -> int = ""

external ( * ) : int -> int -> int = ""

external ( / ) : int -> int -> int = ""

external ( mod ) : int -> int -> int = ""

external ( land ) : int -> int -> int = ""

external ( lor ) : int -> int -> int = ""

external ( lxor ) : int -> int -> int = ""

external ( lsl ) : int -> int -> int = ""

external ( lsr ) : int -> int -> int = ""

external ( asr ) : int -> int -> int = ""

external ( ~-. ) : float -> float = ""

external ( ~+. ) : float -> float = ""

external ( +. ) : float -> float -> float = ""

external ( -. ) : float -> float -> float = ""

external ( *. ) : float -> float -> float = ""

external ( /. ) : float -> float -> float = ""

external ( ** ) : float -> float -> float = ""

external sqrt : float -> float = ""

external exp : float -> float = ""

external log : float -> float = ""

external log10 : float -> float = ""

external expm1 : float -> float = ""

external log1p : float -> float = ""

external cos : float -> float = ""

external sin : float -> float = ""

external tan : float -> float = ""

external acos : float -> float = ""

external asin : float -> float = ""

external atan : float -> float = ""

external atan2 : float -> float -> float = ""

external hypot : float -> float -> float = ""

external cosh : float -> float = ""

external sinh : float -> float = ""

external tanh : float -> float = ""

external ceil : float -> float = ""

external floor : float -> float = ""

external abs_float : float -> float = ""

external copysign : float -> float -> float = ""

external mod_float : float -> float -> float = ""

external frexp : float -> float * int = ""

external modf : float -> float * float = ""

external float : int -> float = ""

external float_of_int : int -> float = ""

external truncate : float -> int = ""

external int_of_float : float -> int = ""

external int_of_char : char -> int = ""

external ignore : 'a -> unit = ""

external int_of_string : string -> int = ""

external float_of_string : string -> float = ""

external fst : 'a * 'b -> 'a = ""

external snd : 'a * 'b -> 'b = ""
