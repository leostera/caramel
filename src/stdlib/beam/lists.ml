external all : ('a -> bool) -> 'a list -> bool = ""

external any : ('a -> bool) -> 'a list -> bool = ""

external append : 'a list -> 'a list -> 'a list = ""

external append : 'a list list -> 'a list = ""

external concat : string list -> string = ""

external delete : 'a -> 'a list -> 'a list = ""

external droplast : 'a list -> 'a list = ""

external dropwhile : ('a -> bool) -> 'a list -> 'a list = ""

external duplicate : int -> 'a -> 'a list = ""

external filter : ('a -> bool) -> 'a list -> 'a list = ""

external flatmap : ('a -> 'b list) -> 'a list -> 'b list = ""

external flatten : 'a list list -> 'a list = ""

external foldl : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = ""

external foldr : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = ""

external foreach : ('a -> unit) -> 'a list -> unit = ""

external join : 'a -> 'a list -> 'a list = ""

external last : 'a list -> 'a = ""

external map : ('a -> 'b) -> 'a list -> 'b list = ""

external max : 'a list -> 'a = ""

external member : 'a -> 'a list -> bool = ""

external min : 'a list -> 'a = ""

external partition : ('a -> bool) -> 'a list -> 'a list * 'a list = ""

external reverse : 'a list -> 'a list = ""

external seq : int -> int -> int -> int list = ""

external sort : ('a -> 'a -> bool) -> 'a list -> 'a list = ""

external split : int -> 'a list -> 'a list * 'a list = ""

external splitwith : ('a -> bool) -> 'a list -> 'a list * 'a list = ""

external sublist : 'a list -> int -> 'a list = ""

external subtract : 'a list -> 'a list -> 'a list = ""

external sum : int list -> int = ""

external takewhile : ('a -> bool) -> 'a list -> 'a list = ""

external unzip : ('a * 'b) list -> 'a list * 'b list = ""

external zip : 'a list -> 'b list -> ('a * 'b) list = ""

external zipwith : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list = ""
