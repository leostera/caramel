external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external ( - ) : 'a -> 'a -> 'a = "erlang:-"

external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( > ) : int -> int -> bool = "erlang:>"

external ( <= ) : int -> int -> bool = "erlang:=<"

external ( mod ) : int -> int -> int = "erlang:rem"

external append : 'a list list -> 'a list = "lists:append"

external format : string -> 'a list -> unit = "io:format"

external length : 'a list -> int = "erlang:length"

external min : int -> int -> int = "erlang:min"

external parse_int : string -> int = "erlang:list_to_integer"

external throw : 'a -> 'b = "erlang:throw"

external rev : 'a list -> 'a list = "lists:reverse"

let ( @ ) a b = append [ a; b ]

module List = struct
  external map : ('a -> 'b) -> 'a list -> 'b list = "lists:map"

  external filter : ('a -> bool) -> 'a list -> 'a list = "lists:filter"

  external all : ('a -> bool) -> 'a list -> bool = "lists:all"

  external concat : 'a list list -> 'a list = "lists:concat"
end

module Random = struct
  external seed : [ `default ] -> int -> unit = "rand:seed"

  external uniform : int -> int = "rand:uniform"
end

let emit l acc = l :: acc

let rec prepend_aux p emit acc = function
  | [] -> emit [] acc
  | ((n, l) as h) :: t ->
      let acc = if n > 0 then emit ((n - 1, p :: l) :: t) acc else acc in
      prepend_aux p (fun l acc -> emit (h :: l) acc) acc t

let prepend p list = prepend_aux p (fun a b -> emit a b) [] list

let rec aux initial = function
  | [] -> [ initial ]
  | h :: t -> append (List.map (fun l -> prepend h l) (aux initial t))

let group list sizes =
  let initial = List.map (fun size -> (size, [])) sizes in
  let all = aux initial list in
  let complete = List.filter (fun ls -> List.all (fun (x, _) -> x = 0) ls) all in
  List.map (fun l -> List.map (fun (_, y) -> y) l) complete

let main (n :: m :: xs) =
  format "~p\n" [ group xs [ parse_int n; parse_int m ] ]
