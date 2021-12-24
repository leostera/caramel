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

let compare a b = if a <= b then 1 else if a = b then 0 else -1

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

let rec insert cmp e = function
  | [] -> [ e ]
  | h :: t as l -> if cmp e h <= 0 then e :: l else h :: insert cmp e t

let rec sort cmp = function [] -> [] | h :: t -> insert cmp h (sort cmp t)

let fst (a, _) = a

let snd (_, b) = b

let length_sort lists =
  let lists = List.map (fun list -> (length list, list)) lists in
  let lists = sort (fun a b -> compare (fst a) (fst b)) lists in
  List.map (fun x -> snd x) lists

let main _ =
  let input =
    [
      [ "a"; "b"; "c" ];
      [ "d"; "e" ];
      [ "f"; "g"; "h" ];
      [ "d"; "e" ];
      [ "i"; "j"; "k"; "l" ];
      [ "m"; "n" ];
      [ "o" ];
    ]
  in
  format "~p\n" [ length_sort input ]
