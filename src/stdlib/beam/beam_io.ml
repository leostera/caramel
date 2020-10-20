external __unsafe_fmt : string -> 'a list -> unit = "format"

let format s x = __unsafe_fmt s x
