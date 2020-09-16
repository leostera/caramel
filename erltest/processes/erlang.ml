external spawn : (unit -> 'a) -> 'message Process.t = ""
external send : 'message Process.t -> 'message -> unit = ""
