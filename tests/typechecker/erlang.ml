type 'msg pid

external spawn : ((unit -> 'message) -> 'a) -> 'message pid = ""

external send : ('message pid * 'message) -> unit = ""
