type mut = { mutable contents : int }

let makemut () = { contents = 1 }

let setfield () = { contents = 1 }.contents <- 2
