type v = V | V1 of int | V2 of int * int | VR of { field : bool }

let v0 () = V

let v1 () = V1 1

let v2 () = V2 (1, 2)

let vr () = VR { field = false }

type gadt = Hello : string -> gadt

let gadt () = Hello "what"
