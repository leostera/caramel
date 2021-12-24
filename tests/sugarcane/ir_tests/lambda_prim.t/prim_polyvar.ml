let polyvar0 () = `poly

let polyvar1 () = `what1 (polyvar0 ())

let polyvar2 () = `what2 (`poly, polyvar1 ())
