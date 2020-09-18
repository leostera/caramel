type 'a option =
  | Some of 'a
  | None

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

