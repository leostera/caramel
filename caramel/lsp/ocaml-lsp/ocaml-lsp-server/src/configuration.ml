open! Import

type t = { diagnostics_delay : float }

let default = { diagnostics_delay = 0.25 }

let diagnostics_delay t = t.diagnostics_delay

let request = { ConfigurationParams.items = [] }

let of_response (_ : Json.t list) = default

let update t _ = t
