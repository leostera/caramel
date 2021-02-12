open Import

type t

val diagnostics_delay : t -> float

val default : t

val update : t -> DidChangeConfigurationParams.t -> t

val request : ConfigurationParams.t

val of_response : Json.t list -> t
