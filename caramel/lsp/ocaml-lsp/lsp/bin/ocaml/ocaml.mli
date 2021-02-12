module Module : sig
  type t
end

val of_typescript : Ts_types.Resolved.t list -> Module.t list

val output : Module.t list -> kind:Ml.Kind.t -> out_channel -> unit
