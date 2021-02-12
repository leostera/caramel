(** Protocol extensions that aren't part of the spec *)

open Import

module DebugEcho : sig
  module Params : sig
    type t = { message : string }

    include Json.Jsonable.S with type t := t
  end

  module Result : sig
    type t = Params.t = { message : string }

    include Json.Jsonable.S with type t := t
  end
end

module DebugTextDocumentGet : sig
  module Params : sig
    include module type of Types.TextDocumentPositionParams
  end

  module Result : sig
    type t = string option

    include Json.Jsonable.S with type t := t
  end
end
