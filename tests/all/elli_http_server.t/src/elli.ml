open Erlang

type meth = GET

type opts = Callback : atom -> opts | Port : int -> opts

let child_spec ~name ~opts =
  Supervisor.
    {
      id = atom name;
      start = (atom "elli", atom "start_link", Erlang.as_term [ opts ]);
    }

module Request = struct
  type t

  external method_ : t -> meth = "elli_request:method"

  external path : t -> string list = "elli_request:path"
end

let reply status opts body = (status, opts, body)
