-module(types).

-type alias() :: int().

-opaque opaque_alias() :: reference().

-type a_list() :: [alias()].
-type b_list() :: [alias(), ...].

-type union() :: a | b | c.

-type nested_union() :: a | {b | c}.

-type empty_map() :: #{}.

-record(r, {}).
-type record() :: #r{}.
