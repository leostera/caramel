% Source code generated with Caramel.
-module(type_defs).

-export_type([compound/0]).
-export_type([hidden/0]).
-export_type([inlined_record/0]).
-export_type([list/1]).
-export_type([opaque/0]).
-export_type([option/1]).
-export_type([phantom/1]).
-export_type([poly/0]).
-export_type([record/0]).
-export_type([result/2]).
-export_type([triplet/2]).

-type opaque() :: string().

-type hidden() :: ref().

-type list(A) :: {cons}
               | {nil}
               .

-type option(A) :: {some}
                 | {none}
                 .

-type result(A, B) :: {ok}
                    | {error}
                    .

-type triplet(A, B) :: {A, A, B}.

-type phantom(A) :: {phantom}
                  .

-type record() :: #{ a :: any()
                   , b :: any()
                   }.

-type inlined_record() :: {compound}
                        .

-type compound() :: #{ c_a :: any()
                     , c_b :: any()
                     , c_c :: any()
                     , c_d :: any()
                     }.


