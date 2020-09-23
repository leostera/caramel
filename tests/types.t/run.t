  $ ls *.ml *.mli
  abstract.ml
  extensible_variant.ml
  fn.ml
  inductive.ml
  opaque_and_hidden.ml
  opaque_and_hidden.mli
  polymorphic_variants.ml
  qualified_names.ml
  record.ml
  type_alias.ml
  type_args.ml
  variants.ml
  $ caramelc compile *.ml *.mli
  Compiling variants.erl	OK
  Compiling type_args.erl	OK
  Compiling type_alias.erl	OK
  Compiling record.erl	OK
  Compiling polymorphic_variants.erl	OK
  Compiling opaque_and_hidden.erl	OK
  Compiling inductive.erl	OK
  Compiling fn.erl	OK
  Compiling extensible_variant.erl	OK
  Compiling abstract.erl	OK
  Compiling qualified_names.erl	OK
  $ cat *.erl
  % Source code generated with Caramel.
  -module(abstract).
  -export_type([t/0]).
  
  
  -opaque t() :: reference().
  
  
  % Source code generated with Caramel.
  -module(extensible_variant).
  -export_type([t/0]).
  
  
  -type t() :: any().
  
  
  % Source code generated with Caramel.
  -module(fn).
  -export_type([add/0]).
  -export_type([defer/1]).
  -export_type([f/2]).
  -export_type([f_with_tuples/2]).
  -export_type([nested/0]).
  -export_type([predicate/1]).
  -export_type([r/1]).
  
  
  -type defer(A) :: fun((ok) -> A).
  
  -type predicate(A) :: fun((A) -> boolean()).
  
  -type add() :: fun((integer(), integer()) -> integer()).
  
  -type f(A, B) :: fun((A, B) -> boolean()).
  
  -type nested() :: fun((ok, fun((integer()) -> boolean())) -> string()).
  
  -type f_with_tuples(A, B) :: fun(({A, B}, ok) -> boolean()).
  
  -type r(A) :: #{ f => fun((ok) -> {A, integer()}) }.
  
  
  % Source code generated with Caramel.
  -module(inductive).
  -export_type([a_list/1]).
  -export_type([tree/1]).
  
  
  -type a_list(A) :: {cons, {A, a_list(A)}}
                   | nil
                   .
  
  -type tree(Value) :: {leaf, Value, Value}
                     | {node, Value, tree(Value)}
                     .
  
  
  % Source code generated with Caramel.
  -module(opaque_and_hidden).
  -export_type([opaque/0]).
  -export_type([visible/0]).
  
  
  -type visible() :: integer().
  
  -opaque opaque() :: string().
  
  -opaque hidden() :: reference().
  
  
  % Source code generated with Caramel.
  -module(polymorphic_variants).
  -export_type([plain/0]).
  -export_type([poly/1]).
  -export_type([poly_ext/1]).
  -export_type([poly_ext_fixed/0]).
  -export_type([poly_ext_phantom/1]).
  
  
  -type plain() :: poly_zero
                 .
  
  -type poly(A) :: plain()
                 | {poly_a, A}
                 | {poly_b, integer()}
                 .
  
  -type poly_ext(A) :: poly(A)
                     | {poly_c, string()}
                     .
  
  -type poly_ext_fixed() :: poly(integer())
                          | {poly_c, string()}
                          .
  
  -type poly_ext_phantom(_A) :: poly(integer())
                              | {poly_c, string()}
                              .
  
  
  % Source code generated with Caramel.
  -module(qualified_names).
  -export_type([compound/0]).
  
  
  -type compound() :: #{ c_a => record:inlined_record()
                       , c_b => type_args:phantom(boolean())
                       , c_c => type_args:triplet(integer(), boolean())
                       , c_d => record:large_record()
                       , c_fn => {record:small_record(), fn:defer(integer())}
                       }.
  
  
  % Source code generated with Caramel.
  -module(record).
  -export_type([inlined_record/0]).
  -export_type([large_record/0]).
  -export_type([record/1]).
  -export_type([small_record/0]).
  
  
  -type record(A) :: #{ author => list(A)
                      , year => integer()
                      , related => option:t(record(A))
                      }.
  
  -type inlined_record() :: {simpler, boolean()}
                          | {many, boolean(), boolean()}
                          | {compound, #{ ir_a => float()
                                        , ir_b => boolean()
                                        }}
                          .
  
  -type small_record() :: #{ a => string() }.
  
  -type large_record() :: #{ lr_a => string()
                           , lr_b => string()
                           , lr_c => string()
                           , lr_d => string()
                           }.
  
  
  % Source code generated with Caramel.
  -module(type_alias).
  -export_type([alias/0]).
  
  
  -type alias() :: integer().
  
  
  % Source code generated with Caramel.
  -module(type_args).
  -export_type([phantom/1]).
  -export_type([triplet/2]).
  
  
  -type triplet(A, B) :: {A, A, B}.
  
  -type phantom(_A) :: phantom
                     .
  
  
  % Source code generated with Caramel.
  -module(variants).
  -export_type([option/1]).
  -export_type([result/2]).
  
  
  -type option(A) :: {some, A}
                   | none
                   .
  
  -type result(A, B) :: {ok, A}
                      | {error, B}
                      .
  
  
