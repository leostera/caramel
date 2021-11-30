let ppf = Format.err_formatter

let todo () =
  Format.fprintf ppf "Oops! This function has not been implemented yet.\n";
  exit 1

let unsupported_top_level_module_value () =
  Format.fprintf ppf
    {|While OCaml allows you to define module-level variables,
these are not valid declarations at the module-leve in Erlang.

Caramel could have translated them into a function, so that `let x = 1` became
`x() -> 1` in Erlang, an every instance of `x` would become `x()`.

Unfortunately this will break the semantics during translation :(

For example, if `let x = print_string "hello"; 1` was translated to Erlang, it
would be translated to `x() -> io:format("~p", ["hello"]), 1.`

Now the side-effect of printing would occur every time you use the variable `x`.

You can, however, turn your constants into a function of a unit, so that
`let x = 1` becomes `let x () = 1`.

Caramel will happily and safely compile that.
\n
|};
  exit 1
