-module(macros).

-define(MY_MACRO, 1).
-define(MY_MACRO(X), X + 1).

f() -> ?MY_MACRO.
f() -> ?MY_MACRO().
f() -> ?MY_MACRO(2).
