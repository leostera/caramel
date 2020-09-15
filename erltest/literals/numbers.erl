% Source code generated with Caramel.
-module(numbers).


-export([character/1]).
-export([float/1]).
-export([id/1]).
-export([integer/1]).
-export([string/1]).

id(X) -> X.

integer({}) -> 1.

float({}) -> 1.0.

character({}) -> 'c'.

string({}) -> <<"hello">>.


