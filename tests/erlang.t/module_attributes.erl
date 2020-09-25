-module(module_attributes).

-behavior(gen_server).

-behaviour(another_behavior).

-export_type([t/0, opt/2]).

-export([f/0, g/2]).

-on_load([pre/0]).
