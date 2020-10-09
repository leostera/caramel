# Erlang Language Libraries 

Libraries to work with Erlang sources in Standard Erlang and Core Erlang
syntax.

## Standard Erlang

This library contains:

* a definition of an AST for Erlang,
* a parser that tries to follow the [Standard Erlang
  grammar](https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_parse.yrl),
  and
* 2 printers: a debugging S-expr printer, and a Standard Erlang printer

## Core Erlang

This libraries contain:

* a definition of an AST for Core Erlang,
* a parser that follows the [Core Erlang
  Spec](https://www.it.uu.se/research/group/hipe/cerl/doc/core_erlang-1.0.3.pdf)
  and,
* 2 printers: a debugging S-expr printer, and a Core Erlang printer
