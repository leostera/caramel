# Introduction

Caramel is a programming language for building type-safe, concurrent software.

It is built in [OCaml 🐫](https://ocaml.org) and maintained by [Abstract
Machines](https://github.com/AbstractMachinesLab).

Check out the [CHANGELOG](./changelog/v0.0.15.md) to see what's new.

For installation instructions see [Installation](./getting-started/installation.md).

## Feature highlights

* Excellent type-inferece, so you never need to annotate your code
* Zero-cost type-safe interop with most existing Erlang and Elixir code
* Has a reviewed standard library included
* Supports sources in OCaml and Reason syntax
* Ships a single executable (`caramel`)
* Has a built-in formatter (`caramel fmt`)

## Philosophy & Goals

Caramel aims to make building type-safe concurrent programs a productive and
fun experience.

Caramel should let anyone with existing OCaml or Reason experience 

Caramel strives to integrate with the larger ecosystem of BEAM languages, like
Erlang, Elixir, Gleam, Purerl, LFE, and Hamler. 

Caramel should be a batteries included environment where you pay only for what
you use.
