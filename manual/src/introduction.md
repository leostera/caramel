# Introduction

Caramel is a functional language for building type-safe, scalable, and
maintainable applications.

It is built in [OCaml 🐫](https://ocaml.org) and maintained by [Abstract
Machines](https://github.com/AbstractMachinesLab).

Caramel leverages:

* the OCaml compiler, to provide you with a pragmatic type system and
  industrial-strength type safety.

* the Erlang VM, known for running low-latency, distributed, and fault-tolerant
  systems used in a wide range of industries.

Check out the [CHANGELOG](./changelog/v0.1.0.md) to see what's new.

For installation instructions see [Installation](./getting-started/installation.md).

## Feature highlights

* Excellent type-inferece, so you never need to annotate your code
* Zero-cost type-safe interop with most existing Erlang and Elixir code
* Has a reviewed standard library included
* Supports sources in OCaml (and soon Reason syntax too)
* Ships a single executable (`caramel`)
* Has a built-in formatter (`caramel fmt`)

## Philosophy & Goals

Caramel aims to make building type-safe concurrent programs a productive and
fun experience.

Caramel should let anyone with existing OCaml or Reason experience be up and
running without having to relearn the entire languages.

Caramel strives to integrate with the larger ecosystem of BEAM languages, like
Erlang, Elixir, Gleam, Purerl, LFE, and Hamler. 

Caramel should be a batteries included environment where you pay only for what
you use.
