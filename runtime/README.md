# Caramel Runtime

This runtime helps compilation of OCaml to Erlang, and typechecking of Erlang
itself.

When compiling OCaml source code to Erlang, certain libraries are necessary to
ensure the interop between them, both at the type-level, but at the value level
as well.

At the type level, the runtime provides you with a typed process identifier,
`'m pid`, that you can use to ensure that you're only sending messages that the
recipient knows how to handle.

However, to guarantee that we are not fabricating poorly typed pids, we have
some type-level machinery in the `Process` module that ensures that the pid
of a spawned process reflects the exact type of messages it will handle.

At the value level, when your program does execute, this runtime tries to be as
low-overhead as possible, but it will incurr in a small penalty for message
passing at the moment, where every message will be tagged with `some` if there
was one.

When typechecking Erlang, certain idioms will be mapped into their respective
Caramel runtime counterparts, to ensure that the same type-safety that you would
get going frm OCaml to Erlang, is present within a subset of Erlang itself.
