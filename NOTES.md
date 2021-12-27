# Ideas and things to do

- [ ] `external` should allow us to just dump raw code to be included
  - [ ] Raw Erlang code?
- [ ] `external` support for `[@@caramel.module "name"]` attribute 

- [ ] `caramel compile` could also compile the .core to .beam directly?

- [ ] Who should know what `ext_calls` to build? `Ir_0` or `B_builder` ?

- [ ] Handle multiple ignored arguments: right now they get converted to `Param`
      but Core ends up pattern matching them against each other, which ofc isn't
      what we want.

