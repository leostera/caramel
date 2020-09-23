# Roadmap

## Up Next

- [ ] Add compiler flag `to_core` to pick this backend
- [ ] Compile and compare against the `./erltest` suite 
- [ ] Scout out what parts of Lambda are not compileable to Core (e.g, `Passign`)
- [ ] Finish mapping from Lambda
- [ ] Finish pretty printer of Core AST
- [ ] Brainstorm ways to verify the semantics have not changed from
      `erlc +to_core` to `caramelc -to_core` 
- [ ] Figure out what type information is available at Lambda stage and if it
      makes sense to use that, or pull in the Signature to decore the Core AST
