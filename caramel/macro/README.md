# Caramel Macro Support

We'll need to provide a way to build all macros into a single platform-specific
executable that we can run before actual compilation:

```
$ caramel build-macro-driver my_ppx.ml graphql_ppx.ml 
$ ls
ppx_driver.exe my_ppx.ml graphql_ppx.ml 
```

To build this driver without any external dependencies, we will need to pack
with the standard library the `ppxlib` library.
