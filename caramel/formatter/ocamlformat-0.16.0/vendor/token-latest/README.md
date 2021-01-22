This library vendors the latest token type, so that when source code is
reparsed, the token type from compiler-libs (which can change) is not directly
manipulated.

If the token type from the compiler-libs version is not the latest one, an
upwards migration is performed. Here is a list of changes:

### 4.10 -> 4.11.alpha3

The `STRING` token (for string literals) is changed from ` `(string * string
option)` (contents, delimiter) to `(string * Location.t * string option)`.
The location corresponds to the contents (see ocaml/ocaml#8820).
When migrating, it is set to `Location.none`.

## 4.07 -> 4.08

Added `LETOP` and ̀`ANDOP` tokens.
