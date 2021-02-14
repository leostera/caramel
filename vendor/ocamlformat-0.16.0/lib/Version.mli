(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

val version : string
(** A version number, or "unknown". This is provided by [dune-build-info],
    which means that it will be resolved in the following way:

    - if (version) is set in (dune-project), it is used. This is what happens
      when using opam pins (through dune subst), or for released versions
      (through dune-release).
    - otherwise, a description from [git describe] will be used. Caveat for
      this case: binaries under [_build/] will not have this information, but
      [dune install --prefix _install] will copy a valid binary under
      [_install/bin]. *)
