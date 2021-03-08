(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2019-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

open CamlinternalFormat

let make_printf = make_printf

module Stack = struct
  include Stack

  let top_opt st = try Some (top st) with Stack.Empty -> None
  let pop_opt st = try Some (pop st) with Stack.Empty -> None
end

module Queue = struct
  include Queue

  let take_opt q = try Some (take q) with Queue.Empty -> None
  let peek_opt q = try Some (peek q) with Queue.Empty -> None
end

module Int = struct
  let to_string = string_of_int
end

module Stdlib = struct
  type out_channel = Pervasives.out_channel

  external open_descriptor_out : int -> out_channel
    = "caml_ml_open_descriptor_out"

  let stdout = open_descriptor_out 1
  let stderr = open_descriptor_out 2
end
