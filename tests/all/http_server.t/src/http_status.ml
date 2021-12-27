open Erlang

(*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

type informational = [ `Continue | `Switching_protocols ]

type successful =
  [ `OK
  | `Created
  | `Accepted
  | `Non_authoritative_information
  | `No_content
  | `Reset_content
  | `Partial_content ]

type redirection =
  [ `Multiple_choices
  | `Moved_permanently
  | `Found
  | `See_other
  | `Not_modified
  | `Use_proxy
  | `Temporary_redirect ]

type client_error =
  [ `Bad_request
  | `Unauthorized
  | `Payment_required
  | `Forbidden
  | `Not_found
  | `Method_not_allowed
  | `Not_acceptable
  | `Proxy_authentication_required
  | `Request_timeout
  | `Conflict
  | `Gone
  | `Length_required
  | `Precondition_failed
  | `Payload_too_large
  | `Uri_too_long
  | `Unsupported_media_type
  | `Range_not_satisfiable
  | `Expectation_failed
  | `I_m_a_teapot
  | `Enhance_your_calm
  | `Upgrade_required ]

type server_error =
  [ `Internal_server_error
  | `Not_implemented
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_timeout
  | `Http_version_not_supported ]

type standard =
  [ informational | successful | redirection | client_error | server_error ]

type t = [ standard | `Code of int ]

let default_reason_phrase = function
  (* Informational *)
  | `Continue -> "Continue"
  | `Switching_protocols -> "Switching Protocols"
  (* Successful *)
  | `OK -> "OK"
  | `Created -> "Created"
  | `Accepted -> "Accepted"
  | `Non_authoritative_information -> "Non-Authoritative Information"
  | `No_content -> "No Content"
  | `Reset_content -> "Reset Content"
  | `Partial_content -> "Partial Content"
  (* Redirection *)
  | `Multiple_choices -> "Multiple Choices"
  | `Moved_permanently -> "Moved Permanently"
  | `Found -> "Found"
  | `See_other -> "See Other"
  | `Not_modified -> "Not Modified"
  | `Use_proxy -> "Use Proxy"
  | `Temporary_redirect -> "Temporary Redirect"
  (* Client error *)
  | `Bad_request -> "Bad Request"
  | `Unauthorized -> "Unauthorized"
  | `Payment_required -> "Payment Required"
  | `Forbidden -> "Forbidden"
  | `Not_found -> "Not Found"
  | `Method_not_allowed -> "Method Not Allowed"
  | `Not_acceptable -> "Not Acceptable"
  | `Proxy_authentication_required -> "Proxy Authentication Required"
  | `Request_timeout -> "Request Timeout"
  | `Conflict -> "Conflict"
  | `Gone -> "Gone"
  | `Length_required -> "Length Required"
  | `Precondition_failed -> "Precondition Failed"
  | `Payload_too_large -> "Payload Too Large"
  | `Uri_too_long -> "URI Too Long"
  | `Unsupported_media_type -> "Unsupported Media Type"
  | `Range_not_satisfiable -> "Range Not Satisfiable"
  | `Expectation_failed -> "Expectation Failed"
  | `I_m_a_teapot -> "I'm a teapot" (* RFC 2342 *)
  | `Enhance_your_calm -> "Enhance Your Calm"
  | `Upgrade_required -> "Upgrade Required"
  (* Server error *)
  | `Internal_server_error -> "Internal Server Error"
  | `Not_implemented -> "Not Implemented"
  | `Bad_gateway -> "Bad Gateway"
  | `Service_unavailable -> "Service Unavailable"
  | `Gateway_timeout -> "Gateway Timeout"
  | `Http_version_not_supported -> "HTTP Version Not Supported"

let to_code = function
  (* Informational *)
  | `Continue -> 100
  | `Switching_protocols -> 101
  (* Successful *)
  | `OK -> 200
  | `Created -> 201
  | `Accepted -> 202
  | `Non_authoritative_information -> 203
  | `No_content -> 204
  | `Reset_content -> 205
  | `Partial_content -> 206
  (* Redirection *)
  | `Multiple_choices -> 300
  | `Moved_permanently -> 301
  | `Found -> 302
  | `See_other -> 303
  | `Not_modified -> 304
  | `Use_proxy -> 305
  | `Temporary_redirect -> 307
  (* Client error *)
  | `Bad_request -> 400
  | `Unauthorized -> 401
  | `Payment_required -> 402
  | `Forbidden -> 403
  | `Not_found -> 404
  | `Method_not_allowed -> 405
  | `Not_acceptable -> 406
  | `Proxy_authentication_required -> 407
  | `Request_timeout -> 408
  | `Conflict -> 409
  | `Gone -> 410
  | `Length_required -> 411
  | `Precondition_failed -> 412
  | `Payload_too_large -> 413
  | `Uri_too_long -> 414
  | `Unsupported_media_type -> 415
  | `Range_not_satisfiable -> 416
  | `Expectation_failed -> 417
  | `I_m_a_teapot -> 418
  | `Enhance_your_calm -> 420
  | `Upgrade_required -> 426
  (* Server error *)
  | `Internal_server_error -> 500
  | `Not_implemented -> 501
  | `Bad_gateway -> 502
  | `Service_unavailable -> 503
  | `Gateway_timeout -> 504
  | `Http_version_not_supported -> 505
  | `Code c -> c

let really_unsafe_of_code = function
  (* Informational *)
  | 100 -> `Continue
  | 101 -> `Switching_protocols
  (* Successful *)
  | 200 -> `OK
  | 201 -> `Created
  | 202 -> `Accepted
  | 203 -> `Non_authoritative_information
  | 204 -> `No_content
  | 205 -> `Reset_content
  | 206 -> `Partial_content
  (* Redirection *)
  | 300 -> `Multiple_choices
  | 301 -> `Moved_permanently
  | 302 -> `Found
  | 303 -> `See_other
  | 304 -> `Not_modified
  | 305 -> `Use_proxy
  | 307 -> `Temporary_redirect
  (* Client error *)
  | 400 -> `Bad_request
  | 401 -> `Unauthorized
  | 402 -> `Payment_required
  | 403 -> `Forbidden
  | 404 -> `Not_found
  | 405 -> `Method_not_allowed
  | 406 -> `Not_acceptable
  | 407 -> `Proxy_authentication_required
  | 408 -> `Request_timeout
  | 409 -> `Conflict
  | 410 -> `Gone
  | 411 -> `Length_required
  | 412 -> `Precondition_failed
  | 413 -> `Payload_too_large
  | 414 -> `Uri_too_long
  | 415 -> `Unsupported_media_type
  | 416 -> `Range_not_satisfiable
  | 417 -> `Expectation_failed
  | 418 -> `I_m_a_teapot
  | 420 -> `Enhance_your_calm
  | 426 -> `Upgrade_required
  (* Server error *)
  | 500 -> `Internal_server_error
  | 501 -> `Not_implemented
  | 502 -> `Bad_gateway
  | 503 -> `Service_unavailable
  | 504 -> `Gateway_timeout
  | 505 -> `Http_version_not_supported
  | c -> `Code c

let unsafe_of_code c =
  match really_unsafe_of_code c with
  | `Code c ->
      if c < 0 then
        throw (Io_lib.format "Status.unsafe_of_code: ~d is negative" [ c ])
      else `Code c
  | s -> s

let of_code c =
  match really_unsafe_of_code c with
  | `Code c ->
      if c < 100 || c > 999 then
        throw
          (Io_lib.format "Status.of_code: ~d is not a three-digit number" [ c ])
      else `Code c
  | s -> s

let is_informational t =
  match t with
  | #informational -> true
  | `Code n -> n >= 100 && n <= 199
  | _ -> false

let is_successful t =
  match t with
  | #successful -> true
  | `Code n -> n >= 200 && n <= 299
  | _ -> false

let is_redirection t =
  match t with
  | #redirection -> true
  | `Code n -> n >= 300 && n <= 399
  | _ -> false

let is_client_error t =
  match t with
  | #client_error -> true
  | `Code n -> n >= 400 && n <= 499
  | _ -> false

let is_server_error t =
  match t with
  | #server_error -> true
  | `Code n -> n >= 500 && n <= 599
  | _ -> false

let is_error t = is_client_error t || is_server_error t

let to_string = function
  (* don't allocate *)
  (* Informational *)
  | `Continue -> "100"
  | `Switching_protocols -> "101"
  (* Successful *)
  | `OK -> "200"
  | `Created -> "201"
  | `Accepted -> "202"
  | `Non_authoritative_information -> "203"
  | `No_content -> "204"
  | `Reset_content -> "205"
  | `Partial_content -> "206"
  (* Redirection *)
  | `Multiple_choices -> "300"
  | `Moved_permanently -> "301"
  | `Found -> "302"
  | `See_other -> "303"
  | `Not_modified -> "304"
  | `Use_proxy -> "305"
  | `Temporary_redirect -> "307"
  (* Client error *)
  | `Bad_request -> "400"
  | `Unauthorized -> "401"
  | `Payment_required -> "402"
  | `Forbidden -> "403"
  | `Not_found -> "404"
  | `Method_not_allowed -> "405"
  | `Not_acceptable -> "406"
  | `Proxy_authentication_required -> "407"
  | `Request_timeout -> "408"
  | `Conflict -> "409"
  | `Gone -> "410"
  | `Length_required -> "411"
  | `Precondition_failed -> "412"
  | `Payload_too_large -> "413"
  | `Uri_too_long -> "414"
  | `Unsupported_media_type -> "415"
  | `Range_not_satisfiable -> "416"
  | `Expectation_failed -> "417"
  | `I_m_a_teapot -> "418"
  | `Enhance_your_calm -> "420"
  | `Upgrade_required -> "426"
  (* Server error *)
  | `Internal_server_error -> "500"
  | `Not_implemented -> "501"
  | `Bad_gateway -> "502"
  | `Service_unavailable -> "503"
  | `Gateway_timeout -> "504"
  | `Http_version_not_supported -> "505"
  | `Code c -> integer_to_binary c
(* except for this *)

let of_string x = of_code (binary_to_integer x)
