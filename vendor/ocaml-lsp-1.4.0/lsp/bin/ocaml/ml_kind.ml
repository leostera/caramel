open! Import

type 'a t =
  { intf : 'a
  ; impl : 'a
  }

type kind =
  | Impl
  | Intf

let get { intf; impl } = function
  | Impl -> impl
  | Intf -> intf

let make_both a = { intf = a; impl = a }

let iter { intf; impl } ~f =
  f intf;
  f impl

let map { intf; impl } ~f = { intf = f intf; impl = f impl }

let both (type a b) (x : a t) (y : b t) : (a * b) t =
  { intf = (x.intf, y.intf); impl = (x.impl, y.impl) }
