open Import

type init =
  | Uninitialized
  | Initialized of ClientCapabilities.t

type t =
  { store : Document_store.t
  ; merlin : Scheduler.thread
  ; init : init
  ; scheduler : Scheduler.t
  ; detached : Fiber_detached.t
  ; configuration : Configuration.t
  }
