open Import
open Types

type t =
  | PublishDiagnostics of PublishDiagnosticsParams.t
  | ShowMessage of ShowMessageParams.t
  | LogMessage of ShowMessageParams.t
  | TelemetryNotification of Json.t
  | CancelRequest of Jsonrpc.Id.t
  | Unknown_notification of Jsonrpc.Message.notification

val to_jsonrpc : t -> Jsonrpc.Message.notification

val of_jsonrpc : Jsonrpc.Message.notification -> (t, string) Result.t
