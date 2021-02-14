#### Infer Interface Request

Infer Interface Request is sent from the client to the server to get the infered
interface for a given module implementation.

The document URI in the request has to be open before sending a the request.

If the file cannot be found in the document store, an error will be returned.

Warning: this custom request is meant to be consumed by `ocaml-vscode-platform` exclusively,
it can be removed any time and should not be relied on.

##### Client capability

nothing that should be noted

##### Server capability

property name: `handleInferIntf`
property type: `boolean`

##### Request

- method: `ocamllsp/inferIntf`
- params: `DocumentUri` (see [`DocumentUri`](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#uri) in LSP specification)

##### Response

- result: String
- error: code and message set in case an exception happens during the processing of the request.
