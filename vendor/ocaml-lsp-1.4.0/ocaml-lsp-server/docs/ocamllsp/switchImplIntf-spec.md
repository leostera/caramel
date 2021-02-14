#### Switch Implementation/Interface Request

Switch Implementation/Interface Request is sent from client to server  to get
URI(s) of the file(s) that the current file can switch to, e.g.,  if the user
has "foo.ml" and "foo.mli" files, the client, who want to  switch from one to
the other, sends this request.

If there are one or more files, to which the currently open file can  switch to,
exist in the same folder, then URIs of all those existing  files are returned.
In case there is no file to switch to in that folder,  the most likely candidate
for creation is returned, e.g., if a user wants  to switch from "foo.ml", but no
files already exist in the project that  could be returned, a URI for "foo.mli"
is returned.

##### Client capability

nothing that should be noted

##### Server capability

property name: `handleSwitchImplIntf`

property type: `boolean`

##### Request

- method: `ocamllsp/switchImplIntf`
- params: `DocumentUri` (see [`DocumentUri`](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#uri) in LSP specification)

##### Response

- result: DocumentUri[] (non-empty)
- error: code and message set in case an exception happens during the `ocamllsp/switchImplIntf` request.


