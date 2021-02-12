import outdent from "outdent";
import * as rpc from "vscode-jsonrpc/node";
import * as LanguageServer from "../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

describe("textDocument/diagnostics", () => {
  let languageServer: rpc.MessageConnection = null;

  async function openDocument(source: string) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        source,
      ),
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("unused values have diagnostic tags", async () => {
    let receivedDiganostics = new Promise((resolve, _reject) =>
      languageServer.onNotification((method, params) => {
        expect(method).toMatchInlineSnapshot(
          `"textDocument/publishDiagnostics"`,
        );
        expect(params).toMatchInlineSnapshot(`
          Object {
            "diagnostics": Array [
              Object {
                "message": "Warning 26: unused variable x.",
                "range": Object {
                  "end": Object {
                    "character": 7,
                    "line": 1,
                  },
                  "start": Object {
                    "character": 6,
                    "line": 1,
                  },
                },
                "severity": 2,
                "source": "ocamllsp",
              },
            ],
            "uri": "file:///test.ml",
          }
        `);
        resolve(null);
      }),
    );
    await openDocument(outdent`
      let () =
        let x = 123 in
        ()
    `);
    await receivedDiganostics;
  });

  it("deprecated values have diganostic tags", async () => {
    let receivedDiganostics = new Promise((resolve, _reject) =>
      languageServer.onNotification((method, params) => {
        expect(method).toMatchInlineSnapshot(
          `"textDocument/publishDiagnostics"`,
        );
        expect(params).toMatchInlineSnapshot(`
          Object {
            "diagnostics": Array [
              Object {
                "message": "Alert deprecated: X.x
          do not use",
                "range": Object {
                  "end": Object {
                    "character": 19,
                    "line": 6,
                  },
                  "start": Object {
                    "character": 16,
                    "line": 6,
                  },
                },
                "severity": 2,
                "source": "ocamllsp",
              },
            ],
            "uri": "file:///test.ml",
          }
        `);
        resolve(null);
      }),
    );
    await openDocument(outdent`
      module X : sig
        val x : unit
        [@@ocaml.deprecated "do not use"]
      end = struct
        let x = ()
      end
      let () = ignore X.x
    `);
    await receivedDiganostics;
  });

  it("no diagnostics for valid files", async () => {
    let receivedDiganostics = new Promise((resolve, _reject) =>
      languageServer.onNotification((method, params) => {
        expect(method).toMatchInlineSnapshot(
          `"textDocument/publishDiagnostics"`,
        );
        expect(params).toMatchInlineSnapshot(`
          Object {
            "diagnostics": Array [],
            "uri": "file:///test.ml",
          }
        `);
        resolve(null);
      }),
    );

    await openDocument(outdent`
      let num = 42
    `);

    await receivedDiganostics;
  });
});
