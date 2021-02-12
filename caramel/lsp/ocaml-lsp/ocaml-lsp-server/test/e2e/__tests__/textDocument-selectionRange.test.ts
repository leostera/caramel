import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

describe("textDocument/selectionRange", () => {
  let languageServer = null;

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

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

  async function selectionRange(positions: Types.Position[]) {
    return await languageServer.sendRequest("textDocument/selectionRange", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      positions: positions,
    });
  }

  it("returns a selection range for modules", async () => {
    await openDocument(outdent`
      let foo a b =
        let min_ab = min a b in
        let max_ab = max a b in
        min_ab * max_ab
        `);

    let result = await selectionRange([Types.Position.create(1, 17)]);
    expect(result).toMatchObject([
      {
        range: {
          start: {
            line: 1,
            character: 15,
          },
          end: {
            line: 1,
            character: 18,
          },
        },
      },
    ]);
  });
});
