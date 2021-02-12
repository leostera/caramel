import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

describe("textDocument/foldingRange", () => {
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

  async function foldingRange() {
    return await languageServer.sendRequest("textDocument/foldingRange", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
    });
  }

  it("returns folding ranges for modules", async () => {
    await openDocument(outdent`
          module type X = sig
            type t
          end

          module X = struct
            module Y = struct
              let bar = 42
            end
            let foo = 3
          end

          class foobar = object
            method add x y = x + y
          end
        `);

    let result = await foldingRange();
    expect(result).toMatchObject([
      {
        endCharacter: 3,
        endLine: 2,
        kind: "region",
        startCharacter: 0,
        startLine: 0,
      },
      {
        endCharacter: 3,
        endLine: 9,
        kind: "region",
        startCharacter: 0,
        startLine: 4,
      },
      {
        endCharacter: 5,
        endLine: 7,
        kind: "region",
        startCharacter: 2,
        startLine: 5,
      },
      {
        endCharacter: 3,
        endLine: 13,
        kind: "region",
        startCharacter: 0,
        startLine: 11,
      },
    ]);
  });
});
