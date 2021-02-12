import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";
import * as Types from "vscode-languageserver-types";
import { testUri, toEqualUri } from "./../src/LanguageServer";

expect.extend({
  toEqualUri: toEqualUri(this),
});

declare global {
  namespace jest {
    interface Matchers<R> {
      toEqualUri(uri: string): R;
    }
  }
}

describe("textDocument/definition", () => {
  let languageServer = null;

  async function openDocument(source) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        testUri("test.ml"),
        "ocaml",
        0,
        source,
      ),
    });
  }

  async function queryDefinition(position) {
    return await languageServer.sendRequest("textDocument/definition", {
      textDocument: Types.TextDocumentIdentifier.create(testUri("test.ml")),
      position,
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("returns location of a definition", async () => {
    await openDocument(outdent`
      let x = 43

      let () =
        print_int x
    `);

    let result = await queryDefinition(Types.Position.create(3, 12));

    expect(result.length).toBe(1);
    expect(result[0].range).toMatchObject({
      end: { character: 4, line: 0 },
      start: { character: 4, line: 0 },
    });
    expect(result[0].uri).toEqualUri(testUri("file.ml"));
  });
});
