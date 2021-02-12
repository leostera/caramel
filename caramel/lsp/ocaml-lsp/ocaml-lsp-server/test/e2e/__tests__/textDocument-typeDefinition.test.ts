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

  async function openDocument(source: string) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        testUri("file.ml"),
        "ocaml",
        0,
        source,
      ),
    });
  }

  async function queryDefinition(position: Types.Position) {
    return await languageServer.sendRequest("textDocument/typeDefinition", {
      textDocument: Types.TextDocumentIdentifier.create(testUri("file.ml")),
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

  it("returns location of a type definition", async () => {
    await openDocument(outdent`
      (* type we are jumping on *)
      type t = T of int

      let x = T 43
    `);

    let result = await queryDefinition(Types.Position.create(3, 4));

    expect(result.length).toBe(1);
    expect(result[0].range).toMatchObject({
      end: { character: 0, line: 1 },
      start: { character: 0, line: 1 },
    });
    expect(result[0].uri).toEqualUri(testUri("file.ml"));
  });

  it("ignores names in values namespace", async () => {
    await openDocument(outdent`
      (* type we are jumping on *)
      type t = T of int

      let t = T 42
      let x = T 43
    `);

    let result = await queryDefinition(Types.Position.create(4, 4));

    expect(result.length).toBe(1);
    expect(result[0].range).toMatchObject({
      end: { character: 0, line: 1 },
      start: { character: 0, line: 1 },
    });
    expect(result[0].uri).toEqualUri(testUri("file.ml"));
  });

  it("ignores names in values namespace (cursor on same named value)", async () => {
    await openDocument(outdent`
      (* type we are jumping on *)
      type t = T of int

      let t = T 42
    `);

    let result = await queryDefinition(Types.Position.create(3, 4));

    expect(result.length).toBe(1);
    expect(result[0].range).toMatchObject({
      end: { character: 0, line: 1 },
      start: { character: 0, line: 1 },
    });
    expect(result[0].uri).toEqualUri(testUri("file.ml"));
  });
});
