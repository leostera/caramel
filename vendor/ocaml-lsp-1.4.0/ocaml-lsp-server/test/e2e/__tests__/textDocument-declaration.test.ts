import { promises as fs } from "fs";
import * as path from "path";
import * as child_process from "child_process";
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

describe("textDocument/declaration", () => {
  let languageServer = null;

  let testWorkspacePath = path.join(__dirname, "declaration_files/");

  let createPathForFile = (filename: string) =>
    path.join(testWorkspacePath, filename);

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  async function openDocument(filepath) {
    let source = await fs.readFile(filepath);
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        testUri(filepath),
        "ocaml",
        0,
        source.toString(),
      ),
    });
  }

  async function queryDeclaration(filepath, position) {
    return await languageServer.sendRequest("textDocument/declaration", {
      textDocument: Types.TextDocumentIdentifier.create(testUri(filepath)),
      position,
    });
  }

  it("returns location of a declaration", async () => {
    child_process.execSync("dune build", { cwd: testWorkspacePath });

    await openDocument(createPathForFile("main.ml"));

    let result = await queryDeclaration(
      createPathForFile("main.ml"),
      Types.Position.create(0, 13),
    );

    expect(result.length).toBe(1);
    expect(result[0].range).toMatchObject({
      end: { character: 0, line: 0 },
      start: { character: 0, line: 0 },
    });
    expect(result[0].uri).toEqualUri(testUri(createPathForFile("lib.ml")));
  });
});
