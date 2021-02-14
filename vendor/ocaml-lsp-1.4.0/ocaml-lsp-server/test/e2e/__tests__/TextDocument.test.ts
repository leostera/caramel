import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

describe("TextDocument: incremental sync", () => {
  let languageServer: LanguageServer.LanguageServer;

  async function getDoc(languageServer: LanguageServer.LanguageServer) {
    let result = await languageServer.sendRequest("debug/textDocument/get", {
      textDocument: Types.TextDocumentIdentifier.create(
        "file:///test-document.txt",
      ),
      position: Types.Position.create(0, 0),
    });
    return result;
  }

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("Manages unicode character ranges correctly", async () => {
    languageServer = await LanguageServer.startAndInitialize();
    languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test-document.txt",
        "ocaml",
        0,
        outdent`
          let x = 4
          let y = "a𐐀b"
        `,
      ),
    });
    languageServer.sendNotification("textDocument/didChange", {
      textDocument: Types.VersionedTextDocumentIdentifier.create(
        "file:///test-document.txt",
        1,
      ),
      contentChanges: [
        {
          range: {
            start: { line: 1, character: 10 },
            end: { line: 1, character: 12 },
          },
          text: "",
        },
      ],
    });

    expect(await getDoc(languageServer)).toEqual('let x = 4\nlet y = "ab"');
  });

  it("updates in the middle of the line", async () => {
    languageServer = await LanguageServer.startAndInitialize();
    languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test-document.txt",
        "ocaml",
        0,
        "let x = 1;\n\nlet y = 2;",
      ),
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\n\nlet y = 2;");

    languageServer.sendNotification("textDocument/didChange", {
      textDocument: Types.VersionedTextDocumentIdentifier.create(
        "file:///test-document.txt",
        1,
      ),
      contentChanges: [
        {
          range: {
            start: { line: 2, character: 5 },
            end: { line: 2, character: 5 },
          },
          rangeLength: 0,
          text: "1",
        },
      ],
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\n\nlet y1 = 2;");

    languageServer.sendNotification("textDocument/didChange", {
      textDocument: Types.VersionedTextDocumentIdentifier.create(
        "file:///test-document.txt",
        1,
      ),
      contentChanges: [
        {
          range: {
            start: { line: 2, character: 5 },
            end: { line: 2, character: 6 },
          },
          rangeLength: 1,
          text: "",
        },
      ],
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\n\nlet y = 2;");
  });

  it("updates in at the start of the line", async () => {
    languageServer = await LanguageServer.startAndInitialize();

    languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test-document.txt",
        "ocaml",
        0,
        "let x = 1;\n\nlet y = 2;",
      ),
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\n\nlet y = 2;");

    languageServer.sendNotification("textDocument/didChange", {
      textDocument: Types.VersionedTextDocumentIdentifier.create(
        "file:///test-document.txt",
        1,
      ),
      contentChanges: [
        {
          range: {
            start: { line: 1, character: 0 },
            end: { line: 1, character: 0 },
          },
          rangeLength: 0,
          text: "s",
        },
      ],
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\ns\nlet y = 2;");
  });

  it("update when inserting a line", async () => {
    languageServer = await LanguageServer.startAndInitialize();

    languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test-document.txt",
        "ocaml",
        0,
        "let x = 1;\n\nlet y = 2;",
      ),
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\n\nlet y = 2;");

    languageServer.sendNotification("textDocument/didChange", {
      textDocument: Types.VersionedTextDocumentIdentifier.create(
        "file:///test-document.txt",
        1,
      ),
      contentChanges: [
        {
          range: {
            start: { line: 0, character: 10 },
            end: { line: 0, character: 10 },
          },
          rangeLength: 0,
          text: "\nlet x = 1;",
        },
      ],
    });

    expect(await getDoc(languageServer)).toEqual(
      "let x = 1;\nlet x = 1;\n\nlet y = 2;",
    );
  });

  it("update when inserting a line at the end of the doc", async () => {
    languageServer = await LanguageServer.startAndInitialize();

    languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test-document.txt",
        "ocaml",
        0,
        "let x = 1;\n\nlet y = 2;",
      ),
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\n\nlet y = 2;");

    languageServer.sendNotification("textDocument/didChange", {
      textDocument: Types.VersionedTextDocumentIdentifier.create(
        "file:///test-document.txt",
        1,
      ),
      contentChanges: [
        {
          range: {
            start: { line: 2, character: 10 },
            end: { line: 2, character: 10 },
          },
          rangeLength: 0,
          text: "\nlet y = 2;",
        },
      ],
    });

    expect(await getDoc(languageServer)).toEqual(
      "let x = 1;\n\nlet y = 2;\nlet y = 2;",
    );
  });

  it("update when deleting a line", async () => {
    languageServer = await LanguageServer.startAndInitialize();

    languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test-document.txt",
        "ocaml",
        0,
        "let x = 1;\n\nlet y = 2;",
      ),
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\n\nlet y = 2;");

    languageServer.sendNotification("textDocument/didChange", {
      textDocument: Types.VersionedTextDocumentIdentifier.create(
        "file:///test-document.txt",
        1,
      ),
      contentChanges: [
        {
          range: {
            start: { line: 0, character: 0 },
            end: { line: 1, character: 0 },
          },
          rangeLength: 11,
          text: "",
        },
      ],
    });

    expect(await getDoc(languageServer)).toEqual("\nlet y = 2;");
  });
});

describe("TextDocument", () => {
  let languageServer;

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  describe("didOpen", () => {
    it("stores text document", async () => {
      languageServer = await LanguageServer.startAndInitialize();
      languageServer.sendNotification("textDocument/didOpen", {
        textDocument: Types.TextDocumentItem.create(
          "file:///test-document.txt",
          "ocaml",
          0,
          "Hello, World!",
        ),
      });

      let result = await languageServer.sendRequest("debug/textDocument/get", {
        textDocument: Types.TextDocumentIdentifier.create(
          "file:///test-document.txt",
        ),
        position: Types.Position.create(0, 0),
      });

      expect(result).toEqual("Hello, World!");
    });
  });

  describe("didChange", () => {
    it("updates text document", async () => {
      languageServer = await LanguageServer.startAndInitialize();
      languageServer.sendNotification("textDocument/didOpen", {
        textDocument: Types.TextDocumentItem.create(
          "file:///test-document.txt",
          "ocaml",
          0,
          "Hello, World!",
        ),
      });

      languageServer.sendNotification("textDocument/didChange", {
        textDocument: Types.VersionedTextDocumentIdentifier.create(
          "file:///test-document.txt",
          1,
        ),
        contentChanges: [{ text: "Hello again!" }],
      });

      let result = await languageServer.sendRequest("debug/textDocument/get", {
        textDocument: Types.TextDocumentIdentifier.create(
          "file:///test-document.txt",
        ),
        position: Types.Position.create(0, 0),
      });

      expect(result).toEqual("Hello again!");
    });
  });
});
