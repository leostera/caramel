import outdent from "outdent";
import * as path from "path";
import * as LanguageServer from "../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

describe("textDocument/codeAction", () => {
  let languageServer = null;

  async function openDocument(source, uri) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(uri, "ocaml", 0, source),
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  async function codeAction(uri, start, end) {
    return await languageServer.sendRequest("textDocument/codeAction", {
      textDocument: Types.TextDocumentIdentifier.create(uri),
      context: { diagnostics: [] },
      range: { start, end },
    });
  }

  it("can destruct sum types", async () => {
    await openDocument(
      outdent`
type t = Foo of int | Bar of bool

let f (x : t) = x
`,
      "file:///test.ml",
    );
    let start = Types.Position.create(2, 16);
    let end = Types.Position.create(2, 17);
    let actions = await codeAction("file:///test.ml", start, end);
    expect(actions).toMatchObject([
      {
        edit: {
          changes: {
            "file:///test.ml": [
              {
                newText: "match x with | Foo _ -> (??) | Bar _ -> (??)",
                range: {
                  end: {
                    character: 17,
                    line: 2,
                  },
                  start: {
                    character: 16,
                    line: 2,
                  },
                },
              },
            ],
          },
        },
        kind: "destruct",
        title: "Destruct",
      },
    ]);
  });

  it("can infer module interfaces", async () => {
    await openDocument(
      outdent`
type t = Foo of int | Bar of bool

let f (x : t) = x
`,
      "file:///test.ml",
    );
    await openDocument("", "file:///test.mli");
    let start = Types.Position.create(0, 0);
    let end = Types.Position.create(0, 0);
    let actions = await codeAction("file:///test.mli", start, end);
    expect(actions).toMatchObject([
      {
        edit: {
          changes: {
            "file:///test.mli": [
              {
                newText: "type t = Foo of int | Bar of bool\nval f : t -> t\n",
                range: {
                  start: {
                    character: 0,
                    line: 0,
                  },
                  end: {
                    character: 0,
                    line: 0,
                  },
                },
              },
            ],
          },
        },
        kind: "inferred_intf",
        title: "Insert inferred interface",
      },
    ]);
  });

  it("opens the implementation if not in store", async () => {
    let testWorkspacePath = path.join(__dirname, "declaration_files/");
    let intfFilepath = path.join(testWorkspacePath, "lib.mli");
    let intfUri = "file://" + intfFilepath;
    await openDocument("", intfUri);
    let start = Types.Position.create(0, 0);
    let end = Types.Position.create(0, 0);
    let actions = await codeAction(intfUri, start, end);
    let changes = {};
    changes[intfUri] = [
      {
        newText: "val x : int\n",
        range: {
          end: {
            character: 0,
            line: 0,
          },
          start: {
            character: 0,
            line: 0,
          },
        },
      },
    ];
    expect(actions).toMatchObject([
      {
        edit: {
          changes: changes,
        },
        kind: "inferred_intf",
        title: "Insert inferred interface",
      },
    ]);
  });
});
