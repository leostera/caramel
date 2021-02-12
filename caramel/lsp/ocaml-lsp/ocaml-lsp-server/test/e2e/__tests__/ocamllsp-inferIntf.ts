import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

describe("ocamllsp/inferIntf", () => {
  let languageServer = null;

  async function openDocument(source, name) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///" + name,
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

  async function inferIntf(name: string) {
    return await languageServer.sendRequest(
      "ocamllsp/inferIntf",
      `file:///${name}`,
    );
  }

  it("can infer module interfaces", async () => {
    await openDocument(
      outdent`
type t = Foo of int | Bar of bool

let f (x : t) = x
`,
      "test.ml",
    );
    let actions = await inferIntf("test.ml");
    expect(actions).toEqual(
      "type t = Foo of int | Bar of bool\nval f : t -> t\n",
    );
  });
});
