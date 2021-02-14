import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

describe("textDocument/completion", () => {
  let languageServer: LanguageServer.LanguageServer = null;

  async function openDocument(source) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        source,
      ),
    });
  }

  async function queryCompletionItemResolve(
    label: string,
    position: Types.Position,
  ) {
    return languageServer.sendRequest("completionItem/resolve", {
      label: label,
      data: {
        textDocument: {
          uri: "file:///test.ml",
        },
        position: position,
      },
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("can get documentation for the end of document", async () => {
    openDocument(outdent`
      List.ma
    `);

    let response = await queryCompletionItemResolve(
      "map2",
      Types.Position.create(0, 5),
    );

    expect(response).toMatchInlineSnapshot(`
      Object {
        "documentation": " [List.map2 f [a1; ...; an] [b1; ...; bn]] is
         [[f a1 b1; ...; f an bn]].
         @raise Invalid_argument if the two lists are determined
         to have different lengths.  Not tail-recursive. ",
        "label": "map2",
      }
    `);
  });

  it("can get documentation at arbitrary position", async () => {
    openDocument(outdent`
      List.fld((=) 0) [1; 2; 3]
    `);

    let response = await queryCompletionItemResolve(
      "find_all",
      Types.Position.create(0, 5),
    );

    expect(response).toMatchInlineSnapshot(`
      Object {
        "documentation": " [find_all] is another name for {!List.filter}. ",
        "label": "find_all",
      }
    `);
  });

  it("can get documentation at arbitrary position (before dot)", async () => {
    openDocument(outdent`
      module Outer = struct
        (** documentation for [Inner] *)
        module Inner = struct
          let v = ()
        end
      end

      let _ = ();;

      Outer.Inner.v
    `);

    let response = await queryCompletionItemResolve(
      "Inner",
      Types.Position.create(9, 10),
    );

    expect(response).toMatchInlineSnapshot(`
      Object {
        "documentation": "documentation for [Inner]",
        "label": "Inner",
      }
    `);
  });
});
