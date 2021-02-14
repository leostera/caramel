import * as cp from "child_process";
import * as os from "os";
import * as path from "path";
import * as rpc from "vscode-jsonrpc/node";

import * as Protocol from "vscode-languageserver-protocol";
import { URI } from "vscode-uri";

const ocamlVersion = cp.execSync("ocamlc --version").toString();
export function ocamlVersionGEq(versString: string) {
  return ocamlVersion >= versString;
}

let serverBin = os.platform() === "win32" ? "ocamllsp.exe" : "ocamllsp";

let serverPath = path.join(
  __dirname,
  "..",
  "..",
  "..",
  "..",
  "_build",
  "install",
  "default",
  "bin",
  serverBin,
);

export type LanguageServer = rpc.MessageConnection;

let prefix = process.platform === "win32" ? "file:///" : "file://";

export const toURI = (s) => {
  return prefix + s;
};

export const start = (opts?: cp.SpawnOptions) => {
  opts = opts || {
    env: { ...process.env, OCAML_LSP_SERVER_LOG: "-" },
  };
  let childProcess = cp.spawn(serverPath, [], opts);

  let connection = rpc.createMessageConnection(
    new rpc.StreamMessageReader(childProcess.stdout),
    new rpc.StreamMessageWriter(childProcess.stdin),
  );

  childProcess.stderr.on("data", (d) => {
    if (process.env.OCAMLLSP_TEST_DEBUG) {
      console.log("Received data: " + d);
    }
  });

  connection.listen();

  return connection as LanguageServer;
};

export const startAndInitialize = async (
  capabilities?: Partial<Protocol.ClientCapabilities>,
) => {
  let languageServer = start();

  capabilities = capabilities || {};

  let initializeParameters: Protocol.InitializeParams = {
    processId: process.pid,
    rootUri: toURI(path.join(__dirname, "..")),
    capabilities: capabilities,
    workspaceFolders: [],
  };

  await languageServer.sendRequest(
    Protocol.InitializeRequest.type,
    initializeParameters,
  );
  return languageServer;
};

export const exit = async (languageServer: rpc.MessageConnection) => {
  let ret = new Promise((resolve, _reject) => {
    languageServer.onClose(() => {
      languageServer.dispose();
      resolve(null);
    });
  });

  let notification = new rpc.NotificationType<string>("exit");
  languageServer.sendNotification(notification);

  return ret;
};

export const testUri = (file: string) => {
  return URI.file(file).toString();
};

export const toEqualUri = (obj) => {
  return (received: string, expected: string) => {
    const options = {
      comment: "Uri equality",
      isNot: obj.isNot,
      promise: obj.promise,
    };
    const pass =
      URI.parse(received).toString() === URI.parse(received).toString();
    const message = pass
      ? () =>
          obj.utils.matcherHint("toEqualUri", undefined, undefined, options) +
          "\n\n" +
          `Expected: not ${obj.utils.printExpected(expected)}\n` +
          `Received: ${obj.utils.printReceived(received)}`
      : () =>
          obj.utils.matcherHint("toBe", undefined, undefined, options) +
          "\n\n" +
          `Expected: ${obj.utils.printExpected(expected)}\n` +
          `Received: ${obj.utils.printReceived(received)}`;
    return { pass, message };
  };
};
