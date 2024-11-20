<%
lang_name = ctx.config.library.language_name.camel
short_name = ctx.short_name_or_long
%>
import * as path from 'path';
import * as vscode from 'vscode';
import {
    Executable,
    LanguageClient,
    LanguageClientOptions,
    TransportKind,
} from 'vscode-languageclient/node';


let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {

    interface Dictionary<T> {
        [Key: string]: T;
    }

    // Specify which executable to run and where the dynamic libraries are
    const serverPath : Dictionary<Dictionary<string>> = {
        "x64": {
            "linux": context.asAbsolutePath("x64/linux/bin/${short_name}ls"),
            "win32":
                context.asAbsolutePath("x64/win32/bin/${short_name}ls.exe"),
        }
    }
    const libs: Dictionary<Dictionary<Dictionary<string>>> = {
        "x64": {
            "linux": {
                "LD_LIBRARY_PATH":
                    context.asAbsolutePath("x64/linux/lib")
                    + path.delimiter
                    + `${'${process.env["LD_LIBRARY_PATH"]'}}`
            },
            "win32": {
                "PATH":
                    context.asAbsolutePath("x64/win32/lib")
                    + path.delimiter
                    + `${'${process.env["PATH"]'}}`
            },
        }
    }
    const serverOptions: Executable = {
        command:
            serverPath[String(process.arch)][String(process.platform)],
        transport: TransportKind.stdio,
        options: { env: libs[process.arch][process.platform] }
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: "${lang_name}" }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher("**/.clientc")
        }
    }

    client = new LanguageClient(
        "${short_name}LanguageServer",
        "${lang_name} Language Server",
        serverOptions,
        clientOptions
    );

    // Start the client and the server.
    client.start();
}

export function deactivate() {
    if (!client)
        return undefined;
    return client.stop();
}
