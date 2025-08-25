## vim: ft=makojava

package com.adacore.lklsp;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.langkit_support.LangkitSupport.ProjectManager;
import com.adacore.langkit_support.LangkitSupport.DummyProjectManager;
import com.adacore.${ctx.lib_name.lower}.${ctx.lib_name};
import com.adacore.lsp.*;

import org.eclipse.lsp4j.services.*;
import org.eclipse.lsp4j.jsonrpc.Launcher;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class ${ctx.config.library.language_name.camel}Ls
{
    public static void main(String[] args)
            throws InterruptedException, ExecutionException {
        startServer(System.in, System.out);
    }

    /**
     * Starts the language server given the input and output streams to read
     * and write messages.
     *
     * @param in  input stream.
     * @param out output stream.
     * @throws InterruptedException
     * @throws ExecutionException
     */
    public static void startServer(InputStream in, OutputStream out)
            throws InterruptedException, ExecutionException {
        ${ctx.lib_name}.AnalysisContext ctx =
            ${ctx.lib_name}.AnalysisContext.create();

        ProjectManager pManager = new DummyProjectManager(
            ctx,
            List.of(${
                ",".join([
                    f'"{ext}"'
                    for ext in ctx.config.language_server.file_extensions
                ])
            })
        );

        LangkitLanguageServer server = new LangkitLanguageServer(pManager);
        Launcher<LanguageClient> launcher = Launcher.createLauncher(
            server, LanguageClient.class, in, out
        );
        LanguageClient client = launcher.getRemoteProxy();
        server.connect(client);
        Future<?> startListening = launcher.startListening();
        startListening.get();
    }
}
