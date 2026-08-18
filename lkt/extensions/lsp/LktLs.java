
package com.adacore.lklsp;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.langkit_support.LangkitSupport.DummyProjectManager;
import com.adacore.langkit_support.LangkitSupport.ProjectManager;
import com.adacore.liblktlang.Liblktlang;
import com.adacore.lsp.*;

import org.eclipse.lsp4j.services.*;
import org.eclipse.lsp4j.jsonrpc.Launcher;

import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.List;

public class LktLs {

    private static void usage() {
        System.out.println("""
        usage: lktls [--help] [--mode={lkt,lkql}]
        
            -h, --help       display usage and exit
            --mode={mode}    select the language mode + \
            (possible values: `lkt`, `lkql`)
        """);
    }

    private static Liblktlang.LanguageMode getMode(List<String> args) {
        String modeValue = null;
        for (int i = 0; i < args.size() && modeValue == null; ++i) {
            if (args.get(i).equals("--mode")) {
                if (i + 1 < args.size()) {
                    modeValue = args.get(i + 1);
                }
            }
            else if (args.get(i).startsWith("--mode=")) {
                modeValue = args.get(i).substring(args.get(i).indexOf('=') + 1);
            }
        }

        return switch (modeValue) {
            case "lkt"  -> Liblktlang.LanguageMode.LKT;
            case "lkql" -> Liblktlang.LanguageMode.LKQL;
            default -> throw new IllegalArgumentException(
                "unknown mode: " + modeValue
            );
        };
    }

    public static void main(String[] args)
            throws InterruptedException, ExecutionException {
        List<String> argList = List.of(args);
        if (argList.contains("-h") || argList.contains("--help")) {
            usage();
            return;
        }
        try {
            Liblktlang.LanguageMode mode = getMode(argList);
            startServer(System.in, System.out, mode);
        } catch (Throwable t) {
            usage();
            throw t;
        }
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
    public static void startServer(
        InputStream in,
        OutputStream out,
        Liblktlang.LanguageMode mode
    ) throws InterruptedException, ExecutionException {
        Liblktlang.UnitProvider provider =
            Liblktlang.UnitProvider.createDefault(mode);
        Liblktlang.AnalysisContext ctx = Liblktlang.AnalysisContext.create(
                null,
                null,
                provider,
                null,
                true,
                1);

        // Fetch the LKQL prelude when working in LKQL mode
        if (mode == Liblktlang.LanguageMode.LKQL) {
            try {
                String content = new String(
                    LktLs.class.getResourceAsStream("/prelude.lkql")
                            .readAllBytes()
                );
                ctx.getUnitFromBuffer(content, "__prelude");
            } catch (IOException e) {
                System.err.println(
                    "Could not find LKQL prelude," +
                    " there might be erroneous results in nameres"
                );
            }
        }

        ProjectManager pManager = new DummyProjectManager(
                ctx,
                List.of(
                        mode == Liblktlang.LanguageMode.LKT
                                ? ".lkt"
                                : ".lkql"));

        LangkitLanguageServer server = new LangkitLanguageServer(pManager);
        Launcher<LanguageClient> launcher = Launcher.createLauncher(
                server, LanguageClient.class, in, out);
        LanguageClient client = launcher.getRemoteProxy();
        server.connect(client);
        Future<?> startListening = launcher.startListening();
        startListening.get();
    }
}
