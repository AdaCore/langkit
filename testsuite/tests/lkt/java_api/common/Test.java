import java.nio.file.Path;

import com.adacore.langkit_support.NativeTools;
import com.adacore.liblktlang.Liblktlang;

public class Test {
    public static void main(String[] args) {
        // Create working variables
        var srcDir = Path.of("src").toAbsolutePath().toString();
        var otherSrcDir = Path.of("other_src").toAbsolutePath().toString();

        // Test with environment variables
        System.out.println("===== Test with environment variables =====\n");
        System.out.println("== Fetch \"foo\" in Lkt mode with LKT_PATH");
        try (var _ = new InEnv("LKT_PATH", srcDir)) {
            fetchFoo(Liblktlang.UnitProvider.createDefault(
                Liblktlang.LanguageMode.LKT
            ));
        }

        System.out.println("== Fetch \"foo\" in LKQL mode with LKQL_PATH");
        try (var _ = new InEnv("LKQL_PATH", srcDir)) {
            fetchFoo(Liblktlang.UnitProvider.createDefault(
                Liblktlang.LanguageMode.LKQL
            ));
        }

        System.out.println("== Fetch \"foo\" in Lkt mode with LKQL_PATH");
        try (var _ = new InEnv("LKQL_PATH", srcDir)){
            fetchFoo(Liblktlang.UnitProvider.createDefault(
                Liblktlang.LanguageMode.LKT
            ));
        }

        System.out.println("== Fetch \"foo\" in LKQL mode with LKT_PATH");
        try (var _ = new InEnv("LKT_PATH", srcDir)) {
            fetchFoo(Liblktlang.UnitProvider.createDefault(
                Liblktlang.LanguageMode.LKQL
            ));
        }

        System.out.println(
            "== Fetch \"foo\" in Lkt mode with no environment variables"
        );
        fetchFoo(Liblktlang.UnitProvider.createDefault(
            Liblktlang.LanguageMode.LKT
        ));

        // Test with explicit directories
        System.out.println("===== Test with explicit directories =====\n");
        System.out.println("== Fetch \"foo\" in Lkt mode in \"src\" dir");
        fetchFoo(Liblktlang.UnitProvider.createFromDirectories(
            Liblktlang.LanguageMode.LKT,
            new String[]{srcDir}
        ));

        System.out.println(
            "== Fetch \"foo\" in Lkt mode in \"other_src\" dirs"
        );
        fetchFoo(Liblktlang.UnitProvider.createFromDirectories(
            Liblktlang.LanguageMode.LKT,
            new String[]{otherSrcDir}
        ));

        System.out.println("== Fetch \"foo\" in LKQL mode in \"src\" dir");
        fetchFoo(Liblktlang.UnitProvider.createFromDirectories(
            Liblktlang.LanguageMode.LKQL,
            new String[]{srcDir}
        ));

        System.out.println(
            "== Fetch \"foo\" in LKQL mode in \"other_src\" dir"
        );
        fetchFoo(Liblktlang.UnitProvider.createFromDirectories(
            Liblktlang.LanguageMode.LKQL,
            new String[]{otherSrcDir}
        ));

        System.out.println("== Fetch \"foo\" in Lkt mode in no dirs");
        fetchFoo(Liblktlang.UnitProvider.createFromDirectories(
            Liblktlang.LanguageMode.LKT,
            new String[0]
        ));

        System.out.println("Test.java: Done");
    }

    /**
     * Create an analysis context with the specified provider and fetch the
     * "foo" unit in it. If it exists, display its parsing tree, otherwise
     * display a message.
     */
    private static void fetchFoo(Liblktlang.UnitProvider provider) {
        var ctx = Liblktlang.AnalysisContext.create(
            null,
            null,
            provider,
            null,
            true,
            8
        );
        var unit = ctx.getUnitFromProvider(
            "foo",
            Liblktlang.AnalysisUnitKind.UNIT_BODY
        );
        var root = unit.getRoot();
        if (!root.isNone()) {
            System.out.println(
                "\"foo\" unit resolved to "
                + Path.of("./")
                    .toAbsolutePath()
                    .relativize(Path.of(unit.getFileName(true)))
            );
            System.out.println("  '" + root.getText() + "'\n");
        } else {
            System.out.println("\"foo\" unit not found...\n");
        }
    }

    /** Support class to run in a modified environment. */
    private static class InEnv implements AutoCloseable {
        private final String key;
        private final String oldValue;

        public InEnv(String key, String value) {
            this.key = key;
            this.oldValue = System.getenv(this.key);
            try {
                NativeTools.setenv(this.key, value);
            } catch (Throwable t) {
                throw new RuntimeException(t);
            }
        }

        @Override
        public void close() {
            try {
                NativeTools.setenv(
                    this.key,
                    this.oldValue == null ? "" : this.oldValue
                );
            } catch (Throwable t) {
                throw new RuntimeException(t);
            }
        }
    }
}
