import com.adacore.libfoolang.Libfoolang.*;

/**
 * Check that exception handling for a property that returns an array works as
 * expected. It used to trigger a sefault.
 */
public final class Main {

    public static void main(String[] args) {
        AnalysisContext ctx = AnalysisContext.create();
        AnalysisUnit u = ctx.getUnitFromBuffer("example", "main.txt");

        FooNodeArray nodes;
        System.out.println("About to call pProp...");
        try {
            nodes = u.getRoot().pProp();
        } catch (LangkitException exc) {
            System.out.println(
                "Got an exception (" + exc.kind + "): " + exc.getMessage()
            );
        }

        System.out.println("Main.java: done.");
    }

}
