import java.io.File;
import java.util.Arrays;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import java.math.BigInteger;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libfoolang.Libfoolang.*;

public final class InterfaceTests {

    // ----- Util methods -----

    /**
     * Display the section header with the given name
     *
     * @param name The name of the section
     */
    private static void header(String name) {
        System.out.println("--- " + name + " ---");
    }

    /**
     * Display the footer of a section
     *
     * @param name The name of the section
     */
    private static void footer(String name) {
        System.out.println("----" + "-".repeat(name.length()) + "----\n");
    }

    // ----- Test methods -----

    /**
     * Test the interfaces manipulation
     */
    private static void testInterfaces() {
        // Display header
        header("Interfaces");

        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            AnalysisUnit unit = context.getUnitFromFile("foo.txt");

            LangkitSupport.TypeInterface type =
                (LangkitSupport.TypeInterface) unit.getRoot().children()[0];
            System.out.println("TypeInterface: " + type);
            System.out.println("TypeInterface.full_name: " + type.gFullName());
            LangkitSupport.DefiningNameInterface typeId = type.gDefiningName();
            System.out.println("DefiningNameInterface: " + typeId);
            System.out.println(
                "DefiningNameInterface.full_name: " + typeId.gFullName()
            );
            LangkitSupport.RefResultInterface[] refs =
                typeId.gFindAllReferences(new AnalysisUnit[] {unit});
            System.out.println("Uses:");
            for (var ref : refs) {
                System.out.println("    " + ref.gRef());
            }
        }

        // Display footer
        footer("Interfaces");
    }

    /**
     * Run the Java tests one by one
     *
     * @param args The arguments for the tests running
     */
    public static void main(String[] args) {
        // Run the tests
        System.out.println("===== Start the Java tests =====\n");
        testInterfaces();
        System.out.println("===== End of the Java tests =====");
    }

}
