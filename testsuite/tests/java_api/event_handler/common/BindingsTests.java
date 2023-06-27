import com.adacore.libfoolang.Libfoolang.*;

public final class BindingsTests {

    private static void testEventHandlers(
        EventHandler.UnitRequestedCallback unitRequestedCallback,
        EventHandler.UnitParsedCallback unitParsedCallback
    ) {

        // Create the context with the event handler and create units
        try(
            EventHandler eventHandler = EventHandler.create(
                unitRequestedCallback,
                unitParsedCallback
            );
            AnalysisContext context = AnalysisContext.create(
                null,
                null,
                null,
                eventHandler,
                true,
                8
            );
        ) {
            // Parse the unit twice to test the "reparsed" argument
            AnalysisUnit unit = context.getUnitFromBuffer("example", "example");
            unit = context.getUnitFromBuffer("example", "example");

            // Call the property to trigger "unit requested" callback
            FooNode root = unit.getRoot();
            root.pTriggerUnitRequested(Symbol.create("foo_1"), false, false);
            root.pTriggerUnitRequested(Symbol.create("foo_2"), true, false);
            root.pTriggerUnitRequested(Symbol.create("foo_3"), true, true);
            root.pTriggerUnitRequested(Symbol.create("foo_4"), false, true);
        }

    }

    public static void main(String[] args) {
        // Create the callback functions
        EventHandler.UnitRequestedCallback unitRequestedCallback = (
            AnalysisContext context,
            String name,
            AnalysisUnit from,
            boolean found,
            boolean isNotFoundError
        ) -> {
            System.out.println("--- Unit requested callback");
            System.out.println("name: " + name);
            System.out.println("from: " + from);
            System.out.println("found: " + found);
            System.out.println("is_not_found_error: " + isNotFoundError);
            System.out.println();
        };
        EventHandler.UnitParsedCallback unitParsedCallback = (
            AnalysisContext context,
            AnalysisUnit unit,
            boolean reparsed
        ) -> {
            System.out.println("--- Unit parsed callback");
            System.out.println("unit: " + unit);
            System.out.println("reparsed: " + reparsed);
            System.out.println();
        };

        System.out.println("=== Non null callbacks ===");
        testEventHandlers(
            unitRequestedCallback,
            unitParsedCallback
        );
        System.out.println("=== Null callbacks ===");
        testEventHandlers(null, null);
    }

}
