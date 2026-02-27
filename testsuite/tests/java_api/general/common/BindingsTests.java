import java.io.File;
import java.util.Arrays;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import java.math.BigInteger;

import com.adacore.libfoolang.Libfoolang.*;

public final class BindingsTests {

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

    /**
     * Display the information about the tokens in the file
     *
     * @param file The file to get tokens from
     * @param withTrivia If the info should include trivia
     */
    private static void tokensInfo(String file, boolean withTrivia) {
        try(
            AnalysisContext context = AnalysisContext.create(
                null,
                null,
                null,
                null,
                withTrivia,
                8
            )
        ) {
            AnalysisUnit unit = context.getUnitFromFile(file);
            Token first = unit.getFirstToken();
            Token last = unit.getLastToken();
            FooNode root = unit.getRoot();
            Token rootStart = root.tokenStart();
            Token rootEnd = root.tokenEnd();
            System.out.println(
                "  unit_text =\n    " + unit.getText() +
                "  first_token = " + first.toString() +
                "\n  first_text = " + first.getText() +
                "\n  last_token = " + last.toString() +
                "\n  last_text = " + last.getText() +
                "\n  root_node = " + root.toString() +
                "\n  root_start = " + rootStart.toString() +
                "\n  root_end = " + rootEnd.toString() +
                "\n  token_count = " + unit.getTokenCount() +
                "\n  trivia_count = " + unit.getTriviaCount()
            );
        }
    }

    // ----- Test methods -----

    /**
     * Test the diagnostic manipulation
     */
    private static void testDiagnostics() {
        // Display header
        header("Diagnostics");

        // Prepare the working variables
        AnalysisUnit unit;
        Diagnostic[] diags;

        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            // Create an unit from an unknown file
            unit = context.getUnitFromFile("unknown");
            diags = unit.getDiagnostics();
            for(Diagnostic diag : diags) {
                System.out.println(diag.toString());
            }

            // Create a unit with a synthax error
            unit = context.getUnitFromBuffer("var identifier", "foo.txt");
            diags = unit.getDiagnostics();
            for(Diagnostic diag : diags) {
                System.out.println(diag.toString());
            }
        }

        // Display footer
        footer("Diagnostics");
    }

    /**
     * Test the unit file name getting
     */
    private static void testUnitFileName() {
        // Display the header
        header("Unit Filename");

        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            // Get the unit for the foo.txt file
            AnalysisUnit unit = context.getUnitFromFile("foo.txt");
            AnalysisUnit other = context.getUnitFromFile("foo.txt");
            AnalysisUnit notSame =
                context.getUnitFromBuffer("var identifier", "bar.txt");
            System.out.println(
                unit.toString() + " != " + other + " = " + (unit != other)
            );
            System.out.println(
                unit.toString() + ".equals(" + other + ") = " +
                unit.equals(other)
            );
            System.out.println(
                unit.toString() + ".equals(" + notSame +
                ") = " + unit.equals(notSame)
            );
            System.out.println(
                "Unit for the file " +
                new File(unit.getFileName()).getName()
            );
        }

        // Display the footer
        footer("Unit Filename");
    }

    /**
     * Test getting a unit from the unit provider.
     */
    private static void testUnitProvider() {
       header("Unit provider");

       AnalysisContext ctx = AnalysisContext.create();
       AnalysisUnit unit = ctx.getUnitFromProvider(
           Text.create("pkg"),
           AnalysisUnitKind.UNIT_BODY
       );
       System.out.println(
           "pkg/body resolved to: "
           + new File(unit.getFileName()).getName()
       );

       footer("Unit provider");
    }

    /**
     * Test the token manipulation
     */
    private static void testTokens() {
        // Display the header
        header("Tokens");

        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            // Test the trivia filtering
            System.out.println("--- Token info without trivia");
            tokensInfo("foo.txt", false);
            System.out.println("--- Token info with trivia");
            tokensInfo("foo.txt", true);
            System.out.println("");

            // Parse the foo.txt file to inspect its tokens
            AnalysisUnit unit = context.getUnitFromFile("foo.txt");
            Token first = unit.getFirstToken();
            Token none = first.previous();
            System.out.println("The NO_TOKEN = " + none.toString());
            System.out.println("Text range with a NO_TOKEN = " +
                Token.textRange(first, none));

            // Test the token equivalence and equality
            AnalysisUnit eqUnit = context.getUnitFromBuffer(
                "null identifier example identifier example",
                "foo.txt"
            );
            Token current = eqUnit.getFirstToken();
            while(!(current.isNone())) {
                Token other = current.next();
                if(!current.isTrivia()) {
                    while(!(other.isNone())) {
                        if(current.isEquivalent(other))
                            System.out.println(
                                "Equivalent tokens : " +
                                current.toString() + " and " +
                                other.toString()
                            );
                        other = other.next();
                    }
                }
                current = current.next();
            }

            Token left = eqUnit.getFirstToken();
            Token right = eqUnit.getFirstToken();
            while(!(left.isNone() || right.isNone())) {
                System.out.println(
                    left.toString() + " != " + right + " = " + (left != right)
                );
                System.out.println(
                    left.toString() + ".equals(" + right + ") = " +
                    left.equals(right)
                );
                left = left.next();
                right = right.next();
            }
        }

        // Display the footer
        footer("Tokens");
    }

    /**
     * Test the node manipulation
     */
    private static void testNodes() {
        // Display the header
        header("Nodes");

        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            AnalysisUnit unit = context.getUnitFromFile("foo.txt");
            FooNode root = unit.getRoot();

            System.out.println("Unit = " + unit.toString());
            System.out.println("Unit root = " + root.toString());
            System.out.println("Unit root kind = " + root.getClassName());
            System.out.println(
                "Unit root children count = " + root.getChildrenCount()
            );
            System.out.println(
                "Unit root children = " + Arrays.toString(root.children())
            );
            System.out.println(
                "Unit root tree dump = " + root.dumpTree()
            );
            System.out.println(
                "Unit root is a list node = " + root.isListNode()
            );
            System.out.println(
                "Unit root is a token node = " + root.isTokenNode()
            );
            System.out.println("Unit root image = " + root.getImage());
            System.out.println("Unit root text = " + root.getText());
            Sequence sequence = (Sequence) root;
            System.out.println("Iterating on root sequence:");
            for (FooNode node : sequence) {
                System.out.println("  - " + node.toString());
            }
            FooNode[] items = sequence.pAllItems();
            System.out.println(
                "Root \"p_all_items\" = " + Arrays.toString(items)
            );
            Example[] examples = sequence.pExampleItems();
            System.out.println(
                "Root \"p_example_items\" = " + Arrays.toString(examples)
            );
            Var var = (Var) root.getChild(2);
            System.out.println("Var (3rd child) = " + var.toString());
            System.out.println("Var image = " + var.getImage());
            System.out.println(
                "Var fields = " + Arrays.toString(var.getFieldNames())
            );
            Sequence arg = var.fArg();
            FooNode[] argItems = arg.pAllItems();
            System.out.println("Var arg = " + arg.toString());
            System.out.println("Var content = " + Arrays.toString(argItems));
            Var var2 = (Var) root.getChild(2);
            System.out.println("Node equality = " + var.equals(var2));
            System.out.println(
                "Node hash equality = " + (var.hashCode() == var2.hashCode())
            );
            System.out.println(
                "Node 'p_all_items' member reference equality = " +
                (root.getFieldDescription("p_all_items").memberRef ==
                 MemberReference.FOO_SEQUENCE_P_ALL_ITEMS)
            );

            // Test node equality and hash codes
            FooNode e1 = root.pWithMd(true, false);
            FooNode e2 = root.pWithMd(true, true);
            FooNode e3 = root.pWithMd(false, true);
            System.out.println("e1.equals(e2) = " + e1.equals(e2));
            System.out.println("e1.equals(e3) = " + e1.equals(e3));
            System.out.println(
                "hash(e1) == hash(e2) = " + (e1.hashCode() == e2.hashCode())
            );
            System.out.println(
                "hash(e1) == hash(e3) = " + (e1.hashCode() == e3.hashCode())
            );

            // Test empty struct nullity
            EmptyStruct emptyStruct = root.pNewEmptyStruct();
            System.out.println(
                "empty struct is_struct_null() = " +
                root.pIsStructNull(emptyStruct)
            );
            System.out.println(
                "null empty struct is_struct_null() = " +
                root.pIsStructNull(EmptyStruct.NONE)
            );
        }

        // Display the footer
        footer("Nodes");
    }

    /**
     * Test the parent navigation
     */
    private static void testParent() {
        // Display the header
        header("Parent(s)");

        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            AnalysisUnit unit = context.getUnitFromFile("foo.txt");
            FooNode root = unit.getRoot();

            // Visit all the nodes and display their parents
            List<FooNode> visitList = new LinkedList<>();
            visitList.add(root);
            while(!visitList.isEmpty()) {
                FooNode current = visitList.remove(0);
                FooNode parent = current.parent();
                System.out.println(
                    "Node " + current.toString() + " | Parents :"
                );
                while(!parent.isNone()) {
                    System.out.println("  " + parent.toString());
                    parent = parent.parent();
                }
                for(FooNode child : current.children()) {
                    if(!child.isNone()) visitList.add(child);
                }
                System.out.println("");
            }
            System.out.println(
                "proposition (for all node n. " +
                "for fields fields f of n. parent f = n) is true"
            );
        }

        // Display the footer
        footer("Parent(s)");
    }

    /**
     * Test the sibling navigation
     */
    private static void testSiblings() {
        // Display the header
        header("Siblings");

        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            AnalysisUnit unit = context.getUnitFromFile("foo.txt");
            Sequence root = (Sequence) unit.getRoot();
            FooNode[] items = root.pAllItems();

            // Visit all the items and display their siblings
            for(FooNode item : items) {
                System.out.println(
                    item.toString() +
                    " | previous = " +
                    item.previousSibling() +
                    " | next = " +
                    item.nextSibling()
                );
            }
        }

        // Display the footer
        footer("Siblings");
    }

    private static void testArrays() {
        // Display the header
        header("Arrays");

        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            AnalysisUnit unit = context.getUnitFromBuffer(
                "example null null example var (example null example)",
                "foo.txt"
            );
            Sequence root = (Sequence) unit.getRoot();

            // Visit the sequence
            FooNode[] items = root.pAllItems();
            System.out.println("Sequence size = " + items.length);
            System.out.println("Sequence content :");
            for(int i = 0 ; i < items.length ; i++) {
                FooNode item = items[i];
                System.out.println("  " + item.toString());
            }
            FooNode[] eqItems = root.pAllItems();
            System.out.println("Array equality = " + items.equals(eqItems));

            // Test an array of primitive type (integer)
            int[] intArray = root.pGetIntArray();
            System.out.println("Integer array size = " + intArray.length);
            System.out.println(
                "Integer array size (from property) = " +
                root.pArrayLen(intArray)
            );
            System.out.println("Integer array content :");
            for (int i = 0; i < intArray.length; i++) {
                System.out.println("  " + intArray[i]);
            }

            // Test a property error raised by an array typed property
            try {
                FooNode[] invalid = root.pArrayPropError();
                System.out.println("No exception raised by 'array_prop_error'");
            } catch (LangkitException exc) {
                System.out.println(
                    "Expected exception: " + exc.kind + " - " +
                    exc.getMessage() + (
                        exc.nativeStackTrace() != "" ?
                            " (Stack trace available)" :
                            " (NO STACK TRACE)"
                    )
                );
            }

        }

        // Display the footer
        footer("Arrays");
    }

    private static void testIterators() {
        header("Iterators");

        try (
            AnalysisContext context = AnalysisContext.create();
        ) {
            AnalysisUnit unit = context.getUnitFromBuffer(
                "my_ident",
                "foo.txt"
            );
            Sequence root = (Sequence) unit.getRoot();

            // Get and display an integer iterator
            System.out.println("--- Integer iterator");
            try (
                IntegerIterator intIterator = root.pIterInt()
            ) {
                while (intIterator.hasNext()) {
                    int i = intIterator.next();
                    System.out.println("  " + i);
                }
            }

            // Get and display an entity iterator
            System.out.println("--- Entity iterator");
            try (
                FooNodeIterator entityIterator = root.pIterEntity()
            ) {
                while (entityIterator.hasNext()) {
                    FooNode i = entityIterator.next();
                    System.out.println("  " + i);
                }
            }

            // Get and display a struct iterator (ensure handing of null values)
            System.out.println("--- Struct iterator");
            try (
                SomeStructIterator structIterator = root.pIterStruct()
            ) {
                while (structIterator.hasNext()) {
                    SomeStruct i = structIterator.next();
                    System.out.println(
                        "  SomeStruct(examples=" +
                        Arrays.toString(i.examples) +
                        ")"
                    );
                }
            }

            // Check passing an iterator as property parameter
            System.out.println("--- Iterator as property parameter");
            try (
                IntegerIterator iterator = root.pIterInt();
                IntegerIterator same = root.pIterIntId(iterator)
            ) {
                System.out.println(
                    "Identity iterator is the same = " + iterator.equals(same)
                );
            }
        }

        footer("Iterators");
    }

    private static void testSymbols() {
        // Display the header
        header("Symbols");

        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            AnalysisUnit unit = context.getUnitFromBuffer(
                "my_ident",
                "foo.txt"
            );

            Sequence root = (Sequence) unit.getRoot();
            Ident ident = (Ident) root.getChild(0);

            String[] sources = {
                "my_ident",
                "MY_IDENT",
                "no_such_symbol",
                "invalid_symbol0"
            };
            for(String source : sources) {
                try {
                    Symbol symbol = ident.pSym(Symbol.create(source));
                    System.out.println(
                        "source = " +
                        source +
                        " | symbol = " +
                        symbol.text
                    );
                } catch(SymbolException e) {
                    System.out.println(
                        "source = " +
                        source +
                        " | exception = " +
                        e.getMessage()
                    );
                }
            }
        }

        // Display the footer
        footer("Symbols");
    }

    /**
     * Test the langkit characters
     */
    private static void testCharacter() {
        // Display the header
        header("Character");

        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            AnalysisUnit unit = context.getUnitFromBuffer(
                "my_ident",
                "foo.txt"
            );
            FooNode root = unit.getRoot();

            // Test the character manipulation
            Reflection.Field getAField = root.getFieldDescription("p_get_a");
            Reflection.Field getEacuteField =
                root.getFieldDescription("p_get_eacute");
            Char c1 = root.pGetA(
                (Char) (
                    (Reflection.Param) getAField.params.get(0)
                ).defaultValue.get()
            );
            Char c2 = root.pGetEacute(
                (Char) (
                    (Reflection.Param) getEacuteField.params.get(0)
                ).defaultValue.get()
            );
            Char cIdent = root.pIdentity(Char.create('\u00e9'));
            System.out.println("The 'a' char = " + c1.toString());
            System.out.println("The eacute char = " + String.valueOf(c2.value));
            System.out.println(
                "Identity of eacute = " + String.valueOf(cIdent.value)
            );
        }

        // Display the footer
        footer("Character");
    }

    /**
     * Test the langkit strings
     */
    private static void testString() {
        // Display the header
        header("String");

        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            AnalysisUnit unit = context.getUnitFromBuffer(
                "my_ident",
                "foo.txt"
            );
            FooNode root = unit.getRoot();

            // Try creating and passing strings
            String source = "Hello!";
            String emptySource = "";
            String identity = root.pGetStr(source);
            String emptyIdentity = root.pGetStr(emptySource);
            System.out.println(
                "Source = \"" +
                source +
                "\" | Identity = \"" +
                identity +
                "\""
            );
            System.out.println(
                "Empty source = \"" +
                emptySource +
                "\" | Empty identity = \"" +
                emptyIdentity +
                "\""
            );
        }

        // Display the footer
        footer("String");
    }

    /**
     * Test the langkit big int
     */
    private static void testBigInt() {
        // Display the header
        header("Big Integer");

        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            AnalysisUnit unit = context.getUnitFromFile("foo.txt");
            FooNode root = unit.getRoot();

            // Test the big integer manipulation
            BigInteger ft = new BigInteger("42");
            BigInteger big = new BigInteger(
                "100000000000000000000000000000000000000000000000" +
                "00000000000000000000000000000000000000000000000000000"
            );
            BigInteger ftDouble = root.pIntDouble(ft);
            BigInteger bigDouble = root.pIntDouble(big);
            System.out.println(
                "Double of " +
                ft.toString() +
                " = " +
                ftDouble.toString()
            );
            System.out.println(
                "Double of 10**100 = " +
                bigDouble.toString()
            );
        }

        // Display the footer
        footer("Big Integer");
    }

    private static void testStruct() {
        // Display the header
        header("Struct");

        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            AnalysisUnit unit = context.getUnitFromFile("foo.txt");
            FooNode root = unit.getRoot();

            // Test a struct with an entity
            FooNodeStruct falseNodeStruct = root.pMe(false);
            FooNodeStruct trueNodeStruct = root.pMe(true);
            FooNode falseNode = root.pGetNode(falseNodeStruct);
            FooNode trueNode = root.pGetNode(trueNodeStruct);
            System.out.println("me false = " + falseNode);
            System.out.println("me true = " + trueNode);
            System.out.println(
                "falseNode.equals(falseNodeStruct.node) = " +
                falseNode.equals(falseNodeStruct.node)
            );

            // Test a struct with a bare node and verify that it has been
            // wrapped.
            BareNodeStruct bareNodeStruct = root.pMyNode();
            FooNode bareNode = bareNodeStruct.bareNode;
            System.out.println("root.pMyNode().bareNode = " + bareNode);

            // Test a struct with other structs and builtin type in it
            WithInner withInner = root.pNewStructWithInner();
            System.out.println("withInner.sloc = " + withInner.sloc);
            System.out.println(
                "withInner.som = "
                + Arrays.toString(withInner.som.examples)
            );

            // Test passing this structure as a property parameter
            Char aChar = bareNodeStruct.aChar;
            Char otherChar = root.pGetChar(bareNodeStruct);
            System.out.println(
                "aChar.equals(otherChar) = " + aChar.equals(otherChar)
            );
        }

        // Display the footer
        footer("Struct");
    }

    private static void testEventHandlers() {
        header("Event handlers");

        // Local function to test event handlers
        BiConsumer<
            EventHandler.UnitRequestedCallback,
            EventHandler.UnitParsedCallback
        > testHandler = (unitRequestedCallback, unitParsedCallback) -> {
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
                // Parse the unit twice to test the "reparsed" callback param
                AnalysisUnit unit = context.getUnitFromBuffer(
                    "example",
                    "example"
                );
                unit = context.getUnitFromBuffer("example example", "example");

                // Call the property to trigger "unit requested" callback
                FooNode root = unit.getRoot();
                root.pTriggerUnitRequested(
                    Symbol.create("foo_a"),
                    false,
                    false
                );
                root.pTriggerUnitRequested(
                    Symbol.create("foo_b"),
                    true,
                    false
                );
                root.pTriggerUnitRequested(
                    Symbol.create("foo_c"),
                    true,
                    true
                );
                root.pTriggerUnitRequested(
                    Symbol.create("foo_d"),
                    false,
                    true
                );
            }
        };

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
        testHandler.accept(
            unitRequestedCallback,
            unitParsedCallback
        );
        System.out.println("=== Null callbacks ===");
        testHandler.accept(null, null);

        System.out.println("=== Unclosed event handler ===");
        EventHandler dontClose = EventHandler.create(
            unitRequestedCallback,
            unitParsedCallback
        );

        footer("Event handlers");
    }

    private static class FooVisitor extends DefaultVisitor<Integer> {

        protected Function<FooNode, Integer> createDefaultBehavior() {
            return (node) -> {
                System.out.println("Visiting " + node.getImage());
                Integer count = 1;
                for (var c : node.children()) {
                    if (!c.isNone()) count += c.accept(this);
                }
                return count;
            };
        }
    }

    private static void testDefaultVisitor() {
        header("Default visitor");
        FooVisitor visitor = new FooVisitor();

        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            AnalysisUnit unit = context.getUnitFromFile("foo.txt");
            Integer count = unit.getRoot().accept(visitor);
            System.out.println("Visited " + String.valueOf(count) + " nodes");
        }

        footer("Default visitor");
    }
    /**
     * Run the Java tests one by one
     *
     * @param args The arguments for the tests running
     */
    public static void main(String[] args) {
        // Run the tests
        System.out.println("===== Start the Java tests =====\n");
        testDiagnostics();
        testUnitFileName();
        testUnitProvider();
        testTokens();
        testNodes();
        testParent();
        testSiblings();
        testArrays();
        testIterators();
        testSymbols();
        testCharacter();
        testString();
        testBigInt();
        testStruct();
        testEventHandlers();
        testDefaultVisitor();
        System.out.println("===== End of the Java tests =====");
    }

}
