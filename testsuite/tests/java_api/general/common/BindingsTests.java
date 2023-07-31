import java.io.File;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

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
        List<Diagnostic> diags;

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

            // Test the token equivalence
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
            System.out.println("Unit root kind = " + root.getKindName());
            System.out.println(
                "Unit root children count = " + root.getChildrenCount()
            );
            System.out.println(
                "Unit root children = " + root.children().toString()
            );
            System.out.println(
                "Unit root AST dump = " + root.dumpAST()
            );
            System.out.println(
                "Unit root is a list node = " + root.isListType()
            );
            System.out.println(
                "Unit root is a token node = " + root.isTokenNode()
            );
            System.out.println("Unit root image = " + root.getImage());
            System.out.println("Unit root text = " + root.getText());
            Sequence sequence = (Sequence) root;
            FooNodeArray items = sequence.pAllItems();
            System.out.println("Root \"p_all_items\" = " + items.toString());
            Var var = (Var) root.getChild(2);
            System.out.println("Var (3rd child) = " + var.toString());
            System.out.println("Var image = " + var.getImage());
            System.out.println(
                "Var fields = " + Arrays.toString(var.getFieldNames())
            );
            Sequence arg = var.fArg();
            FooNodeArray argItems = arg.pAllItems();
            System.out.println("Var arg = " + arg.toString());
            System.out.println("Var content = " + argItems.toString());
            Var var2 = (Var) root.getChild(2);
            System.out.println("Node equality = " + var.equals(var2));
            System.out.println(
                "Node hash equality = " + (var.hashCode() == var2.hashCode())
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
            FooNodeArray items = root.pAllItems();

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
            FooNodeArray items = root.pAllItems();
            System.out.println("Sequence size = " + items.size());
            System.out.println("Sequence content :");
            for(int i = 0 ; i < items.size() ; i++) {
                FooNode item = items.get(i);
                System.out.println("  " + item.toString());
            }
            FooNodeArray eqItems = root.pAllItems();
            System.out.println("Array equality = " + items.equals(eqItems));
        }

        // Display the footer
        footer("Arrays");
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
            LibfoolangField getAField = root.getFieldDescription("p_get_a");
            LibfoolangField getEacuteField =
                root.getFieldDescription("p_get_eacute");
            Char c1 = root.pGetA(
                (Char) (
                    (ParamWithDefaultValue) getAField.params.get(0)
                ).defaultValue
            );
            Char c2 = root.pGetEacute(
                (Char) (
                    (ParamWithDefaultValue) getEacuteField.params.get(0)
                ).defaultValue
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

            FooNodeStruct falseNodeStruct = root.pMe(false);
            FooNodeStruct trueNodeStruct = root.pMe(true);
            FooNode falseNode = root.pGetNode(falseNodeStruct);
            FooNode trueNode = root.pGetNode(trueNodeStruct);
            System.out.println("me false = " + falseNode);
            System.out.println("me true = " + trueNode);
        }

        // Display the footer
        footer("Struct");
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
        testSymbols();
        testCharacter();
        testString();
        testBigInt();
        testStruct();
        System.out.println("===== End of the Java tests =====");
    }

}
