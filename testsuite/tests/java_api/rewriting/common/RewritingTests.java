import com.adacore.libfoolang.Libfoolang.*;

import java.util.Arrays;

public class RewritingTests {

    // ----- Utils -----

    /** Functional interface representing a simple block of code. */
    @FunctionalInterface
    public interface Executable {
        void execute();
    }

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
     * Util function for assertions
     */
    private static void assertTrue(
        String what,
        boolean predicate
    ) throws RuntimeException {
        System.out.println("Asserting: " + what);
        if (!predicate) {
            throw new RuntimeException("Assertion \"" + what + "\" failed");
        }
    }

    private static void assertLangkitException(
        String what,
        Executable executable
    ) {
        try {
            System.out.println("Try: " + what);
            executable.execute();
            throw new RuntimeException("Assertion \"" + what + "\" failed");
        } catch (LangkitException e) {
            System.out.println("  Exception kind = " + e.kind);
            System.out.println("  Exception message = " + e.getMessage());
        }
    }

    // ----- Test methods -----

    /** Test the rewriting context creation and properties */
    private static void testRewritingContext() {
        header("Rewriting context");
        try (
            AnalysisContext context = AnalysisContext.create();
        ) {
            assertTrue(
                "Rewriting context reference is null",
                context.getRewritingContext() == null
            );

            // Open a context and close it manually
            System.out.println("Creating rewriting context");
            RewritingContext rcontext1 = context.startRewriting();
            assertTrue("Rewriting context reference to analysis context",
                       rcontext1.getAnalysisContext() == context);
            assertLangkitException("Creating an other rewriting context",
                                   () -> context.startRewriting());
            assertTrue("Rewriting context is opened", !rcontext1.isClosed());

            System.out.println("Closing the rewriting context");
            rcontext1.close();
            assertTrue("Rewriting context is closed", rcontext1.isClosed());

            // Open a new context and apply it empty
            System.out.println("\nCreating an other rewriting context");
            RewritingContext rcontext2 = context.startRewriting();
            System.out.println("Applying the new rewriting context");
            RewritingApplyResult applyResult = rcontext2.apply();
            assertTrue("Rewriting result is successful", applyResult.success);
            assertTrue("Rewriting result has no diagnostics",
                       applyResult.getDiagnostics().length == 0);
            assertTrue("Rewriting result unit is none",
                       applyResult.unit == AnalysisUnit.NONE);
            assertTrue("Other rewriting context is closed",
                       rcontext2.isClosed());

            System.out.println("Free the apply result");
            applyResult.close();
        }
        footer("Rewriting context");
    }

    /** Test the rewriting unit creation and properties */
    private static void testRewritingUnit() {
        header("Rewriting unit");
        try (
            AnalysisContext context1 = AnalysisContext.create();
            AnalysisContext context2 = AnalysisContext.create();
            RewritingContext rcontext = context1.startRewriting();
        ) {
            // Create valid rewriting units
            System.out.println("Creating analysis and rewriting units");
            AnalysisUnit unit1 = context1.getUnitFromFile("s1.txt");
            AnalysisUnit unit2 = context1.getUnitFromFile("s2.txt");
            AnalysisUnit unit3 = context1.getUnitFromFile("s3.txt");
            AnalysisUnit nonRewritableUnit =
                context2.getUnitFromFile("s1.txt");
            RewritingUnit runit1 = unit1.getRewritingUnit();
            RewritingUnit runit2 = unit2.getRewritingUnit();
            assertTrue("Analysis unit reference to rewriting unit",
                       unit1.getRewritingUnit().equals(runit1));
            assertTrue("Rewriting unit reference to analysis unit",
                       runit2.getAnalysisUnit().equals(unit2));
            System.out.println("s2.txt rewriting unit unparsing:");
            System.out.println("#####");
            System.out.println(runit2.unparse());
            System.out.println("#####");
            assertTrue("Fetched rewriting units count is 2",
                       rcontext.rewritingUnits().length == 2);
            assertLangkitException(
                "Creating a rewriting unit from a non-rewritable context",
                () -> nonRewritableUnit.getRewritingUnit()
            );
        }
        footer("Rewriting unit");
    }

    /** Test the rewriting node fetching and properties */
    private static void testRewritingNode() {
        header("Rewriting node");
        try (
            AnalysisContext context1 = AnalysisContext.create();
            AnalysisContext context2 = AnalysisContext.create();
            RewritingContext rcontext = context1.startRewriting();
        ) {
            // Create rewriting units
            System.out.println("Creating analysis and rewriting units");
            AnalysisUnit unit1 = context1.getUnitFromFile("s1.txt");
            AnalysisUnit unit2 = context2.getUnitFromFile("s2.txt");
            RewritingUnit runit1 = unit1.getRewritingUnit();

            // Get the roots and verify them
            System.out.println("\nGetting rewriting units root nodes");
            FooNode node1 = unit1.getRoot();
            FooNode node2 = unit2.getRoot();
            RewritingNode rnode1 = runit1.getRoot();
            assertTrue("Rewriting root is not None", !rnode1.isNone());
            assertTrue("Rewriting node parsed node reference",
                       node1.equals(rnode1.getParsedNode()));
            assertTrue("Parsed node rewriting reference",
                       rnode1.equals(node1.getRewritingNode()));
            assertTrue("Rewriting root kind is the same as root",
                       rnode1.getKind() == node1.getKind());
            assertTrue("Rewriting node context reference",
                       rcontext.equals(rnode1.getRewritingContext()));
            assertTrue("Rewriting root is tied", rnode1.isTied());
            assertLangkitException(
                "Create a rewriting node from a non rewriting context",
                () -> node2.getRewritingNode()
            );
            System.out.println("s1.txt rewriting root node image");
            System.out.println(rnode1.image());
            System.out.println("s1.txt rewriting root node unparsing");
            System.out.println("#####");
            System.out.println(rnode1.unparse());
            System.out.println("#####");
            assertTrue("Unit and root unparsing results are the same",
                       runit1.unparse().equals(rnode1.unparse()));

            // Root children visiting
            System.out.println("\nGetting the rewriting root children");
            RewritingNode[] children = rnode1.children();
            System.out.println(
                "Root rewriting node children: " + Arrays.toString(children)
            );
            assertTrue("Children count is 3", children.length == 3);
            assertTrue("Child parent is the root",
                       children[0].parent().equals(rnode1));

            // Child navigation
            System.out.println("\nNavigating the root node children");
            RewritingNode rfirstChild = rnode1.firstChild();
            RewritingNode rlastChild = rnode1.lastChild();
            System.out.println("Root first child: " + rfirstChild);
            System.out.println("Root last child: " + rlastChild);
            assertTrue("Last child previous is the first child next",
                       rlastChild.previousChild()
                                 .equals(rfirstChild.nextChild()));
            assertTrue("Last child next is None",
                       rlastChild.nextChild().isNone());
            assertTrue("First child previous is None",
                       rfirstChild.previousChild().isNone());

            // First declaration visiting
            System.out.println(
                "\nGetting the name and expression of the first declaration"
            );
            RewritingNode rdecl1 = children[0];
            RewritingNode rname1 = rdecl1.getChild(
                MemberReference.FOO_DECL_F_NAME
            );
            RewritingNode rexpr1 = rdecl1.getChild(
                MemberReference.FOO_DECL_F_EXPR
            );
            System.out.println("  name: " + rname1.toString());
            System.out.println("  expr: " + rexpr1.toString());
            assertLangkitException(
                "Getting f_name child on the root list",
                () -> rnode1.getChild(MemberReference.FOO_DECL_F_NAME)
            );
            assertLangkitException(
                "Getting f_expr child on a name node",
                () -> rname1.getChild(MemberReference.FOO_DECL_F_EXPR)
            );
            assertTrue("First child of the name is None",
                       rname1.firstChild().isNone());
            assertTrue("Last child of the expr is None",
                       rexpr1.lastChild().isNone());
        }
        footer("Rewriting node");
    }

    /** Text the rewriting node text manipulation methods */
    private static void testRewritingNodeTexts() {
        header("Rewriting node text getting/setting");
        try (
            AnalysisContext context = AnalysisContext.create();
            RewritingContext rcontext = context.startRewriting();
        ) {
            // Create rewriting units
            System.out.println("Creating analysis and rewriting units");
            AnalysisUnit unit = context.getUnitFromFile("s1.txt");
            RewritingUnit runit = unit.getRewritingUnit();

            // Get the first decl stmt and get/set the name text
            System.out.println("\nGetting the first declaration statement");
            RewritingNode rroot = runit.getRoot();
            RewritingNode rdecl1 = rroot.children()[0];
            System.out.println("Original declaration statement: '" +
                               rdecl1.unparse().trim() +
                               "'");
            RewritingNode rname1 = rdecl1.children()[0];
            assertTrue("Variable name is 'orig_1'",
                       rname1.getText().equals("orig_1"));
            System.out.println("Changing the variable name for 'hello'");
            rname1.setText("hello");
            assertTrue("Variable name has been changed",
                       rname1.getText().equals("hello"));
            System.out.println("New declaration statement: '" +
                               rdecl1.unparse().trim() +
                               "'");
            System.out.println("New rewriting unit:");
            System.out.println("#####");
            System.out.println(runit.unparse());
            System.out.println("#####");
        }
        footer("Rewriting node text getting/setting");
    }

    /** Test the rewriting node creation methods */
    private static void testRewritingNodeCreation() {
        header("Rewriting node creation");
        try (
            AnalysisContext context = AnalysisContext.create();
            RewritingContext rcontext = context.startRewriting();
        ) {
            // Create rewriting units
            System.out.println("Creating analysis and rewriting units");
            AnalysisUnit unit = context.getUnitFromFile("s1.txt");
            RewritingUnit runit = unit.getRewritingUnit();
            RewritingNode rroot = runit.getRoot();

            // Clone the first declaration statement
            System.out.println("\nCloning the first declaration statement");
            RewritingNode rdecl1 = rroot.firstChild();
            RewritingNode rdecl1Clone = rdecl1.clone();
            System.out.println("Source node: " + rdecl1);
            System.out.println("Cloned node: " + rdecl1Clone);
            assertTrue("Clone result is not tied", !rdecl1Clone.isTied());
            assertTrue("Clone is different from the source",
                       !rdecl1Clone.equals(rdecl1));

            // Create a declaration from scratch
            System.out.println(
                "\nCreating a declaration statement from scratch"
            );
            RewritingNode rdeclCreated1 = rcontext.createNode(NodeKind.VAR);
            System.out.println("Created node: " + rdeclCreated1);
            assertTrue("Created node is not tied", !rdeclCreated1.isTied());
            assertTrue("Created node parsed node is None",
                       rdeclCreated1.getParsedNode().isNone());
            assertTrue("Created declaration name is None",
                       rdeclCreated1.getChild(MemberReference.FOO_DECL_F_NAME)
                                    .isNone());

            // Create a name and an integer literal from scratch
            System.out.println("\nCreating a name and an integer literal");
            RewritingNode rname1 =
                rcontext.createTokenNode(NodeKind.NAME, "first_name");
            RewritingNode rlit1 =
                rcontext.createTokenNode(NodeKind.LITERAL, "1");
            assertTrue("Create name is not tied", !rname1.isTied());
            assertTrue("Created name kind is NAME",
                       rname1.getKind() == NodeKind.NAME);
            assertTrue("Create name text is 'first_name'",
                       rname1.getText().equals("first_name"));
            assertLangkitException(
                "Creating a token node with an non-token kind ",
                () -> rcontext.createTokenNode(NodeKind.VAR, "impossible")
            );

            // Create another declaration directly with the name and value
            System.out.println(
                "\nCreating a declaration statement with its children"
            );
            RewritingNode rdeclCreated2 =
                rcontext.createNode(NodeKind.VAR, rname1, rlit1);
            System.out.println("Created node: " + rdeclCreated2);
            assertTrue("Created node is not tied", !rdeclCreated2.isTied());
            assertTrue("Created decl name is the previously created name",
                       rdeclCreated2.getChild(MemberReference.FOO_DECL_F_NAME)
                                    .equals(rname1));
            assertTrue(
                "Created decl expression is the previously created one",
                rdeclCreated2.getChild(MemberReference.FOO_DECL_F_EXPR)
                             .equals(rlit1)
            );
            assertLangkitException(
                "Creating a node with already tied children",
                () -> rcontext.createNode(NodeKind.VAR, rname1, rlit1)
            );
            RewritingNode rname2 =
                rcontext.createTokenNode(NodeKind.NAME, "second_name");
            RewritingNode rlit2 =
                rcontext.createTokenNode(NodeKind.LITERAL, "2");
            assertLangkitException(
                "Creating a node with the invalid type",
                () -> rcontext.createNode(NodeKind.NAME, rname2, rlit2)
            );

            // Create a declaration from a template
            System.out.println(
                "\nCreating a declaration statement from a template"
            );
            RewritingNode rdeclCreated3 = rcontext.createFromTemplate(
                "var {} = {}",
                GrammarRule.VAR_RULE_RULE,
                rname2,
                rlit2
            );
            System.out.println("Created node: " + rdeclCreated3);
            assertTrue("Created node is a VAR",
                       rdeclCreated3.getKind() == NodeKind.VAR);
            assertTrue("Created node is not tied", !rdeclCreated3.isTied());
            assertTrue(
                "Created declaration name text is 'second_name'",
                rdeclCreated3.getChild(MemberReference.FOO_DECL_F_NAME)
                             .getText()
                             .equals("second_name")
            );
            assertTrue(
                "Created declaration expression text is '2'",
                rdeclCreated3.getChild(MemberReference.FOO_DECL_F_EXPR)
                             .getText()
                             .equals("2")
            );
            assertLangkitException(
                "Creating from template with invalid argument count",
                () -> rcontext.createFromTemplate(
                    "var {} = {}",
                    GrammarRule.VAR_RULE_RULE,
                    rname2
                )
            );

            // Create the final declaration list with its children
            System.out.println("\nCreating the declaration list");
            RewritingNode rdeclList = rcontext.createNode(
                NodeKind.DECL_LIST,
                rdeclCreated2,
                rdeclCreated3
            );
            System.out.println("Created declaration list: " + rdeclList);
            RewritingNode[] children = rdeclList.children();
            assertTrue("Created list child count is 2", children.length == 2);
            System.out.println("Create declaration list unparse result:");
            System.out.println("#####");
            System.out.println(rdeclList.unparse());
            System.out.println("#####");
        }
        footer("Rewriting node creation");
    }

    /**
     * Test the rewriting node modification operations (replace, add, remove)
     */
    private static void testRewritingNodeModifications() {
        header("Rewriting node modifications");

        try (
            AnalysisContext context = AnalysisContext.create();
            RewritingContext rcontext = context.startRewriting();
        ) {
            // Create rewriting units
            System.out.println("Creating analysis and rewriting units");
            AnalysisUnit unit = context.getUnitFromFile("s1.txt");
            RewritingUnit runit = unit.getRewritingUnit();
            RewritingNode rroot = runit.getRoot();
            RewritingNode[] children = rroot.children();

            // Set the first declaration name
            System.out.println("\nReplacing the first declaration name");
            RewritingNode rdecl1 = children[0];
            RewritingNode rname1 =
                rdecl1.getChild(MemberReference.FOO_DECL_F_NAME);
            RewritingNode rnameCreated1 =
                rcontext.createTokenNode(NodeKind.NAME, "replaced");
            assertTrue("First declaration name is 'orig_1'",
                       rname1.getText().equals("orig_1"));
            assertTrue("First declaration name is tied", rname1.isTied());
            rdecl1.setChild(MemberReference.FOO_DECL_F_NAME, rnameCreated1);
            RewritingNode rname2 =
                rdecl1.getChild(MemberReference.FOO_DECL_F_NAME);
            assertTrue("Current name node is the created one",
                       rname2.equals(rnameCreated1));
            assertTrue("First declaration name is now 'replaced'",
                       rname2.getText().equals("replaced"));
            assertTrue("Created name is now tied", rnameCreated1.isTied());
            assertTrue("Old name node is not tied anymore", !rname1.isTied());
            assertLangkitException(
                "Setting an already tied child",
                () -> rdecl1.setChild(
                    MemberReference.FOO_DECL_F_NAME,
                    rnameCreated1
                )
            );

            // Replace the first declaration name for the original one
            System.out.println(
                "\nReplacing the first delaration name for the original one"
            );
            rname2.replace(rname1);
            assertTrue("Replacing name is now untied", !rname2.isTied());
            assertTrue("Original name is now tied", rname1.isTied());
            assertTrue(
                "First declaration name is now 'orig_1'",
                rdecl1.getChild(MemberReference.FOO_DECL_F_NAME)
                      .getText()
                      .equals("orig_1")
            );
            assertLangkitException(
                "Replacing the name with an already tied node",
                () -> rname1.replace(rname1)
            );
            assertLangkitException(
                "Replacing an untied node",
                () -> rname2.replace(rname2)
            );

            // Insert new declarations in the declaration list
            System.out.println("\nDeclaration list original unparse:");
            System.out.println("#####");
            System.out.println(rroot.unparse());
            System.out.println("#####");

            RewritingNode rdeclCreated1 = rcontext.createFromTemplate(
                "var c1 = 1",
                GrammarRule.VAR_RULE_RULE
            );
            RewritingNode rdeclCreated2 = rcontext.createFromTemplate(
                "var c2 = 2",
                GrammarRule.VAR_RULE_RULE
            );
            RewritingNode rdeclCreated3 = rcontext.createFromTemplate(
                "var c3 = 3",
                GrammarRule.VAR_RULE_RULE
            );
            RewritingNode rdeclCreated4 = rcontext.createFromTemplate(
                "var c4 = 4",
                GrammarRule.VAR_RULE_RULE
            );

            // Insert a new declaration at the beginning
            System.out.println("\nInserting a declaration at the beginning");
            rroot.insertFirst(rdeclCreated1);
            System.out.println("Declaration list unparse:");
            System.out.println("#####");
            System.out.println(rroot.unparse());
            System.out.println("#####");
            assertTrue("Inserted node is now tied", rdeclCreated1.isTied());
            assertTrue("First declaration is the inserted one",
                       rroot.firstChild().equals(rdeclCreated1));
            assertTrue("Children count is now 4",
                       rroot.children().length == 4);
            assertLangkitException(
                "Inserting a first child in a non list node",
                () -> rdeclCreated1.insertFirst(rname2)
            );
            assertLangkitException(
                "Inserting an already tied child node",
                () -> rroot.insertFirst(rdeclCreated1)
            );

            // Insert a new declaration at the end
            System.out.println("\nInserting a declartion at the end");
            rroot.insertLast(rdeclCreated2);
            System.out.println("Declaration list unparse:");
            System.out.println("#####");
            System.out.println(rroot.unparse());
            System.out.println("#####");
            assertTrue("Inserted node is now tied", rdeclCreated2.isTied());
            assertTrue("Last declaration is the inserted one",
                       rroot.lastChild().equals(rdeclCreated2));
            assertTrue("Children count is now 5",
                       rroot.children().length == 5);
            assertLangkitException(
                "Inserting a last child in a non list node",
                () -> rdeclCreated2.insertLast(rname2)
            );
            assertLangkitException(
                "Inserting an already tied child node",
                () -> rroot.insertLast(rdeclCreated2)
            );

            // Insert a node before the 3rd node
            System.out.println("\nInserting a declaration before the 3rd");
            RewritingNode rthird = rroot.children()[2];
            RewritingNode rthridName =
                rthird.getChild(MemberReference.FOO_DECL_F_NAME);
            rthird.insertBefore(rdeclCreated3);
            System.out.println("Declaration list unparse:");
            System.out.println("#####");
            System.out.println(rroot.unparse());
            System.out.println("#####");
            assertTrue("Inserted node is now tied", rdeclCreated3.isTied());
            assertTrue("3rd declaration is the inserted one",
                       rroot.children()[2].equals(rdeclCreated3));
            assertTrue("Children count is now 6",
                       rroot.children().length == 6);
            assertLangkitException(
                "Inserting a node before a non-list-parent node",
                () -> rthridName.insertBefore(rname2)
            );
            assertLangkitException(
                "Inserting an already tied node",
                () -> rthird.insertBefore(rdeclCreated3)
            );

            // Insert a node after the 4th one
            System.out.println("\nInserting a declaration after the 4th");
            rthird.insertAfter(rdeclCreated4);
            System.out.println("Declaration list unparse:");
            System.out.println("#####");
            System.out.println(rroot.unparse());
            System.out.println("#####");
            assertTrue("Inserted node is now tied", rdeclCreated4.isTied());
            assertTrue("5th declaration is the inserted one",
                       rroot.children()[4].equals(rdeclCreated4));
            assertTrue("Children count is now 7",
                       rroot.children().length == 7);
            assertLangkitException(
                "Inserting a node before a non-list-parent node",
                () -> rthridName.insertAfter(rname2)
            );
            assertLangkitException(
                "Inserting an already tied node",
                () -> rthird.insertAfter(rdeclCreated4)
            );

            // Remove the 2nd child from the declaration list
            System.out.println("\nRemoving the 2nd child");
            rroot.children()[1].removeFromParent();
            System.out.println("Declaration list unparse:");
            System.out.println("#####");
            System.out.println(rroot.unparse());
            System.out.println("#####");
            assertTrue("Children count is now 6",
                       rroot.children().length == 6);
            assertLangkitException(
                "Removing a node which is not in a list node",
                () -> rname1.removeFromParent()
            );
            assertLangkitException(
                "Removing a node which is not tied",
                () -> rname2.removeFromParent()
            );
        }

        footer("Rewriting node modifications");
    }

    /** Run all Java rewriting tests */
    public static void main(String[] args) {
        System.out.println("===== Start the Java rewriting tests =====");
        testRewritingContext();
        testRewritingUnit();
        testRewritingNode();
        testRewritingNodeTexts();
        testRewritingNodeCreation();
        testRewritingNodeModifications();
        System.out.println("===== End of the Java rewriting tests =====");
    }
}
