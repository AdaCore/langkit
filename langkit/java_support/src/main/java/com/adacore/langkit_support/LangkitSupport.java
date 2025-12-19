package com.adacore.langkit_support;

import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import com.oracle.truffle.api.CompilerDirectives;


public class LangkitSupport {

    /** Exception to raise when interface default methods are not overrode. */
    private static final
    class NotImplementedException extends RuntimeException {
        public NotImplementedException () {};

        @Override
        public String getMessage(){
            return "Missing default method override";
        }
    }

    /** Exception to represent errors during project manipulation. */
    public static final
    class ProjectManagerException extends RuntimeException {
        public ProjectManagerException(final String message) {
            super(message);
        }
    }

    /**
     * Abstract class representing a project manager that fetches project files
     * and creates analysis contexts.
     */
    public abstract static class ProjectManager {
        /**
         * Returns all files in project.
         *
         * <p>
         * Requests in language servers such as ``textDocument/references``
         * require to have a list of all units used in the project. Thus, it is
         * recommended to list all sources on creation of the ProjectManager.
         * By doing so, we are able to provide all the units in which to look
         * for references instead of expecting the user to open all files
         * manually, or by looking up the list of units only when the user
         * requests it, which may delay the request.
         */
        public abstract String[] getFiles();

        /** Returns a new AnalysisContext. */
        public abstract AnalysisContextInterface createContext();

        /** Returns diagnostics raised during project loading. */
        public abstract List<String> getDiagnostics();
    }

    /**
     * Dummy project manager used to fetch files if no project manager was
     * provided by the Langkit library.
     *
     * <p>
     * The capacities provided by this class are limited: it is expected to
     * implement a language specific {@link ProjectManager} in order to cover
     * all desired cases.
     */
    public static class DummyProjectManager extends ProjectManager {

        private String[] files;
        private AnalysisContextInterface context;
        private List<String> diagnostics;

        /**
         * Searches all sources in the opened project.
         */
        private void searchFiles(List<String> extensions) {
            try (Stream<Path> paths =
                    Files.walk( Paths.get(System.getProperty("user.dir")))) {
                // We traverse recursively the current directory. To avoid this
                // taking too long, if there are too many files, just ignore
                // everthing and wait for the client to open files to read
                // them.
                int count = 0;
                ArrayList<String> files = new ArrayList<>();
                var pathIterator = paths.iterator();
                while (pathIterator.hasNext() && count < 200) {
                    Path p = pathIterator.next();
                    count++;
                    String file = p.toString();
                    if (extensions.stream().anyMatch(file::endsWith) &&
                            Files.isRegularFile(p)) {
                        files.add(file);
                    }
                }
                this.files = files.toArray(new String[0]);
            } catch (IOException e) {
                throw new ProjectManagerException(e.getMessage());
            }
        }

        public DummyProjectManager(
                AnalysisContextInterface context,
                List<String> extensions) {
            // TODO: Emit a warning when the DummyProjectManager is used
            this.context = context;
            searchFiles(extensions);
        }

        @Override
        public String[] getFiles() {
            return files;
        }

        @Override
        public AnalysisContextInterface createContext() {
            return context;
        }

        @Override
        public List<String> getDiagnostics() {
            return diagnostics;
        }
    }

    /** Classes that implement this interface wrap the langkit characters
     *  which are 32 bit wide. */
    public interface CharInterface {}

    /**
     * Reference to a symbol. Symbols are owned by analysis contexts, so they
     * must not outlive them. This type exists only in the C API, and roughly
     * wraps the corresponding Ada type (an array fat pointer).
     */
    public interface SymbolInterface {}

    public static class SourceLocationRange {

        /** The start of the range. */
        public final SourceLocation start;

        /** The end of the range. */
        public final SourceLocation end;

        public SourceLocationRange(
                final SourceLocation start,
                final SourceLocation end) {
            this.start = start;
            this.end = end;
        }

        public final SourceLocation getStart() {
            return start;
        }

        public final SourceLocation getEnd() {
            return end;
        }
    }

    public static class SourceLocation implements Comparable<SourceLocation> {

        /** The line of the source location. */
        public final int line;

        /** The column of the source location. */
        public final short column;

        public SourceLocation(final int line, final short column) {
            this.line = line;
            this.column = column;
        }

        @Override
        public int compareTo(SourceLocation other) {
            if (this.line == other.line)
                return this.column - other.column;
            return this.line - other.line;
        }
    }

    public abstract static class Text implements AutoCloseable {

        /** The size of the text. */
        public abstract long getLength();

        /** If the text is allocated. */
        public abstract boolean getIsAllocated();

        /** If the text object is the owner of its buffer. */
        public abstract boolean getIsOwner();

        /** The content of the text in a Java string. */
        public abstract String getContent();
    }

    /**
     * Diagnostic for an analysis unit: cannot open the source file, parsing
     * error, ...
     */
    public abstract static class Diagnostic {

        /** The location of the error. */
        public abstract SourceLocationRange getSourceLocationRange();

        /** The message of the error. */
        public abstract Text getMessage();
    }

    /** Default method for rendering solver diagnostics. */
    public static String renderSolverDiag(SolverDiagnosticInterface diag) {
        String message = diag.gMessageTemplate();
        for (var arg : diag.gArgs()) {
            String str;
            if (arg instanceof LangkitSupport.TypeInterface decl)
                str = decl.gFullName();
            else if (arg instanceof LangkitSupport.TypeInterface decl)
                str = decl.gFullName();
            else if (arg instanceof LangkitSupport.DeclarationInterface decl)
                str = decl.gDefiningNames()[0].gFullName();
            else
                str = arg.getText();
            message = message.replaceFirst("\\{\\}", str);
        }
        return message;
    }

    public interface NodeKindInterface {}

    public interface TokenKindInterface {
        /**
         * Get the C value from the enum instance.
         *
         * @return The int C value of the enum instance.
         */
        public abstract int toC();
    }

    /**
     * Reference to a token in an analysis unit.
     */
    public interface TokenInterface {
        /** The source location range of the token. */
        public abstract SourceLocationRange getSourceLocationRange();

        /** The analysis unit of the token */
        public abstract AnalysisUnit getUnit();

        /** The text of the token. */
        public abstract String getText();

        /** The kind of the token. */
        public abstract TokenKindInterface getKind();

        /** Return whether the token is a trivia. */
        public abstract boolean isTrivia();

        /** Return whether the token is a none token. */
        public abstract boolean isNone();

        /** The next token. */
        public abstract TokenInterface next();

        /** The previous token. */
        public abstract TokenInterface previous();

        /** Check if the token is equivalent to the other one. */
        @CompilerDirectives.TruffleBoundary
        public default boolean isEquivalent(TokenInterface other) {
             throw new NotImplementedException();
        }
    }

    /**
     * Result of applying a rewriting session.
     *
     * On success, ``Success`` is true.
     *
     * On failure, ``Success`` is false, ``Unit`` is set to the unit on which
     * rewriting failed, and ``Diagnostics`` is set to related rewriting
     * errors.
     */
    public abstract static class RewritingApplyResult implements AutoCloseable {
        /** Whether the rewriting application was successful. */
        public final boolean success;

        public RewritingApplyResult(final boolean success) {
            this.success = success;
        }

        /** Return all diagnostics. */
        public abstract Diagnostic[] getDiagnostics();
    }

    /** Handle for an analysis context rewriting session. */
    public interface RewritingContextInterface extends AutoCloseable {
        /** Return whether the rewriting context is closed. */
        public abstract boolean isClosed();

        /**
         * Apply all modifications to Handle's analysis context. If that
         * worked, close Handle and return (Success => True). Otherwise,
         * reparsing did not work, so keep Handle and its Context unchanged and
         * return details about the error that happened.
         *
         * Note that on success, this invalidates all related unit/node
         * handles.
         */
        public abstract RewritingApplyResult apply();

        /**
         * Discard all modifications registered in Handle and close Handle.
         * This invalidates all related unit/node handles.
         */
        public abstract void close();

        /**
         * Create a new regular node of the given Kind and assign it the given
         * Children.
         *
         * Except for lists, which can have any number of children, the size of
         * Children must match the number of children associated to the given
         * Kind. Besides, all given children must not be tied.
         */
        @CompilerDirectives.TruffleBoundary
        public default RewritingNodeInterface createNode(
            final NodeKindInterface kind,
            final RewritingNodeInterface... children)
        {
            throw new NotImplementedException();
        }

        /** Create a new token node with the given Kind and Text. */
        @CompilerDirectives.TruffleBoundary
        public default RewritingNodeInterface createTokenNode(
            final NodeKindInterface kind,
            final String text
        ) {
            throw new NotImplementedException();
        }

        /**
         * Create a tree of new nodes from the given Template string, replacing
         * placeholders with nodes in Arguments and parsed according to the
         * given grammar Rule.
         */
        @CompilerDirectives.TruffleBoundary
        public default RewritingNodeInterface createFromTemplate(
            final String templage,
            final String rule,
            final RewritingNodeInterface... arguments
        ) {
            throw new NotImplementedException();
        }
    }

    /** Interface to override how source files are fetched and decoded. */
    public interface FileReaderInterface extends AutoCloseable {}

    /**
     * Interface to fetch analysis units from a name and a unit kind.
     *
     * The unit provider mechanism provides an abstraction which assumes that
     * to any couple (unit name, unit kind) we can associate at most one source
     * file. This means that several couples can be associated to the same
     * source file, but on the other hand, only one one source file can be
     * associated to a couple.
     *
     * This is used to make the semantic analysis able to switch from one
     * analysis units to another.
     *
     * See the documentation of each unit provider for the exact semantics of
     * the unit name/kind information.
     */
    public interface UnitProviderInterface extends AutoCloseable {}

    /** Interface to handle events sent by the analysis context. */
    public interface EventHandlerInterface extends AutoCloseable {}

    /**
     * Represents a context for all source analysis. This is the first type
     * needed to start analyzing sources. It will contain the result of all
     * analysis, and is the main holder for all the data.
     */
    public interface AnalysisContextInterface extends AutoCloseable {

        /**
         * Get an analysis unit from the given file in the current context.
         *
         * @param fileName The file to get the analysis unit from.
         * @return The new analysis unit.
         */
        public abstract AnalysisUnit getUnitFromFile(String fileName);

        /**
         * Get an analysis unit from the given file in the current context with
         * additional parameters.
         *
         * @param fileName The file to get the analysis unit from.
         * @param charset  The charset of the given file.
         * @return The new analysis unit.
         */
        public abstract AnalysisUnit getUnitFromFile(
                final String fileName,
                final String charset);

        /**
         * Get an analysis unit from the given file in the current context with
         * additional parameters.
         *
         * @param fileName The file to get the analysis unit from.
         * @param charset  The charset of the given file.
         * @param reparse  If the file should be reparsed.
         * @return The new analysis unit.
         */
        public abstract AnalysisUnit getUnitFromFile(
                final String fileName,
                final String charset,
                final boolean reparse);

        /**
         * Get a new analysis unit with the given file name and content in the
         * current context
         *
         * @param buffer The content of the file.
         * @param name   The file name associated to the analysis unit.
         * @return The new analysis unit.
         */
        public abstract AnalysisUnit getUnitFromBuffer(
                final String buffer,
                final String name);

        /**
         * Start a rewriting session for Context.
         *
         * This handle will keep track of all changes to do on Context's
         * analysis units. Once the set of changes is complete, call the Apply
         * procedure to actually update Context. This makes it possible to
         * inspect the "old" Context state while creating the list of changes.
         *
         * There can be only one rewriting session per analysis context, so
         * this will raise an Existing_Rewriting_Handle_Error exception if
         * Context already has a living rewriting session.
         */
        @CompilerDirectives.TruffleBoundary
        public default RewritingContextInterface startRewriting() {
            throw new NotImplementedException();
        }

        /**
         * Create an analysis context with its parameters.
         *
         * @param charset The charset for the analysis context, it can be null.
         * @param fileReader The file reader for the analysis context,
         *                   it can be null.
         * @param unitProvider The unit provider for the analysis context,
         *                    it can be null.
         * @param eventHandler The event handler for the analysis context,
         *                    it can be null.
         * @param withTrivia If the analysis context should include trivias.
         * @param tabStop The effect of the tabulations on the column number.
         * @return The newly create analysis unit.
         */
        @CompilerDirectives.TruffleBoundary
        public default AnalysisContextInterface create(
            final String charset,
            final FileReaderInterface fileReader,
            final UnitProviderInterface unitProvider,
            final EventHandlerInterface eventHandler,
            final boolean withTrivia,
            final int tabStop
        ) {
            throw new NotImplementedException();
        }

        /** Create a new analysis context with the default parameters. */
        @CompilerDirectives.TruffleBoundary
        public static AnalysisContextInterface create() {
            throw new NotImplementedException();
        }

        /** @see java.lang.AutoCloseable#close() */
        public abstract void close();

        /**
         * Assign in ``Context`` configuration pragmas files to analysis units
         * as described in ``Global_Pragmas`` (configuration pragmas file that
         * applies to all analysis units, or null) and ``Local_Pragmas``
         * (mapping that associates an analysis unit to the local configuration
         * pragmas file that applies to it).
         *
         * This raises a ``Precondition_Failure`` exception if any analysis
         * unit in ``Mapping`` does not belong to ``Context`` or if an analysis
         * unit appears twice as a key in ``Mapping``.
         */
        @CompilerDirectives.TruffleBoundary
        public default void setConfigPragmasMapping(
            AnalysisUnit globalPragmas,
            Map<AnalysisUnit, AnalysisUnit> localPragmas
        ) {
            throw new NotImplementedException();
        }
    }

    /** This type represents the analysis of a single file. */
    public abstract static class AnalysisUnit {

        /** The root node of the analysis unit. */
        public abstract NodeInterface getRoot();

        /** The analysis unit file name with its full path. */
        public abstract String getFileName();

        /** The analysis unit file name. */
        public abstract String getFileName(boolean fullPath);

        /** The number of token in the analysis unit. */
        public abstract int getTokenCount();

        /** The number of trivia in the analysis unit. */
        public abstract int getTriviaCount();

        /** The first token of the analysis unit. */
        public abstract TokenInterface getFirstToken();

        /** The last token of the analysis unit. */
        public abstract TokenInterface getLastToken();

        /** The text of the analysis unit in a string. */
        public abstract String getText();

        /** The analysis context that owns the unit. */
        public abstract AnalysisContextInterface getContext();

        /** The list of associated parsing diagnostics. */
        public abstract Diagnostic[] getDiagnostics();
    }

    /** This type represents the Reflection utils nodes. */
    public abstract static class Reflection {
        /** This class represents the description of a node. */
        public abstract static class Node {}

        /** This class represents the description of a node field. */
        public abstract static class Field {
            /** The parameters of the method */
            public abstract List<? extends Reflection.Param> getParams();
            /** The Java method for the field */
            public abstract Method getJavaMethod();
        }

        /** This class represents a parameter description. */
        public abstract static class Param {
            /** Get the name of the parameter. */
            public abstract String getName();
            /** The type of the parameter */
            public abstract Class<?> getType();
            /** The optional default value of the parameter */
            public abstract Optional<Object> getDefaultValue();
        }
    }

    /** This type represents a member reference. */
    public interface MemberReferenceInterface {}

    /** This type represents the rewriting node. */
    public interface RewritingNodeInterface {
        /** Return the None value for this type. */
        @CompilerDirectives.TruffleBoundary
        public static RewritingNodeInterface getNONE() {
            throw new NotImplementedException();
        }

        /** Return a copy of this node. */
        public abstract RewritingNodeInterface clone();

        /** Return whether this rewriting node is tied. */
        public abstract boolean isTied();

        /** Insert a node before this node. */
        @CompilerDirectives.TruffleBoundary
        public default void insertBefore(
            final RewritingNodeInterface toInsert)
        {
            throw new NotImplementedException();
        }

        /** Replace this node by a new node. */
        @CompilerDirectives.TruffleBoundary
        public default void replace(RewritingNodeInterface newNode) {
            throw new NotImplementedException();
        }

        /** Insert a node after this node. */
        @CompilerDirectives.TruffleBoundary
        public default void insertAfter(RewritingNodeInterface toInsert) {
            throw new NotImplementedException();
        }

        /** Insert a node first amongst this node siblings. */
        @CompilerDirectives.TruffleBoundary
        public default void insertFirst(RewritingNodeInterface toInsert) {
            throw new NotImplementedException();
        }

        /** Insert a node last amongst this node siblings. */
        @CompilerDirectives.TruffleBoundary
        public default void insertLast(RewritingNodeInterface toInsert) {
            throw new NotImplementedException();
        }

        /** Remove this node from its parent. */
        public abstract void removeFromParent();

        /** Set the child of this node. */
        @CompilerDirectives.TruffleBoundary
        public default void setChild(
            MemberReferenceInterface childMember,
            RewritingNodeInterface child
        ) {
            throw new NotImplementedException();
        }
    }

    /** The base type shared by all Langkit nodes. */
    public interface NodeInterface {
        /** The description of the node. */
        public abstract Reflection.Node getDescription();

        /** The list of all field names of the node. */
        public abstract String[] getFieldNames();

        /**
         * The map containing the list of all field names and their description.
         */
        public abstract Map<String, ? extends Reflection.Field>
            getFieldDescriptions();

        /** Get the description of a field by its name. */
        public abstract Reflection.Field getFieldDescription(String name);

        /** Whether the node is a list node. */
        public abstract boolean isListNode();

        /** Whether the node is a token node. */
        public abstract boolean isTokenNode();

        /** The token starting this node. */
        public abstract TokenInterface tokenStart();

        /** The token ending this node. */
        public abstract TokenInterface tokenEnd();

        /** Whether the node is a none node. */
        public abstract boolean isNone();

        /** The analysis unit owning the node. */
        public abstract AnalysisUnit getUnit();

        /** The list of children nodes. */
        public abstract NodeInterface[] children();

        /** The number of children. */
        public abstract int getChildrenCount();

        /** The nth child of the node. */
        public abstract NodeInterface getChild(int n);

        /** The text of the node. */
        public abstract String getText();

        /** The image of the node. */
        public abstract String getImage();

        /** The source location range of the node. */
        public abstract SourceLocationRange getSourceLocationRange();

        /** The rewriting node of the node. */
        @CompilerDirectives.TruffleBoundary
        public default RewritingNodeInterface getRewritingNode() {
            throw new NotImplementedException();
        }

        /** The parsing tree in a string. */
        public abstract String dumpTree();

        /** Dumps the parsing tree in the given string builder. */
        public abstract void dumpTree(StringBuilder builder);

        /** The parent of the node. */
        public abstract NodeInterface parent();
    }

    /**
     * Describes an interpretation of a reference. Can be attached to logic
     * atoms (e.g. Binds) to indicate under which interpretation this
     * particular atom was produced, which can in turn be used to produce
     * informative diagnostics for resolution failures.
     */
    public interface LogicContextInterface {
        public default NodeInterface gRefNode() {
            throw new NotImplementedException();
        }

        public default NodeInterface gDeclNode() {
            throw new NotImplementedException();
        }
    }

    /**
     * A raw diagnostic produced by a solver resolution failure.
     * This contains as much information as possible to allow formatters
     * down the chain to filter/choose which diagnostics to show among
     * a set of diagnostics produced for a single equation.
     */
    public interface SolverDiagnosticInterface {
        /**
         * Return the string explaining the error, which may contain holes
         * represented by the ``{}`` characters.
         */
        public default String gMessageTemplate() {
            throw new NotImplementedException();
        }

        /**
         * Return an array of nodes, which are to be plugged in the holes of
         * the template in the same order (i.e. the first argument goes into
         * the first hole of the template, etc.).
         */
        public default NodeInterface[] gArgs() {
            throw new NotImplementedException();
        }

        /** Return the node which indicates the location of the error. */
        public default NodeInterface gLocation() {
            throw new NotImplementedException();
        }

        /**
         * Return the array of contexts that were deemed relevant for this
         * error.
         */
        public default LogicContextInterface[] gContexts() {
            throw new NotImplementedException();
        }
    }

    /**
     * Interface to be implemented by all nodes that will support common LSP
     * requests: run name resolution or code completion.
     */
    public interface LspNodeInterface extends NodeInterface {
        /** Return True if this node is an entry point for name resolution. */
        public default boolean gXrefEntryPoint() {
            throw new NotImplementedException();
        }

        /** Return a list of completion item. */
        public default CompletionItemInterface[] gCompleteItems() {
            throw new NotImplementedException();
        }

        /**
         * Run name resolution on this node if it is an entry point and return
         * all raised diagnostics, if any.
         */
        public default SolverDiagnosticInterface[] gNameresDiagnostics() {
            throw new NotImplementedException();
        }
    }

    /**
     * Interface representing identifiers that define an entity in the
     * analyzed source code.
     */
    public interface DefiningNameInterface extends NodeInterface {
        /** Return the full name defined by this DefiningName. */
        public default String gFullName() {
            throw new NotImplementedException();
        }

        /**
         * Return a string containing the detail of this defining name to
         * display when hovering or in completion items (generally the type or
         * prototype).
         */
        public default String gDeclDetail() {
            throw new NotImplementedException();
        }

        /**
         * Return an integer correspoding to the LSP's CompletionItemKind
         * type.
         */
        public default int gCompletionItemKind() {
            throw new NotImplementedException();
        }

        /** Return the documentation associated to this defining name. */
        public default String gDocumentation() {
            throw new NotImplementedException();
        }

        /** Return the type associated to this defining name. */
        public default TypeInterface gGetType() {
            throw new NotImplementedException();
        }

        /**
         * Return an array of RefResults containing all references in the given
         * AnalysisUnits.
         */
        public default RefResultInterface[] gFindAllReferences(
                final AnalysisUnit[] units) {
            throw new NotImplementedException();
        }

        /**
         * Return an array of DefiningName containing all implementations of
         * the DefiningName in the given units.
         */
        public default DefiningNameInterface[] gFindImplementations(
                final AnalysisUnit[] units) {
            throw new NotImplementedException();
        }
    }

    /** Interface representing types in the analyzed source code. */
    public interface TypeInterface extends NodeInterface {
        /** Return the full name of the type. */
        public default String gFullName() {
            throw new NotImplementedException();
        }

        /** Return the DefiningName of the type associated to this type. */
        public default DefiningNameInterface gDefiningName() {
            throw new NotImplementedException();
        }
    }

    /**
     * Interface representing nodes that can have a type in the analyzed source
     * code.
     */
    public interface TypableNodeInterface extends NodeInterface {
        /** Return the type of the associated to this node. */
        public default TypeInterface gExprType() {
            throw new NotImplementedException();
        }
    }

    /** Interface representing nodes that reference a definition. */
    public interface ReferenceInterface extends NodeInterface {
        /** Return the DefiningName referenced by this object. */
        public default DefiningNameInterface gReferencedDefiningName() {
            throw new NotImplementedException();
        }
    }

    /**
     * Interface representing a declaration containing at least one defined
     * name.
     */
    public interface DeclarationInterface extends NodeInterface {
        /** Return the list of names defined by this definition. */
        public default DefiningNameInterface[] gDefiningNames() {
            throw new NotImplementedException();
        }
    }

    /**
     * Interface representing a struct that contains completion information.
     */
    public interface CompletionItemInterface {
        /** Return the declaration for this completion item. */
        public default DeclarationInterface gDecl() {
            throw new NotImplementedException();
        }
    }

    /**
     * Interface representing a struct that contains referencing information.
     */
    public interface RefResultInterface {
        /** Return the reference of this RefResult. */
        public default ReferenceInterface gRef() {
            throw new NotImplementedException();
        }
    }
}
