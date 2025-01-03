package com.adacore.langkit_support;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

public class LangkitSupport {

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
        public abstract AnalysisContext createContext();

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
        private AnalysisContext context;
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
                AnalysisContext context,
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
        public AnalysisContext createContext() {
            return context;
        }

        @Override
        public List<String> getDiagnostics() {
            return diagnostics;
        }
    }

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

    /**
     * Represents a context for all source analysis. This is the first type
     * needed to start analyzing sources. It will contain the result of all
     * analysis, and is the main holder for all the data.
     */
    public abstract static class AnalysisContext implements AutoCloseable {

        /**
         * Get an analysis unit from the given file in the current context.
         *
         * @param fileName The file to get the analysis unit from.
         * @return The new analysis unit.
         */
        public abstract AnalysisUnit getUnitFromFile(final String fileName);

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
    }

    /** This type represents the analysis of a single file. */
    public abstract static class AnalysisUnit {

        /** The root node of the analysis unit. */
        public abstract Node getRoot();

        /** The analysis unit file name with its full path. */
        public abstract String getFileName();

        /** The analysis unit file name. */
        public abstract String getFileName(final boolean fullPath);

        /** The number of token in the analysis unit. */
        public abstract int getTokenCount();

        /** The number of trivia in the analysis unit. */
        public abstract int getTriviaCount();

        /** The text of the analysis unit in a string. */
        public abstract String getText();

        /** The analysis context that owns the unit. */
        public abstract AnalysisContext getContext();

        /** The list of associated parsing diagnostics. */
        public abstract Diagnostic[] getDiagnostics();
    }

    /** The base type shared by all Langkit nodes. */
    public interface Node {
        /** The list of all field names of the node. */
        public abstract String[] getFieldNames();

        /** Whether the node is a list node. */
        public abstract boolean isListNode();

        /** Whether the node is a token node. */
        public abstract boolean isTokenNode();

        /** Whether the node is a none node. */
        public abstract boolean isNone();

        /** The analysis unit owning the node. */
        public abstract AnalysisUnit getUnit();

        /** The list of children nodes. */
        public abstract Node[] children();

        /** The number of children. */
        public abstract int getChildrenCount();

        public abstract Node getChild(final int n);

        /** The text of the Node. */
        public abstract String getText();

        /** The image of the Node. */
        public abstract String getImage();

        /** The source location range of the node. */
        public abstract SourceLocationRange getSourceLocationRange();

        /** The parsing tree in a string. */
        public abstract String dumpTree();

        /** Dumps the parsing tree in the given string builder. */
        public abstract void dumpTree(final StringBuilder builder);
    }

    /**
     * Describes an interpretation of a reference. Can be attached to logic
     * atoms (e.g. Binds) to indicate under which interpretation this
     * particular atom was produced, which can in turn be used to produce
     * informative diagnostics for resolution failures.
     */
    public interface LogicContextInterface {
        public default Node gRefNode() {
            throw new RuntimeException(
                "Default implementation LogicContextInterface.gRefNode");
        }

        public default Node gDeclNode() {
            throw new RuntimeException(
                "Default implementation LogicContextInterface.gDeclNode");
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
            throw new RuntimeException(
                "Default implementation"
                + " SolverDiagnosticInterface.gMessageTemplate"
            );
        }

        /**
         * Return an array of nodes, which are to be plugged in the holes of
         * the template in the same order (i.e. the first argument goes into
         * the first hole of the template, etc.).
         */
        public default Node[] gArgs() {
            throw new RuntimeException(
                "Default implementation SolverDiagnosticInterface.gArgs");
        }

        /** Return the node which indicates the location of the error. */
        public default Node gLocation() {
            throw new RuntimeException(
                "Default implementation SolverDiagnosticInterface.gLocation");
        }

        /**
         * Return the array of contexts that were deemed relevant for this
         * error.
         */
        public default LogicContextInterface[] gContexts() {
            throw new RuntimeException(
                "Default implementation SolverDiagnosticInterface.gContexts");
        }
    }

    /**
     * Interface to be implemented by all nodes that will support common LSP
     * requests: run name resolution or code completion.
     */
    public interface LspNodeInterface extends Node {
        /** Return True if this node is an entry point for name resolution. */
        public default boolean gXrefEntryPoint() {
            throw new RuntimeException(
                "Default implementation LspNodeInterface.gXrefEntryPoint");
        }

        /** Return a list of completion item. */
        public default CompletionItemInterface[] gCompleteItems() {
            throw new RuntimeException(
                "Default implementation LspNodeInterface.gCompleteItems");
        }

        /**
         * Run name resolution on this node if it is an entry point and return
         * all raised diagnostics, if any.
         */
        public default SolverDiagnosticInterface[] gNameresDiagnostics() {
            throw new RuntimeException(
                "Default implementation LspNodeInterface.gNameresDiagnostics");
        }
    }

    /**
     * Interface representing identifiers that define an entity in the
     * analyzed source code.
     */
    public interface DefiningNameInterface extends Node {
        /** Return the full name defined by this DefiningName. */
        public default String gFullName() {
            throw new RuntimeException(
                "Default implementation DefiningNameInterface.gFullName");
        }

        /**
         * Return a string containing the detail of this defining name to
         * display when hovering or in completion items (generally the type or
         * prototype).
         */
        public default String gDeclDetail() {
            throw new RuntimeException(
                "Default implementation DefiningNameInterface.gDeclDetail");
        }

        /**
         * Return an integer correspoding to the LSP's CompletionItemKind
         * type.
         */
        public default int gCompletionItemKind() {
            throw new RuntimeException(
                "Default implementation"
                + " DefiningNameInterface.gCompletionItemKind"
            );
        }

        /** Return the documentation associated to this defining name. */
        public default String gDocumentation() {
            throw new RuntimeException(
                "Default implementation DefiningNameInterface.gDocumentation");
        }

        /** Return the type associated to this defining name. */
        public default TypeInterface gGetType() {
            throw new RuntimeException(
                "Default implementation DefiningNameInterface.gGetType");
        }

        /**
         * Return an array of RefResults containing all references in the given
         * AnalysisUnits.
         */
        public default RefResultInterface[] gFindAllReferences(
                final AnalysisUnit[] units) {
            throw new RuntimeException(
                "Default implementation"
                + " DefiningNameInterface.gFindAllReferences");
        }

        /**
         * Return an array of DefiningName containing all implementations of
         * the DefiningName in the given units.
         */
        public default DefiningNameInterface[] gFindImplementations(
                final AnalysisUnit[] units) {
            throw new RuntimeException(
                "Default implementation"
                + " DefiningNameInterface.gFindImplementations");
        }
    }

    /** Interface representing types in the analyzed source code. */
    public interface TypeInterface extends Node {
        /** Return the full name of the type. */
        public default String gFullName() {
            throw new RuntimeException(
                "Default implementation TypeInterface.gFullName");
        }

        /** Return the DefiningName of the type associated to this type. */
        public default DefiningNameInterface gDefiningName() {
            throw new RuntimeException(
                "Default implementation TypeInterface.gDefiningName");
        }
    }

    /**
     * Interface representing nodes that can have a type in the analyzed source
     * code.
     */
    public interface TypableNodeInterface extends Node {
        /** Return the type of the associated to this node. */
        public default TypeInterface gExprType() {
            throw new RuntimeException(
                "Default implementation TypableNodeInterface.gExprType");
        }
    }

    /** Interface representing nodes that reference a definition. */
    public interface ReferenceInterface extends Node {
        /** Return the DefiningName referenced by this object. */
        public default DefiningNameInterface gReferencedDefiningName() {
            throw new RuntimeException(
                "Default implementation"
                + " ReferenceInterface.gReferencedDefiningName");
        }
    }

    /**
     * Interface representing a declaration containing at least one defined
     * name.
     */
    public interface DeclarationInterface extends Node {
        /** Return the list of names defined by this definition. */
        public default DefiningNameInterface[] gDefiningNames() {
            throw new RuntimeException(
                "Default implementation DeclarationInterface.gDefiningNames");
        }
    }

    /**
     * Interface representing a struct that contains completion information.
     */
    public interface CompletionItemInterface {
        /** Return the declaration for this completion item. */
        public default DeclarationInterface gDecl() {
            throw new RuntimeException(
                "Default implementation CompletionItemInterface.gDecl");
        }
    }

    /**
     * Interface representing a struct that contains referencing information.
     */
    public interface RefResultInterface {
        /** Return the reference of this RefResult. */
        public default ReferenceInterface gRef() {
            throw new RuntimeException(
                "Default implementation RefResultInterface.gRef");
        }
    }
}
