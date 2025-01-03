## vim: ft=makojava

<%namespace name="jni_lib" file="jni_lib.mako" />
<%namespace name="ni_lib" file="ni_lib.mako" />
<%namespace name="enum" file="enum.mako" />
<%namespace name="ast_node" file="ast_node.mako" />
<%namespace name="struct" file="struct.mako" />
<%namespace name="array" file="array.mako" />
<%namespace name="iterator" file="iterator.mako" />
<%namespace name="exts" file="/extensions.mako" />
<%
api = java_api
nat = c_api.get_name

root_node_type = api.wrapping_type(T.root_node)
%>

package com.adacore.${ctx.lib_name.lower};
import com.adacore.langkit_support.LangkitSupport;

import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

import java.lang.StringBuilder;
import java.lang.Iterable;
import java.lang.reflect.Method;

import java.math.BigInteger;

import java.io.File;
import java.nio.ByteOrder;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.strings.TruffleString;

import org.graalvm.nativeimage.CurrentIsolate;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.nativeimage.IsolateThread;
import org.graalvm.nativeimage.StackValue;
import org.graalvm.nativeimage.UnmanagedMemory;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CEntryPoint;
import org.graalvm.nativeimage.c.function.CEntryPointLiteral;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.function.CFunctionPointer;
import org.graalvm.nativeimage.c.function.InvokeCFunctionPointer;
import org.graalvm.nativeimage.c.struct.*;
import org.graalvm.nativeimage.c.type.*;
import org.graalvm.word.PointerBase;
import org.graalvm.word.Pointer;
import org.graalvm.word.UnsignedWord;
import org.graalvm.word.WordFactory;


/*
====================

This is the Java bindings of the ${ctx.lib_name.lower} API.
You can use all functionalities of the library in a Java environment.
Those bindings call the native library using JNI and Native-Image C API.

====================
*/
public final class ${ctx.lib_name.camel} {

    // ==========
    // Native entry points
    // ==========

    /**
     * This method is the only valid callback to pass to a native event
     * handler for unit requests events.
     * This method will dispatch the execution according to the passed
     * analysis context.
     */
    @CEntryPoint
    static void unitRequested(
        final IsolateThread thread,
        final AnalysisContextNative contextNative,
        final TextNative nameNative,
        final AnalysisUnitNative fromNative,
        final byte foundNative,
        final byte isNotFoundErrorNative
    ) {
        try(
            final AnalysisContext context = AnalysisContext.wrap(
                contextNative
            );
            final Text text = Text.wrap(nameNative)
        ) {
            // Get the callback from the context event handler
            final EventHandler.UnitRequestedCallback callback = context
                .getEventHandler()
                .getUnitRequestCallback();

            // Call the callback
            if(callback != null) {
                callback.invoke(
                    context,
                    text.getContent(),
                    AnalysisUnit.wrap(fromNative),
                    foundNative != 0,
                    isNotFoundErrorNative != 0
                );
            }
        }
    }

    /**
     * This method is the only valid callback to pass to a native event
     * handler for unit parsing events.
     * This method will dispatch the execution according to the passed
     * analysis context.
     */
    @CEntryPoint
    static void unitParsed(
        final IsolateThread thread,
        final AnalysisContextNative contextNative,
        final AnalysisUnitNative unitNative,
        final byte reparsedNative
    ) {
        try(
            final AnalysisContext context = AnalysisContext.wrap(
                contextNative
            )
        ) {
            // Get the callback from the context event handler
            final EventHandler.UnitParsedCallback callback = context
                .getEventHandler()
                .getUnitParsedCallback();

            // Call the callback
            if(callback != null) {
                callback.invoke(
                    context,
                    AnalysisUnit.wrap(unitNative),
                    reparsedNative != 0
                );
            }
        }
    }

    // ==========
    // Static values
    // ==========

    /** The default grammar rule to parse the inputs. */
    private static final GrammarRule DEFAULT_GRAMMAR_RULE =
        GrammarRule.${ctx.main_rule_api_name.upper};

    /** The os name in lower case. */
    private static final String OS =
            System.getProperty("os.name").toLowerCase();

    /** The byte order of the system. */
    private static final ByteOrder BYTE_ORDER = ByteOrder.nativeOrder();

    /** The node to convert a Java string to a truffle string */
    private static final TruffleString.FromJavaStringNode fromJavaStringNode =
        TruffleString.FromJavaStringNode.create();

    /** The node to convert a truffle string to a Java string. */
    private static final TruffleString.ToJavaStringNode toJavaStringNode =
        TruffleString.ToJavaStringNode.create();

    /** The node to convert a byte array to a truffle string. */
    private static final TruffleString.FromByteArrayNode fromByteArrayNode =
        TruffleString.FromByteArrayNode.create();

    /** The node to convert a truffle string to a byte array. */
    private static final TruffleString.CopyToByteArrayNode toByteArrayNode =
        TruffleString.CopyToByteArrayNode.create();

    /** A map to store node descriptions associated to their camel name. */
    public static final Map<String, Reflection.Node>
        NODE_DESCRIPTION_MAP = new HashMap<>();

    static {
        // Fill the node description map and set the node kind descriptions
        % for astnode in ctx.astnode_types:
        NODE_DESCRIPTION_MAP.put(
            "${astnode.kwless_raw_name.camel}",
            ${api.wrapping_type(astnode)}.description
        );
            % if not astnode.abstract:
        NodeKind.${astnode.kwless_raw_name.upper}.setDescription(
            ${api.wrapping_type(astnode)}.description
        );
            % endif
        % endfor
    }

    // ==========
    // Util functions
    // ==========

    /**
     * Get the string representing the memory.
     *
     * @param pointer The pointer to start displaying the memory from.
     * @param count The number of bytes to display from the pointer.
     * @return The string representing the memory as hex bytes.
     */
    private static String dumpMemory(
        final Pointer pointer,
        final long count
    ) {
        final StringBuilder res = new StringBuilder();
        for(int i = 0 ; i < count ; i++) {
            final byte toDump = pointer.readByte(i);
            res.append(String.format("%02x ", toDump));
        }
        return res.toString();
    }

    /**
     * Convert a Java string to a C string by allocating memory.
     *
     * @param jString The Java string to convert.
     * @return The native C char pointer. This pointer MUST be freed.
     */
    @CompilerDirectives.TruffleBoundary
    private static CCharPointer toCString(
        final String jString
    ) {
        final UnsignedWord size = WordFactory.unsigned(jString.length() + 1);
        final CCharPointer res = UnmanagedMemory.calloc(size);
        if(jString.length() > 0) {
            CTypeConversion.toCString(
                jString,
                StandardCharsets.UTF_8,
                res,
                size
            );
        }

        return res;
    }

    /**
     * Convert a Java byte array in a native byte array.
     *
     * @param bytes The Java bytes.
     * @return The native C char pointer. This pointer MUST be freed.
     */
    private static CCharPointer toCBytes(
        final byte[] bytes
    ) {
        final UnsignedWord size = WordFactory.unsigned(bytes.length);
        final CCharPointer res = UnmanagedMemory.malloc(size);
        for(int i = 0 ; i < bytes.length ; i++) {
            res.write(i, bytes[i]);
        }
        return res;
    }

    /**
     * Convert a native-image C string to a Java string.
     *
     * @param pointer The char pointer to convert to a Java string.
     * @return The Java string.
     */
    @CompilerDirectives.TruffleBoundary
    private static String toJString(
        final CCharPointer pointer
    ) {
        if (pointer.isNull()) {
            return null;
        }
        return CTypeConversion.toJavaString(pointer);
    }

    /**
     * This function decode a utf 32 int array in a
     * string without calling Java charset.
     *
     * @param chars The int array to decode.
     * @return The resulting string.
     */
    @CompilerDirectives.TruffleBoundary
    private static String decodeUTF32(
        final byte[] toDecode
    ) {
        return toJavaStringNode.execute(
            fromByteArrayNode.execute(toDecode, TruffleString.Encoding.UTF_32)
        );
    }

    /**
     * This function encode a given string to a int array
     * according to the utf 32 standard.
     *
     * @param toEncode The string to encode.
     * @return The encoded string in an int array.
     */
    @CompilerDirectives.TruffleBoundary
    private static byte[] encodeUTF32(
        final String toEncode
    ) {
        return toByteArrayNode.execute(
            fromJavaStringNode.execute(
                toEncode,
                TruffleString.Encoding.UTF_32
            ),
            TruffleString.Encoding.UTF_32
        );
    }

    /**
     * Get the string representation of the given string.
     * This function escaped needed chars and format the string.
     *
     * @param source The source string to get the representation for.
     * @return The representation of the string.
     */
    private static String stringRepresentation(
        final String source
    ) {
        return source
            .replace("\"", "\\\"")
            .replace("\n", "\\x0a");
    }

    /**
     * Convert a C Langkit exception to the LangkitException class.
     */
    private static LangkitException wrapException(
        final LangkitExceptionNative exc
    ) {
        return new LangkitException(
            exc.get_kind(),
            toJString(exc.get_information()),
            toJString(exc.get_stack_trace())
        );
    }

    /**
      * Return the exception raised by the last C API call, or null if the last
      * call was successful.
      */
    private static LangkitException getLastException() {
        LangkitException result = null;

        if(ImageInfo.inImageCode()) {
            final LangkitExceptionNative exceptionNative =
                NI_LIB.${nat("get_last_exception")}();
            if(exceptionNative.isNonNull()) {
                result = wrapException(exceptionNative);
            }
        } else {
            result = JNI_LIB.${nat("get_last_exception")}();
        }
        return result;
    }

    /**
     * Check the last exception raised by langkit and throw it.
     *
     * @throws The last langkit exception if there is one.
     */
    @CompilerDirectives.TruffleBoundary
    private static void checkException() throws LangkitException {
        LangkitException exc = getLastException();
        if(exc != null)
            throw exc;
    }

    % if ctx.generate_unparsers:
    /**
     * Create a native array from the given rewriting nodes array.
     * The returned pointer must be freed using UnmanagedMemory#free methods.
     */
    private static WordPointer rewritingNodesToNative(
        final RewritingNode[] rewritingNodes
    ) {
        final WordPointer res = UnmanagedMemory.malloc(
            rewritingNodes.length * SizeOf.get(WordPointer.class)
        );
        for(int i = 0; i < rewritingNodes.length; i++) {
            res.write(i, rewritingNodes[i].unwrap());
        }
        return res;
    }
    % endif

    // ==========
    // Util interfaces
    // ==========

    /**
     * Interface to visit the parse tree.
     */
    public static interface BasicVisitor<T> {
        T visit(${root_node_type} node);
        % for astnode in ctx.astnode_types:
            % if astnode != T.root_node and not astnode.abstract:
        T visit(${api.wrapping_type(astnode)} node);
            % endif
        % endfor
    }

    /**
     * Abstract class to visit the parse tree with default behavior.
     */
    public static abstract class DefaultVisitor<T> implements BasicVisitor<T> {

        /**
         * The default behavior to use when a ``visit`` method was not
         * overriden by the parent class.
         */
        protected final
        Function<${root_node_type}, T> defaultBehavior;

        /**
         * Create the default behavior function and return it.
         *
         * This method is called by the ``DefaultVisitor`` constructor. You do
         * not need to call it yourself. This method can be overriden to change
         * the default behavior. The first argument is the node that is
         * currently being visited.
         */
        protected Function<${root_node_type}, T> createDefaultBehavior() {
            return (node) -> null;
        }

        /**
         * Construct a DefaultVisitor with a default behavior that does
         * nothing.
         */
        public DefaultVisitor () {
            this.defaultBehavior = createDefaultBehavior();
        }

        @Override
        public T visit(${root_node_type} node) {
            return defaultBehavior.apply(node);
        }
        % for astnode in ctx.astnode_types:
            % if astnode != T.root_node and not astnode.abstract:
        @Override
        public T visit(${api.wrapping_type(astnode)} node) {
            return defaultBehavior.apply(node);
        }
            % endif
        % endfor
    }

    /**
     * Interface to visit the parse tree with a parameter.
     */
    public static interface ParamVisitor<T, P> {
        T visit(${root_node_type} node, P param);
        % for astnode in ctx.astnode_types:
            % if astnode != T.root_node and not astnode.abstract:
        T visit(${api.wrapping_type(astnode)} node, P param);
            % endif
        % endfor
    }

    // ==========
    // Util classes
    // ==========

    /**
     * This class represents a pointer and can hold NI and JNI addresses.
     */
    public static final class PointerWrapper {

        // ----- Instance attributes -----

        /** The pointer NI value. */
        private PointerBase ni;

        /** The pointer JNI value. */
        private final long jni;

        // ----- Constructors -----

        /**
         * Create a new custom pointer from a NI pointer based value.
         *
         * @param niPointer The pointer based value.
         */
        PointerWrapper(
            final PointerBase niPointer
        ) {
            this.ni = niPointer;
            this.jni = -1;
        }

        /**
         * Create a new custom pointer from a long value.
         *
         * @param jniPointer The pointer in a long value.
         */
        PointerWrapper(
            final long jniPointer
        ) {
            this.jni = jniPointer;
        }

        /**
         * Wrap the given NI pointer in the Java class.
         *
         * @param niPointer The NI pointer to wrap.
         * @return The wrapped pointer.
         */
        static PointerWrapper wrap(
            final PointerBase niPointer
        ) {
            return new PointerWrapper(niPointer);
        }

        /**
         * Get the null pointer according to the execution mode.
         *
         * @return The null custom pointer.
         */
        public static PointerWrapper nullPointer() {

            if(ImageInfo.inImageCode()) {
                return new PointerWrapper(WordFactory.nullPointer());
            } else {
                return new PointerWrapper(0L);
            }

        }

        // ----- Instance methods -----

        /**
         * Get the pointer as an NI pointer based value.
         *
         * @return The pointer based value for NI.
         */
        public <T extends PointerBase> T ni() {
            return (T) this.ni;
        }

        /**
         * Get the pointer as a long Java value.
         *
         * @return The pointer as a long value for JNI.
         */
        public long jni() {
            return this.jni;
        }

        /**
         * Get if the pointer is null.
         *
         * @return True if the pointer is null, false else.
         */
        public boolean isNull() {

            if(ImageInfo.inImageCode()) {
                return this.ni.isNull();
            } else {
                return this.jni == 0;
            }

        }

        // ----- Override methods -----

        @Override
        public String toString() {

            if(ImageInfo.inImageCode()) {
                return "PointerWrapper{"
                    + this.ni.rawValue()
                    + "}";
            } else {
                return "PointerWrapper{"
                    + this.jni
                    + "}";
            }

        }

        @Override
        public boolean equals(Object o) {
            if(o == this) return true;
            if(!(o instanceof PointerWrapper)) return false;
            final PointerWrapper other = (PointerWrapper) o;
            if(ImageInfo.inImageCode()) {
                return this.ni.equal(other.ni);
            } else {
                return this.jni == other.jni;
            }
        }

        @Override
        public int hashCode() {

            if(ImageInfo.inImageCode()) {
                return (int) this.ni.rawValue();
            } else {
                return (int) this.jni;
            }

        }

    }

    // ==========
    // Reflection utils
    // ==========

    public static final class Reflection {
        /**
        * This class represents the description of a node.
        */
        public static final class Node {

            // ----- Instance attributes -----

            /** Kind of the node. This kind is null if the node is abstract */
            public final NodeKind kind;

            /** Whether the node is a token node */
            public final boolean isTokenNode;

            /** Whether the node is a list node */
            public final boolean isListNode;

            /** Java class of the node */
            public final Class<? extends ${root_node_type}> clazz;

            /** Simple name of the Java class of the node */
            public final String className;

            /** Fields of the node, sorted by parsing order */
            public final String[] fields;

            /** Map containing description for all fields of the node */
            public final Map<String, Field> fieldDescriptions;

            // ----- Constructors -----

            /** Create a new node description with its kind and class */
            public Node (
                NodeKind kind,
                final boolean isTokenNode,
                final boolean isListNode,
                final Class<? extends ${root_node_type}> clazz,
                final String className,
                final String[] fields,
                final Map<String, Field> fieldDescriptions
            ) {
                this.kind = kind;
                this.isTokenNode = isTokenNode;
                this.isListNode = isListNode;
                this.clazz = clazz;
                this.className = className;
                this.fieldDescriptions = fieldDescriptions;
                this.fields = fields;
            }

        }

        /**
        * This class represents the description of a node field.
        */
        public static final class Field {

            // ----- Instance attributes -----

            /** The Java method for the field */
            public final Method javaMethod;

            /** The parameters of the method */
            public final List<Param> params;

            /** The generic member reference of this field */
            public final MemberReference memberRef;

            // ----- Constructors -----

            /**
            * Create a new field description.
            *
            * @param method The Java method to access the field.
            * @param params The parameters of the field call.
            */
            public Field(
                final Method javaMethod,
                final List<Param> params,
                final MemberReference memberRef
            ) {
                this.javaMethod = javaMethod;
                this.params = params;
                this.memberRef = memberRef;
            }

        }

        /**
        * This class represents a parameter description.
        */
        public static class Param {

            // ----- Instance attributes -----

            /** The type of the argument */
            public final Class<?> type;

            /** The name of the parameter */
            public final String name;

            /** The optional default value of the parameter */
            public final Optional<Object> defaultValue;

            // ----- Constructors -----

            /**
            * Create a new langkit parameter.
            *
            * @param type The type of the parameter.
            * @param name The name of the parameter.
            */
            public Param(
                final Class<?> type,
                final String name
            ) {
                this.type = type;
                this.name = name;
                this.defaultValue = Optional.empty();
            }

            /** Create a new parameter description with a default value. */
            public Param(
                final Class<?> type,
                final String name,
                final Object defaultValue
            ) {
                this.type = type;
                this.name = name;
                this.defaultValue = Optional.ofNullable(defaultValue);
            }

        }
    }

    // ==========
    // Language specific extensions
    // ==========

    ${exts.include_extension(ctx.ext("java_api", "main_class"))}

    // ==========
    // Defining the JNI bindings library
    // ==========

    ${jni_lib.generate()}

    // ==========
    // Defining the Native-Image bindings library
    // ==========

    ${ni_lib.generate()}

    // ==========
    // Exceptions
    // ==========

    /**
     * This class represents exception during symbol manipulation.
     */
    public static final class SymbolException extends RuntimeException {
        public SymbolException(
            final String symbol
        ) {
            super("Invalid symbol : '" + symbol + "'");
        }
    }

    /**
     * This class reprsents exception during enum manipulation.
     */
    public static final class EnumException extends RuntimeException {
        public EnumException(
            final String msg
        ) {
            super(msg);
        }
    }

    /**
     * This class represents an exception in the references manipulation.
     */
    public static final class ReferenceException extends RuntimeException {
        public ReferenceException(
            final String msg
        ) {
            super(msg);
        }
    }

    /**
     * This class wraps the exceptions from the langkit native library.
     */
    public static class LangkitException extends RuntimeException {

        // ----- Instance attributes -----

        /** The kind of the langkit exception. */
        public final ExceptionKind kind;

        /** Native Ada stack trace, if some. */
        public final Optional<String> adaStackTrace;

        // ----- Constructors -----

        /**
         * Create a new langkit exception.
         *
         * @param kind The kind of the exception represented by an integer
         *   which will be mapped to an enum value.
         * @param message The message of the exception.
         * @param adaStackTrace The string representing the Ada stack trace
         *   associated with the exception.
         */
        public LangkitException(
            final int kind,
            final String message,
            final String adaStackTrace
        ) {
            super(message);
            this.kind = ExceptionKind.fromC(kind);
            this.adaStackTrace = Optional.ofNullable(adaStackTrace);
        }

    }

    // ==========
    // Enum definitions
    // ==========

    // ===== Constants enumeration =====

    ${java_doc('langkit.token_kind', 4)}
    public enum TokenKind {

        // ----- Enum values -----

        NO_TOKEN(-1, "No_Token"),
        % for i, t in enumerate(ctx.lexer.sorted_tokens):
        ${t.c_name}(${t.value}, "${t.name}"),
        % endfor
        ;

        // ----- Class attributes -----

        /** Singleton that represents the none token kind. */
        public static final TokenKind NONE = NO_TOKEN;

        /** The map from int to enum values. */
        private static final Map<Integer, TokenKind> map = new HashMap<>();

        // ----- Instance attributes -----

        /** The value of the enum instance. */
        private final int value;

        /** The name of the enum instance in the Langkit DSL. */
        public final String name;

        // ----- Constructors -----

        static {
            // Initialise the lookup map
            for(TokenKind elem : TokenKind.values()) {
                map.put(elem.value, elem);
            }
        }

        /** Private constructor. */
        private TokenKind(
            final int value,
            final String name
        ) {
            this.value = value;
            this.name = name;
        }

        // ----- Graal C API methods -----

        /**
         * Internal method to wrap a Native Image C pointer to a token kind
         * value.
         */
        static TokenKind fromC(
            final CIntPointer pointer
        ) throws EnumException {
            return fromC(pointer.read());
        }

        // ----- Enum methods -----

        /**
         * Get the enum instance for the given C value.
         *
         * @param cValue The C int value to get the enum instance from.
         * @return The enum instance which correspond to the int value.
         * @throws EnumException When the int value doesn't map to any enum
         * instance.
         */
        public static TokenKind fromC(
            final int cValue
        ) throws EnumException {
            if(!map.containsKey(cValue))
                throw new EnumException(
                    "Cannot get TokenKind from " + cValue
                );
            return (TokenKind) map.get(cValue);
        }

        /**
         * Get the C value from the enum instance.
         *
         * @return The int C value of the enum instance.
         */
        public int toC() {
            return this.value;
        }

    }

    ${java_doc('langkit.exception_kind_type', 4)}
    public enum ExceptionKind {

        // ----- Enum values -----

        % for i in range(len(ctx.sorted_exception_types)):
        ${ctx.sorted_exception_types[i].name.upper}(${i}),
        % endfor
        ;

        // ----- Class attributes -----

        /** Singleton that represents the none expcetion kind. */
        public static final ExceptionKind NONE =
            ${ctx.sorted_exception_types[0].name.upper};

        /** The map from int to enum values. */
        private static final Map<Integer, ExceptionKind> map =
            new HashMap<>();

        // ----- Instance ttributes -----

        /** The value of the enum instance. */
        private final int value;

        // ----- Constructors -----

        static {
            // Initialise the lookup map
            for(ExceptionKind elem : ExceptionKind.values()) {
                map.put(elem.value, elem);
            }
        }

        /** Private constructor. */
        private ExceptionKind(
            final int value
        ) {
            this.value = value;
        }

        // ----- Graal C API methods -----

        /**
         * Internal method to wrap a Native Image C pointer to a exception
         * kind value.
         */
        static ExceptionKind fromC(
            final CIntPointer pointer
        ) throws EnumException {
            return fromC(pointer.read());
        }

        // ----- Enum methods -----

        /**
         * Get the enum instance for the given C value.
         *
         * @param cValue The C int value to get the enum instance from.
         * @return The enum instance which correspond to the int value.
         * @throws EnumException When the int value doesn't map to any enum
         * instance.
         */
        public static ExceptionKind fromC(
            final int cValue
        ) throws EnumException {
            if(!map.containsKey(cValue))
                throw new EnumException(
                    "Cannot get ExceptionKind from " + cValue
                );
            return (ExceptionKind) map.get(cValue);
        }

        /**
         * Get the C value from the enum instance.
         *
         * @return The int C value of the enum instance.
         */
        public int toC() {
            return this.value;
        }

    }

    ${java_doc('langkit.node_kind_type', 4)}
    public enum NodeKind {

        // ----- Enum values -----

        % for n in ctx.astnode_types:
            <% name = n.kwless_raw_name.upper %>
            % if n.abstract:
        // ${name} is abstract
            % else:
        ${java_doc(n, 8)}
        ${name}(${ctx.node_kind_constants[n]}),
            % endif
        % endfor
        ;

        // ----- Class attributes -----

        /** Map containing relation from node kind value and enum instance. */
        private static final Map<Integer, NodeKind> map = new HashMap<>();

        // ----- Instance attributes -----

        /** Integer value of the node kind. */
        public final int value;

        /** Description associated to the node kind. */
        private Reflection.Node description;

        // ----- Constructors -----

        static {
            // Initialize the lookup map
            for(NodeKind elem : NodeKind.values()) {
                map.put(elem.value, elem);
            }
        }

        /** Private constructor. */
        private NodeKind(
            final int value
        ) {
            this.value = value;

            // The "description" field is intiialized with null to avoid
            // static code execution order issues.
            this.description = null;
        }

        // ----- Class methods -----

        /**
         * Get the enum instance from the given C integer value.
         *
         * @throws EnumException If the given C value is not a valid enum
         *                       value.
         */
        public static NodeKind fromC(
            final int cValue
        ) {
            if(!map.containsKey(cValue))
                throw new EnumException("Cannot get NodeKind from " + cValue);
            return (NodeKind) map.get(cValue);
        }

        // ----- Instance methods -----

        /** Get the C integer value of the enum instance. */
        public int toC() {
            return this.value;
        }

        public Reflection.Node getDescription() {
            return this.description;
        }

        void setDescription(Reflection.Node description) {
            this.description = description;
        }

    }

    /** This enum contains all language nodes' member references */
    public enum MemberReference {

        // ----- Enum values -----

        % for i, m in enumerate(generic_api.all_members, 1):
            <%
            prefix = capi.symbol_prefix.upper()
            member_name = generic_api.member_name(m).upper()
            %>
        ${prefix}_${member_name}(${i}),
        % endfor
        ;

        // ----- Class attributes -----

        /** Map containing relation from native value to Jave instance. */
        private static Map<Integer, MemberReference> map = new HashMap<>();

        // ----- Instance attribtues -----

        /** Native value if the member reference. */
        private final int value;

        // ----- Constructors -----

        /** Private constructor. */
        private MemberReference (
            final int value
        ) {
            this.value = value;
        }

        static {
            // Initialize the lookup map
            for(MemberReference elem : MemberReference.values()) {
                map.put(elem.value, elem);
            }
        }

        // ----- Class methods -----

        /**
         * Get the enum instance from the given C integer value.
         *
         * @throws EnumException If the given C value is not a valid enum
         *                       value.
         */
        public static MemberReference fromC(
            final int cValue
        ) {
            if(!map.containsKey(cValue))
                throw new EnumException(
                    "Cannot get MemberReference from " + cValue
                );
            return (MemberReference) map.get(cValue);
        }

        // ----- Instance methods -----

        /** Get the native value of the enum instance. */
        public int toC() {
            return this.value;
        }

    }

    // ===== Generated enums =====

    % for enum_type in ctx.enum_types:
    ${enum.decl(enum_type)}
    % endfor

    // ==========
    // Java wrapping classes
    // ==========

    // ===== Constant structure wrapping classes =====

    /**
     * This class provides static methods to help wrapping and unwrapping
     * native boolean values.
     */
    public static final class BooleanWrapper {

        // ----- Graal C API methods -----

        static boolean wrap(
            final CCharPointer pointer
        ) {
            return pointer.read() != 0;
        }

        static boolean wrap(
            final byte nativeValue
        ) {
            return nativeValue != 0;
        }

    }

    /**
     * This class provides static methods to help wrapping and unwrapping
     * native integer values.
     */
    public static final class IntegerWrapper {

        // ----- Graal C API methods -----

        static int wrap(
            final CIntPointer pointer
        ) {
            return pointer.read();
        }

        static int wrap(
            final int nativeValue
        ) {
            return nativeValue;
        }

    }

    /**
     * This class wraps the langkit characters which are 32 bit wide.
     */
    public static final class Char {

        // ----- Class attributes -----

        /** Singleton that represents the none char. */
        public static final Char NONE = new Char(0);

        // ----- Instance attributes -----

        /** The value of the character. */
        public final int value;

        // ----- Constructors -----

        /**
         * Create a new character from its value. In langkit characters are
         * 32 bit wide so represented by Java integer.
         *
         * @param value The value of the character.
         */
        Char(
            final int value
        ) {
            this.value = value;
        }

        /**
         * Create a character from its integer value.
         *
         * @param value The character value.
         * @return The newly created character.
         */
        public static Char create(
            final int value
        ) {
            return new Char(value);
        }

        /**
         * Create a character from a Java character.
         *
         * @param value The source value of the character.
         * @return The newly created character.
         */
        public static Char create(
            final char value
        ) {
            return new Char((int) value);
        }

        // ----- Graal C API methods -----

        /**
         * Wrap the given NI pointer in a Java class.
         *
         * @param pointer The NI pointer to wrap.
         * @return The wrapped character.
         */
        static Char wrap(
            final CIntPointer pointer
        ) {
            return wrap(pointer.read());
        }

        /**
         * Wrap an integer to a character.
         *
         * @param value The value of the character in an integer.
         * @return The newly created character.
         */
        static Char wrap(
            final int value
        ) {
            return new Char(value);
        }

        /**
         * Unwrap the character in the given int pointer.
         *
         * @param pointer The pointer to unwrap the character in.
         */
        void unwrap(
            final CIntPointer pointer
        ) {
            pointer.write(this.value);
        }

        /**
         * Unwrap the character in a Java integer.
         *
         * @return The character value in a Java integer.
         */
        int unwrap() {
            return this.value;
        }

        // ----- Override methods -----

        @Override
        public String toString() {
            final ByteBuffer buffer = ByteBuffer.allocate(4);
            buffer.order(BYTE_ORDER);
            buffer.putInt(this.value);
            return decodeUTF32(buffer.array());
        }

        @Override
        public boolean equals(Object o) {
            if(o == this) return true;
            if(!(o instanceof Char)) return false;
            Char other = (Char) o;
            return this.value == other.value;
        }

    }

    ${java_doc('langkit.big_integer_type', 4)}
    static final class BigIntegerWrapper {

        // ----- Class attributes -----

        /** Singleton that represents the none big integer. */
        public static final BigInteger NONE = BigInteger.ZERO;

        // ----- Graal C API methods -----

        /**
         * Wrap a pointer which points to a native big integer.
         *
         * @param pointer The pointer to the native big integer.
         * @return The Java big integer.
         */
        static BigInteger wrap(
            final WordPointer pointer
        ) {
            return wrap((BigIntegerNative) pointer.read());
        }

        /**
         * Wrap the given native big integer in a Java big integer.
         *
         * @param bigIntegerNative The big integer native value.
         * @return The Java big integer.
         */
        static BigInteger wrap(
            final BigIntegerNative bigIntegerNative
        ) {
            final String representation = getRepresentation(bigIntegerNative);
            return new BigInteger(representation);
        }

        /**
         * Unwrap the given Java big integer in the given word pointer.
         *
         * @param bigInteger The big integer to unwrap.
         * @param pointer The word pointer to place the big integer in.
         */
        static void unwrap(
            final BigInteger bigInteger,
            final WordPointer pointer
        ) {
            pointer.write(unwrap(bigInteger));
        }

        /**
         * Unwrap the given big integer.
         *
         * @param bigInteger The big integer to unwrap.
         * @return The native big integer newly allocated.
         */
        static BigIntegerNative unwrap(
            final BigInteger bigInteger
        ) {
            // Create the representation of the big integer
            final String representation = bigInteger.toString();
            try(final Text bigIntegerText = Text.create(representation)) {
                TextNative bigIntegerTextNative = StackValue.get(
                    TextNative.class
                );
                bigIntegerText.unwrap(bigIntegerTextNative);

                // Create the big intger by calling the native function
                return NI_LIB.${nat("create_big_integer")}(
                    bigIntegerTextNative
                );
            }
        }

        /**
         * Release the big integer pointed by the given pointer.
         *
         * @param pointer The pointer to the big integer to release.
         */
        static void release(
            final WordPointer pointer
        ) {
            release((BigIntegerNative) pointer.read());
        }

        /**
         * Release the given native big integer.
         *
         * @param bigIntegerNative The native big integer to release.
         */
        static void release(
            final BigIntegerNative bigIntegerNative
        ) {
            NI_LIB.${nat("big_integer_decref")}(bigIntegerNative);
        }

        // ----- Class methods -----

        /**
         * Get the string representation of the given native big integer.
         *
         * @param bigIntegerNative The native big integer to get the
         * representation from.
         */
        private static String getRepresentation(
            final BigIntegerNative bigIntegerNative
        ) {
            // Allocate the stack value for the text
            final TextNative bigIntegerTextNative = StackValue.get(
                TextNative.class
            );
            Text.NONE.unwrap(bigIntegerTextNative);

            // Call the native function
            NI_LIB.${nat("big_integer_text")}(
                bigIntegerNative,
                bigIntegerTextNative
            );

            // Wrap the text and return the result
            try(final Text bigIntegerText = Text.wrap(bigIntegerTextNative)) {
                return bigIntegerText.getContent();
            }
        }

    }

    ${java_doc('langkit.symbol_type', 4)}
    public static final class Symbol {

        // ----- Class attributes -----

        /** Singleton that represents the none symbol. */
        public static final Symbol NONE = new Symbol("");

        // ----- Instance attributes

        /** The text of the symbol. */
        public final String text;

        // ----- Constructors -----

        /**
         * Create a new symbol from its text.
         *
         * @param text The symbol text.
         */
        Symbol(
            final String text
        ) {
            this.text = text;
        }

        /**
         * Public access to the symbol creation.
         *
         * @param text The text of the symbol.
         */
        public static Symbol create(
            final String text
        ) {
            return new Symbol(text);
        }

        // ----- Graal C API methods -----

        /**
         * Wrap the given pointer to a native symbol.
         *
         * @param pointer The pointer to the native symbol.
         * @return The wrapped symbol.
         */
        static Symbol wrap(
            final WordPointer pointer
        ) {
            return wrap((SymbolNative) pointer.read());
        }

        /**
         * Wrap the given symbol native value in a Java value.
         *
         * @param symbolNative The symbol native value.
         * @return The wrapped symbol.
         */
        static Symbol wrap(
            final SymbolNative symbolNative
        ) {
            // Get the symbol text
            final TextNative textNative = StackValue.get(TextNative.class);
            Text.NONE.unwrap(textNative);
            NI_LIB.${nat("symbol_text")}(
                symbolNative,
                textNative
            );

            // Return the new symbol
            try(final Text text = Text.wrap(textNative)) {
                return new Symbol(text.getContent());
            }
        }

        /**
         * Unwrap the symbol in the given native structure.
         *
         * @param symbolNative The native structure to unwrap in.
         */
        void unwrap(
            final SymbolNative symbolNative,
            final AnalysisContext context
        ) {
            // Unwrap the symbol text
            try(Text text = Text.create(this.text)) {
                final TextNative textNative = StackValue.get(
                    TextNative.class
                );
                text.unwrap(textNative);

                // Call the symbol creation
                final int resCode = NI_LIB.${nat("context_symbol")}(
                    context.reference.ni(),
                    textNative,
                    symbolNative
                );

                // Check the symbol creation success
                if(resCode == 0) {
                    throw new SymbolException(this.text);
                }
            }
        }

        // ----- Override methods -----

        @Override
        public String toString() {
            return this.text;
        }

    }

    ${java_doc('langkit.string_type', 4)}
    static final class StringWrapper {

        // ----- Class attributes -----

        /** Singleton that represents the none string. */
        public static final String NONE = "";

        // ----- Graal C API methods -----

        /**
         * Wrap a pointer that points to a native string.
         *
         * @param pointer The pointer to the native string.
         * @return The Java string wrapped from the native string.
         */
        static String wrap(
            final WordPointer pointer
        ) {
            return wrap((StringNative) pointer.read());
        }

        /**
         * Wrap a native string wrapper in a Java string.
         *
         * @param stringNative The native string to wrap.
         * @return The Java string created from the native one.
         */
        static String wrap(
            final StringNative stringNative
        ) {
            return getNativeStringContent(stringNative);
        }

        /**
         * Unwrap the string in the given word pointer.
         *
         * @param string The string to unwrap.
         * @param pointer The word pointer to place the native string in.
         */
        static void unwrap(
            final String string,
            final WordPointer pointer
        ) {
            pointer.write(unwrap(string));
        }

        /**
         * Unwrap the given string in a native one.
         *
         * @param string The string to unwrap.
         * @return The native string unwrapped.
         */
        static StringNative unwrap(
            final String string
        ) {
            return nativeStringFromContent(string);
        }

        /**
         * Release the native string at the given pointer.
         *
         * @param pointer The pointer to the native string.
         */
        static void release(
            final WordPointer pointer
        ) {
            release((StringNative) pointer.read());
        }

        /**
         * Release the given native string.
         *
         * @param stringNative The native string to release.
         */
        static void release(
            final StringNative stringNative
        ) {
            NI_LIB.${nat("string_dec_ref")}(stringNative);
        }

        // ----- Class methods -----

        /**
         * Get the content of the given native string in a Java one.
         *
         * @param stringNative The native string to get the content from.
         * @return The Java string.
         */
        private static String getNativeStringContent(
            final StringNative stringNative
        ) {
            // Prepare the working variable
            final Pointer pointer = (Pointer) stringNative;
            final int length = pointer.readInt(0);
            final byte[] contentArray = new byte[length * 4];

            // Get the content from the native string
            for(int i = 0 ; i < contentArray.length ; i++) {
                contentArray[i] = pointer.readByte(i + 8);
            }

            // Return the decoded string
            return decodeUTF32(contentArray);
        }

        /**
         * Create a native string from a Java string.
         *
         * @param string The Java string to create the native string from.
         * @return The native string.
         */
        private static StringNative nativeStringFromContent(
            final String string
        ) {
            // Encode the string in UTF-32
            final byte[] contentArray = encodeUTF32(string);
            final int length = string.length();

            // Create the native array
            final Pointer contentArrayNative = UnmanagedMemory.malloc(
                contentArray.length
            );
            for(int i = 0 ; i < contentArray.length ; i++) {
                contentArrayNative.writeByte(i, contentArray[i]);
            }

            // Create the native string
            final StringNative res = NI_LIB.${nat("create_string")}(
                (CIntPointer) contentArrayNative,
                length
            );

            // Free the memory
            UnmanagedMemory.free(contentArrayNative);

            // Return the result
            return res;
        }

    }

    ${java_doc('langkit.text_type', 4)}
    public static final class Text extends LangkitSupport.Text {

        // ----- Class attributes -----

        /** Singleton that represents the none text. */
        public static final Text NONE = new Text(
            PointerWrapper.nullPointer(),
            0,
            false
        );

        // ----- Instance attributes -----

        /** The pointer to the characters. */
        private final PointerWrapper charPointer;

        /** The size of the text. */
        private final long length;

        /** If the text is allocated. */
        private final boolean isAllocated;

        /** If the text object is the owner of its buffer. */
        private final boolean isOwner;

        /** The content of the text in a Java string. */
        private String content;

        // ----- Constructors -----

        /**
         * Create a new text from its content.
         *
         * @param charPointer The pointer to the characters of the text.
         * @param length The length of the text in character.
         * @param isAllocated If the text is allocated in the memory.
         */
        Text(
            final PointerWrapper charPointer,
            final long length,
            final boolean isAllocated
        ) {
            this(
                charPointer,
                length,
                isAllocated,
                false,
                null
            );
        }

        /**
         * Create a new text from its content and buffer.
         *
         * @param charPointer The pointer to the characters of the text.
         * @param length The length of the text in character.
         * @param isAllocated If the text is allocated in the memory.
         * @param contentArray The characters of the text (for JNI)
         */
        Text(
            final PointerWrapper charPointer,
            final long length,
            final boolean isAllocated,
            final byte[] contentArray
        ) {
            this(
                charPointer,
                length,
                isAllocated,
                false,
                contentArray
            );
        }

        /**
         * Create a new text from its content and buffer.
         *
         * @param charPointer The pointer to the characters of the text.
         * @param length The length of the text in character.
         * @param isAllocated If the text is allocated in the memory.
         * @param isOwner If the Java object owns the text buffer.
         * @param contentArray The characters of the text
         * (as strings, this is only used in JNI mode).
         */
        private Text(
            final PointerWrapper charPointer,
            final long length,
            final boolean isAllocated,
            final boolean isOwner,
            final byte[] contentArray
        ) {
            this.charPointer = charPointer;
            this.length = length;
            this.isAllocated = isAllocated;
            this.isOwner = isOwner;

            if(contentArray != null) {
                this.content = decodeUTF32(contentArray);
            } else {
                this.content = null;
            }
        }

        /**
         * Create a new langkit text from its string content.
         *
         * @param content The content of the text in a Java string.
         * @return The newly created text.
         */
        public static Text create(
            final String content
        ) {
            final byte[] contentArray = encodeUTF32(content);

            if(ImageInfo.inImageCode()) {
                final PointerWrapper charPointer = new PointerWrapper(
                    toCBytes(contentArray)
                );
                return new Text(
                    charPointer,
                    (long) content.length(),
                    false,
                    true,
                    contentArray
                );
            } else {
                return JNI_LIB.${nat("create_text")}(contentArray);
            }
        }

        // ----- Graal C API methods -----

        /**
         * Wrap the native text pointed by the given pointer.
         *
         * @param pointer The pointer to the native text to wrap.
         * @return The wrapped text.
         */
        static Text wrap(
            final WordPointer pointer
        ) {
            return wrap((TextNative) pointer.read());
        }

        /**
         * Wrap a text native value in the Java class.
         *
         * @param textNative The text native NI value.
         * @return The newly wrapped text.
         */
        static Text wrap(
            final TextNative textNative
        ) {
            return new Text(
                new PointerWrapper(textNative.get_chars()),
                textNative.get_length(),
                textNative.get_is_allocated() != 0
            );
        }

        /**
         * Unwrap the text in the given NI pointer.
         *
         * @param textNative The NI pointer to the native text structure.
         */
        void unwrap(
            final TextNative textNative
        ) {
            textNative.set_chars(this.charPointer.ni());
            textNative.set_length(this.length);
            textNative.set_is_allocated(this.isAllocated ? 1 : 0);
        }

        // ----- Instance methods -----

        /** The pointer to the characters. */
        public final PointerWrapper getCharPointer() {
            return charPointer;
        }

        /** The size of the text. */
        public final long getLength() {
            return length;
        }

        /** If the text is allocated. */
        public final boolean getIsAllocated() {
            return isAllocated;
        }

        /** If the text object is the owner of its buffer. */
        public final boolean getIsOwner() {
            return isOwner;
        }

        /**
         * Get the content of the text in a Java string.
         *
         * @return the content of the text.
         */
        public String getContent() {
            // Content is null only when using the Graal C API.
            if(this.content == null) {
                final byte[] contentArray = new byte[(int) this.length * 4];
                for(int i = 0 ; i < contentArray.length ; i++) {
                    contentArray[i] = (
                        (CCharPointer) this.charPointer.ni()
                    ).read(i);
                }
                this.content = decodeUTF32(contentArray);
            }

            return this.content;
        }

        /** @see java.lang.AutoCloseable#close() */
        @Override
        public void close() {

            if(ImageInfo.inImageCode()) {
                if(this.isOwner) {
                    UnmanagedMemory.free(this.charPointer.ni());
                } else {
                    final TextNative textNative = StackValue.get(
                        TextNative.class
                    );
                    this.unwrap(textNative);
                    NI_LIB.${nat("destroy_text")}(textNative);
                }
            } else {
                JNI_LIB.${nat("destroy_text")}(this);
            }
            checkException();

        }

        // ----- Override methods -----

        @Override
        public String toString() {
            return this.getContent();
        }

    }

    ${java_doc('langkit.sloc_type', 4)}
    public static final
    class SourceLocation extends LangkitSupport.SourceLocation {

        // ----- Class attributes -----

        /** Singleton that represents the none source location. */
        public static final SourceLocation NONE = new SourceLocation(
            0,
            (short) 0
        );

        // ----- Constructors -----

        /**
         * Create a new source location from a line and a column.
         *
         * @param line The line of the source location.
         * @param column The column of the source location.
         */
        SourceLocation(
            final int line,
            final short column
        ) {
            super(line, column);
        }

        /**
         * Create a source location from its line and column.
         *
         * @param line The line.
         * @param column The column.
         * @return The newly create source location.
         */
        public static SourceLocation create(
            final int line,
            final short column
        ) {
            return new SourceLocation(
                line,
                column
            );
        }

        // ----- Graal C API methods -----

        /**
         * Wrap the given source location in the Java class.
         *
         * @param pointer Pointer to the native source location.
         * @return The newly wrapper source location.
         */
        static SourceLocation wrap(
            final WordPointer pointer
        ) {
            return wrap((SourceLocationNative) pointer.read());
        }

        /**
         * Wrap a source location native value in the Java class.
         *
         * @param sourceLocationNative The source location NI native value.
         * @return The newly wrapped source location.
         */
        static SourceLocation wrap(
            final SourceLocationNative sourceLocationNative
        ) {
            return new SourceLocation(
                sourceLocationNative.get_line(),
                sourceLocationNative.get_column()
            );
        }

        /**
         * Uwrap the source location in the given NI pointer.
         *
         * @param sourceLocationNative The NI pointer to the native structure
         *  to fill.
         */
        public void unwrap(
            final SourceLocationNative sourceLocationNative
        ) {
            sourceLocationNative.set_line(this.line);
            sourceLocationNative.set_column(this.column);
        }

        // ----- Override methods -----

        @Override
        public String toString() {
            return this.line + ":" + this.column;
        }

    }

    ${java_doc('langkit.sloc_range_type', 4)}
    public static final
    class SourceLocationRange extends LangkitSupport.SourceLocationRange {

        // ----- Class attributes -----

        /** Singleton that represents the none source location range. */
        public static final SourceLocationRange NONE =
            new SourceLocationRange(
                SourceLocation.NONE,
                SourceLocation.NONE
            );

        // ----- Constructors -----

        /**
         * Create a source location range from its bounds.
         *
         * @param start The start of the range.
         * @param end The end of the range.
         */
        SourceLocationRange(
            final SourceLocation start,
            final SourceLocation end
        ) {
            super(start, end);
        }

        /**
         * Create a new source location range from its bounds.
         *
         * @param start The starting bound.
         * @param end The ending bound.
         * @return The newly created source location range.
         */
        public static SourceLocationRange create(
            final SourceLocation start,
            final SourceLocation end
        ) {
            return new SourceLocationRange(
                start,
                end
            );
        }

        // ----- Graal C API methods -----

        /**
         * Wrap the given source location range in the Java class.*
         *
         * @param pointer The pointer to the native source location range.
         * @return The newly wrapped source location range.
         */
        static SourceLocationRange wrap(
            final WordPointer pointer
        ) {
            return wrap((SourceLocationRangeNative) pointer.read());
        }

        /**
         * Wrap a source location range native value in the Java class.
         *
         * @param sourceLocationRangeNative The source location range NI
         * native value.
         * @return The newly wrapped source location range.
         */
        static SourceLocationRange wrap(
            final SourceLocationRangeNative sourceLocationRangeNative
        ) {
            return new SourceLocationRange(
                new SourceLocation(
                    sourceLocationRangeNative.get_start_line(),
                    sourceLocationRangeNative.get_start_column()
                ),
                new SourceLocation(
                    sourceLocationRangeNative.get_end_line(),
                    sourceLocationRangeNative.get_end_column()
                )
            );
        }

        /**
         * Uwrap the source location range in the given NI pointer.
         *
         * @param sourceLocationRangeNative The NI pointer to the native
         * structure to fill.
         */
        void unwrap(
            final SourceLocationRangeNative sourceLocationRangeNative
        ) {
            sourceLocationRangeNative.set_start_line(this.start.line);
            sourceLocationRangeNative.set_start_column(this.start.column);
            sourceLocationRangeNative.set_end_line(this.end.line);
            sourceLocationRangeNative.set_end_column(this.end.column);
        }

        // ----- Override methods -----

        @Override
        public String toString() {
            return this.start.toString() + "-" + this.end.toString();
        }

    }

    ${java_doc('langkit.diagnostic_type', 4)}
    public static final class Diagnostic extends LangkitSupport.Diagnostic {

        // ----- Class attributes -----

        /** Singleton that represents the none diagnostic. */
        public static final Diagnostic NONE = new Diagnostic(
            SourceLocationRange.NONE,
            Text.NONE
        );

        // ----- Instance attributes -----

        /** The source location range of the diagnostic. */
        public final SourceLocationRange sourceLocationRange;

        /** The message of the diagnostic. */
        public final Text message;

        // ----- Getters -----

        public final SourceLocationRange getSourceLocationRange() {
            return sourceLocationRange;
        }

        public final Text getMessage() {
            return message;
        }

        // ----- Constructors -----

        /**
         * Create a diagnostic from its content.
         *
         * @param sourceLocationRange The range of the diagnostic.
         * @param message The message of the diagnostic.
         */
        Diagnostic(
            final SourceLocationRange sourceLocationRange,
            final Text message
        ) {
            this.sourceLocationRange = sourceLocationRange;
            this.message = message;
        }

        /**
         * Create a new diagnostic from its content.
         *
         * @param sourceLocationRange The source location range concerned by
         * this diagnostic.
         * @param message The message of the diagnostic.
         * @return The newly created diagnostic
         */
        public static Diagnostic create(
            final SourceLocationRange sourceLocationRange,
            final Text message
        ) {
            return new Diagnostic(
                sourceLocationRange,
                message
            );
        }

        // ----- Graal C API methods -----

        /**
         * Wrap a pointer to a native diagnostic.
         *
         * @param pointer The pointer to the native diagnostic.
         * @return The wrapped diagnostic.
         */
        static Diagnostic wrap(
            final WordPointer pointer
        ) {
            return wrap((DiagnosticNative) pointer.read());
        }

        /**
         * Wrap a diagnostic native value in the Java class.
         *
         * @param diagnosticNative The diagnostic NI native value.
         * @return The newly wrapped diagnostic.
         */
        static Diagnostic wrap(
            final DiagnosticNative diagnosticNative
        ) {
            return new Diagnostic(
                new SourceLocationRange(
                    new SourceLocation(
                        diagnosticNative.get_start_line(),
                        diagnosticNative.get_start_column()
                    ),
                    new SourceLocation(
                        diagnosticNative.get_end_line(),
                        diagnosticNative.get_end_column()
                    )
                ),
                new Text(
                    new PointerWrapper(diagnosticNative.get_message_chars()),
                    diagnosticNative.get_message_length(),
                    diagnosticNative.get_message_is_allocated() != 0
                )
            );
        }

        /**
         * Unwrap the diagnostic in the given NI pointer.
         *
         * @param diagnosticNative The pointer to the native structure to
         * fill.
         */
        public void unwrap(
            final DiagnosticNative diagnosticNative
        ) {
            diagnosticNative.set_start_line(
                this.sourceLocationRange.start.line
            );
            diagnosticNative.set_start_column(
                this.sourceLocationRange.start.column
            );
            diagnosticNative.set_end_line(
                this.sourceLocationRange.end.line
            );
            diagnosticNative.set_end_column(
                this.sourceLocationRange.end.column
            );
            diagnosticNative.set_message_chars(
                this.message.charPointer.ni()
            );
            diagnosticNative.set_message_length(
                this.message.length
            );
            diagnosticNative.set_message_is_allocated(
                this.message.isAllocated ? 1 : 0
            );
        }

        // ----- Override methods -----

        @Override
        public String toString() {
            return this.message.toString()
                + " at <"
                + this.sourceLocationRange.toString()
                + ">";
        }

    }

    % if ctx.generate_unparsers:
    ${java_doc('langkit.rewriting.apply_result_type', 4)}
    public static final class RewritingApplyResult implements AutoCloseable {

        // ----- Instance attributes -----

        /** Whether the rewriting application was successful. */
        public final boolean success;

        /**
         * If the rewriting failed, this is the unit on which the rewriting
         * failed. Otherwise it is the None unit.
         */
        public final AnalysisUnit unit;

        /**
         * Number of diagnostics in the result. 0 if the apply has been a
         * success.
         */
        private final int diagnosticsCount;

        /** Pointer to the diagnostic array, null if successful. */
        private final PointerWrapper diagnosticsReference;

        /** Cache for the unwrapped diagnostics. */
        private Diagnostic[] diagnostics;

        // ----- Constructors ------

        /** Create a new rewriting result with its content. */
        RewritingApplyResult(
            final boolean success,
            final AnalysisUnit unit,
            final int diagnosticsCount,
            final PointerWrapper diagnosticsReference
        ) {
            this.success = success;
            this.unit = unit;
            this.diagnosticsCount = diagnosticsCount;
            this.diagnosticsReference = diagnosticsReference;
            this.diagnostics = null;
        }

        /** Shortcut creation method for success. */
        static RewritingApplyResult success() {
            return new RewritingApplyResult(
                true,
                AnalysisUnit.NONE,
                0,
                PointerWrapper.nullPointer()
            );
        }

        // ----- Graal C API methods -----

        /** Wrap a pointer to a native rewriting apply result. */
        static RewritingApplyResult wrap(
            final Pointer pointer
        ) {
            return wrap((RewritingApplyResultNative) pointer.readWord(0));
        }

        /** Wrap a native rewriting apply result to the Java class. */
        static RewritingApplyResult wrap(
            final RewritingApplyResultNative rewritingApplyResultNative
        ) {
            if(rewritingApplyResultNative.get_success() > 0) {
                return RewritingApplyResult.success();
            }
            return new RewritingApplyResult(
                false,
                AnalysisUnit.wrap(rewritingApplyResultNative.get_unit()),
                rewritingApplyResultNative.get_diagnostics_count(),
                new PointerWrapper(
                    rewritingApplyResultNative.get_diagnostics()
                )
            );
        }

        /** Unwrap the rewriting apply result to the provided native value. */
        void unwrap(
            final RewritingApplyResultNative rewritingApplyResultNative
        ) {
            rewritingApplyResultNative.set_success(this.success ? 1 : 0);
            rewritingApplyResultNative.set_unit(this.unit.unwrap());
            rewritingApplyResultNative.set_diagnostics_count(
                this.diagnosticsCount
            );
            rewritingApplyResultNative.set_diagnostics(
                (DiagnosticNative) this.diagnosticsReference.ni()
            );
        }

        // ----- Instance methods -----

        /** Unwrap all diagnostics, cache the result and return it */
        public Diagnostic[] getDiagnostics() {
            if(this.diagnostics == null) {
                if(this.success) {
                    this.diagnostics = new Diagnostic[0];
                } else {

                    if(ImageInfo.inImageCode()) {
                        this.diagnostics =
                            new Diagnostic[this.diagnosticsCount];
                        final Pointer diagnostics =
                            (Pointer) this.diagnosticsReference.ni();
                        DiagnosticNative diagnosticNative;
                        final int diagnosticNativeSize = SizeOf.get(
                            DiagnosticNative.class
                        );
                        for(int i = 0; i < this.diagnosticsCount; i++) {
                            diagnosticNative = WordFactory.unsigned(
                                diagnostics.add(i * diagnosticNativeSize)
                                        .rawValue()
                            );
                            this.diagnostics[i] = Diagnostic.wrap(
                                diagnosticNative
                            );
                        }
                    } else {
                        this.diagnostics =
                            JNI_LIB
                                .${nat("rewriting_get_result_diagnostics")}(
                                    this.diagnosticsCount,
                                    this.diagnosticsReference.jni()
                                );
                    }

                }
            }
            return this.diagnostics;
        }

        ${java_doc('langkit.rewriting.free_apply_result', 8)}
        @Override
        public void close() {

            if(ImageInfo.inImageCode()) {
                final RewritingApplyResultNative resultNative =
                    StackValue.get(RewritingApplyResultNative.class);
                unwrap(resultNative);
                NI_LIB.${nat("rewriting_free_apply_result")}(resultNative);
            } else {
                JNI_LIB.${nat("rewriting_free_apply_result")}(this);
            }

        }

    }
    % endif

    ${java_doc('langkit.file_reader_type', 4)}
    public static final class FileReader implements AutoCloseable {

        // ----- Class attributes -----

        /** Singleton that represents the none file reader. */
        public static final FileReader NONE = new FileReader(
            PointerWrapper.nullPointer()
        );

        // ----- Instance attributes -----

        /** The reference to the file reader */
        private final PointerWrapper reference;

        // ----- Constructors -----

        /**
         * Create a new file reader from its native reference.
         *
         * @param reference The reference to the native file reader.
         */
        FileReader(
            final PointerWrapper reference
        ) {
            this.reference = reference;
        }

        // ----- Graal C API methods -----

        /**
         * Wrap a pointer to a native file reader.
         *
         * @param pointer The pointer to the native file reader.
         * @return The newly wrapped file reader.
         */
        static FileReader wrap(
            final WordPointer pointer
        ) {
            return wrap((FileReaderNative) pointer.read());
        }

        /**
         * Wrap the given file reader in the Java class.
         *
         * @param fileReaderNative The native file reader to wrap.
         * @return The wrapped file reader.
         */
        static FileReader wrap(
            final FileReaderNative fileReaderNative
        ) {
            return new FileReader(
                new PointerWrapper(fileReaderNative)
            );
        }

        /**
         * Unwrap the file reader in the given pointer.
         *
         * @param pointer The pointer to unwrap the file reader in.
         */
        void unwrap(
            final WordPointer pointer
        ) {
            pointer.write(0, this.unwrap());
        }

        /**
         * Get the unwrapped file reader.
         *
         * @return The unwrapped native file reader.
         */
        FileReaderNative unwrap() {
            return (FileReaderNative) this.reference.ni();
        }

        // ----- Instance methods -----

        /** @see java.lang.AutoCloseable#close() */
        @Override
        public void close() {

            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("dec_ref_file_reader")}(
                    this.reference.ni()
                );
            } else {
                JNI_LIB.${nat("dec_ref_file_reader")}(this);
            }
            checkException();

        }

    }

    ${java_doc('langkit.unit_provider_type', 4)}
    public static final class UnitProvider implements AutoCloseable {

        // ----- Class attributes -----

        /** Singleton that represents the none unit provider. */
        public static final UnitProvider NONE = new UnitProvider(
            PointerWrapper.nullPointer()
        );

        // ----- Instance attributes -----

        /** The reference to the unit provider */
        private final PointerWrapper reference;

        // ----- Constructors -----

        /**
         * Create a new unit provider with the reference to the native one.
         *
         * @param reference The reference to the native unit provider.
         */
        UnitProvider(
            final PointerWrapper reference
        ) {
            this.reference = reference;
        }

        // ----- Graal C API methods -----

        /**
         * Wrap the given pointer to a native unit provider.
         *
         * @param pointer The pointer to the native unit provider.
         * @return The wrapped unit provider.
         */
        static UnitProvider wrap(
            final WordPointer pointer
        ) {
            return wrap((UnitProviderNative) pointer.read());
        }

        /**
         * Wrap the given native unit provider.
         *
         * @param unitProviderNative The native unit provider.
         * @return The wrapped unit provider.
         */
        static UnitProvider wrap(
            final UnitProviderNative unitProviderNative
        ) {
            return new UnitProvider(
                new PointerWrapper(unitProviderNative)
            );
        }

        /**
         * Unwrap the unit provider in the given native pointer.
         *
         * @param pointer The pointer to place the native unit provider in.
         */
        void unwrap(
            final WordPointer pointer
        ) {
            pointer.write(this.unwrap());
        }

        /**
         * Get the native unit provider.
         *
         * @return The native unit provider.
         */
        UnitProviderNative unwrap() {
            return (UnitProviderNative) this.reference.ni();
        }

        // ----- Instance methods -----

        /** @see java.lang.AutoCloseable#close() */
        public void close() {

            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("dec_ref_unit_provider")}(this.reference.ni());
            } else {
                JNI_LIB.${nat("dec_ref_unit_provider")}(this);
            }
            checkException();

        }

    }

    ${java_doc('langkit.event_handler_type', 4)}
    public static final class EventHandler implements AutoCloseable {

        // ----- Class attributes -----

        /** Singleton that represents the none event handler. */
        public static final EventHandler NONE = new EventHandler(
            PointerWrapper.nullPointer(),
            null,
            null
        );

        /** This map contains all created event handlers. */
        private static final Map<PointerWrapper, EventHandler>
            eventHandlerCache = new ConcurrentHashMap<>();

        // ----- Instance attributes -----

        /** The reference to the native event handler. */
        private final PointerWrapper reference;

        /**
         * The Java callback when an analysis unit is requested in the
         * associated context.
         */
        private final UnitRequestedCallback unitRequestedCallback;

        /**
         * The Java callback when an analysis unit is parsed in the
         * associated context.
         */
        private final UnitParsedCallback unitParsedCallback;

        // ----- Constructors -----

        /**
         * Create a new event handler from its native reference.
         *
         * @param reference The reference to the native event handler.
         * @param unitRequestedCallback The callback for unit requests.
         * @param unitParsedCallback The callback for unit parsing.
         */
        EventHandler(
            final PointerWrapper reference,
            final UnitRequestedCallback unitRequestedCallback,
            final UnitParsedCallback unitParsedCallback
        ) {
            this.reference = reference;
            this.unitRequestedCallback = unitRequestedCallback;
            this.unitParsedCallback = unitParsedCallback;
        }

        /**
         * Create a new even handler with its callbacks. Callbacks can be null.
         *
         * @param unitRequestedCallback The callback for analysis unit requests.
         * @param unitParsedCallback The callback for analysis unit parsing.
         */
        public static EventHandler create(
            final UnitRequestedCallback unitRequestedCallback,
            final UnitParsedCallback unitParsedCallback
        ) {
            // Prepare the reference to the native event handler
            final PointerWrapper reference;

            if(ImageInfo.inImageCode()) {
                // Get the current thread
                final IsolateThread thread = CurrentIsolate.getCurrentThread();

                // Create the native event handler
                final EventHandlerNative resNative =
                    NI_LIB.${nat("create_event_handler")}(
                        (VoidPointer) thread,
                        WordFactory.nullPointer(),
                        NI_LIB.unitRequestedFunction.getFunctionPointer(),
                        NI_LIB.unitParsedFunction.getFunctionPointer()
                    );

                // Set the result to the created event handler
                reference = new PointerWrapper(resNative);
            } else {
                reference = JNI_LIB.${nat("create_event_handler")}(
                    unitRequestedCallback,
                    unitParsedCallback
                );
            }

            // Return the new event handler wrapped object
            return new EventHandler(
                reference,
                unitRequestedCallback,
                unitParsedCallback
            );
        }

        /**
         * Get event handler Java object from its native pointer.
         *
         * @param reference The pointer to the native event handler.
         * @return The associated Java event handler.
         */
        static EventHandler fromReference(
            final PointerWrapper reference
        ) {
            if(eventHandlerCache.containsKey(reference)) {
                return eventHandlerCache.get(reference);
            } else {
                throw new ReferenceException(
                    "Cannot get event handler from this reference: " +
                    reference.toString()
                );
            }
        }

        // ----- Graal C API methods -----

        /**
         * Wrap the given pointer to an event handler.
         *
         * @param pointer The pointer to the native event handler.
         * @return The wrapped event handler.
         */
        EventHandler wrap(
            final WordPointer pointer
        ) {
            return wrap((EventHandlerNative) pointer.read());
        }

        /**
         * Wrap the given native event handler.
         *
         * @param eventHandlerNative The native value of the event handler.
         * @return The wrapped event handler.
         */
        EventHandler wrap(
            final EventHandlerNative eventHandlerNative
        ) {
            return fromReference(new PointerWrapper(eventHandlerNative));
        }

        /**
         * Unwrap the event handler in the given native pointer.
         *
         * @param pointer The pointer to place the native event handler.
         */
        void unwrap(
            final WordPointer pointer
        ) {
            pointer.write(this.unwrap());
        }

        /**
         * Unwrap the event handler to a native value.
         *
         * @return The native value of the event handler.
         */
        EventHandlerNative unwrap() {
            return (EventHandlerNative) this.reference.ni();
        }

        // ----- Getters -----

        public UnitRequestedCallback getUnitRequestCallback() {
            return this.unitRequestedCallback;
        }

        public UnitParsedCallback getUnitParsedCallback() {
            return this.unitParsedCallback;
        }

        // ----- Instance methods -----

        /** @see java.lang.AutoCloseable#close() */
        public void close() {

            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("dec_ref_event_handler")}(this.reference.ni());
            } else {
                JNI_LIB.${nat("dec_ref_event_handler")}(this);
            }
            checkException();

        }

        // ----- Inner classes -----

        ${java_doc('langkit.event_handler_unit_requested_callback', 8)}
        @FunctionalInterface
        public interface UnitRequestedCallback {
            void invoke(
                AnalysisContext context,
                String name,
                AnalysisUnit from,
                boolean found,
                boolean isNotFoundError
            );
        }

        ${java_doc('langkit.event_handler_unit_parsed_callback', 8)}
        @FunctionalInterface
        public interface UnitParsedCallback {
            void invoke(
                AnalysisContext context,
                AnalysisUnit unit,
                boolean reparsed
            );
        }

    }

    ${java_doc('langkit.token_reference_type', 4)}
    public static class Token {

        // ----- Instance attributes -----

        /**
         * We only store the reference to the context to avoid ref-count
         * problems. To access the token context go throught the
         * analysis unit.
         */
        protected final PointerWrapper contextRef;

        /** The unit of the token. */
        public final AnalysisUnit unit;

        /** The pointer to the token data handler. */
        protected final PointerWrapper tokenDataHandler;

        /** The index of the token. */
        public final int tokenIndex;

        /** The trivia index. */
        public final int triviaIndex;

        /** The kind of the token. */
        public final TokenKind kind;

        /** The text of the token. */
        protected final String text;

        /** The source location range of the token. */
        public final SourceLocationRange sourceLocationRange;

        // ----- Constructors -----

        /**
         * Create a new token from its content.
         *
         * @param contextRef The context of the token.
         * @param unit The unit which owns the token.
         * @param tokenDataHandler The pointer to the token data.
         * @param tokenIndex The index of the token.
         * @param triviaIndex The trivia index of the token.
         * @param kind The kind of the token in an integer.
         * @param text The text of the token.
         * @param sourceLocationRange The location of the token.
         */
        Token(
            final PointerWrapper contextRef,
            final AnalysisUnit unit,
            final PointerWrapper tokenDataHandler,
            final int tokenIndex,
            final int triviaIndex,
            final TokenKind kind,
            final String text,
            final SourceLocationRange sourceLocationRange
        ) {
            this.contextRef = contextRef;
            this.unit = unit;
            this.tokenDataHandler = tokenDataHandler;
            this.tokenIndex = tokenIndex;
            this.triviaIndex = triviaIndex;
            this.kind = kind;
            this.text = text;
            this.sourceLocationRange = sourceLocationRange;
        }

        // ----- Graal C API methods -----

        /**
         * Wrap the pointer to a native token.
         *
         * @param pointer The pointer to the native token.
         * @param unit The analysis unit which owns the token.
         * @return The wrapped token or a none value if the token is a none
         * one.
         */
        static Token wrap(
            final WordPointer pointer,
            final AnalysisUnit unit
        ) {
            return wrap(
                (TokenNative) pointer.read(),
                unit
            );
        }

        /**
         * Wrap a native token value in the Java class.
         *
         * @param tokenNative The native NI token value.
         * @param unit The analysis unit that owns the token.
         * @return The wrapped token or a none value if the token data
         * handler is null.
         */
        static Token wrap(
            final TokenNative tokenNative,
            final AnalysisUnit unit
        ) {
            if(tokenNative.get_data().isNull())
                return NONE(unit);

            // Fetch the token kind, source location range and text from the
            // tokenNative reference.
            final TokenKind kind = TokenKind.fromC(
                NI_LIB.${nat("token_get_kind")}(tokenNative)
            );

            final SourceLocationRangeNative slocRangeNative =
                StackValue.get(SourceLocationRangeNative.class);
            NI_LIB.${nat("token_sloc_range")}(
                tokenNative,
                slocRangeNative
            );
            final SourceLocationRange slocRange =
                SourceLocationRange.wrap(slocRangeNative);

            final TextNative textNative = StackValue.get(TextNative.class);
            NI_LIB.${nat("token_range_text")}(
                tokenNative,
                tokenNative,
                textNative
            );
            final String text = Text.wrap(textNative).getContent();
            NI_LIB.${nat("destroy_text")}(textNative);

            // Wrap them in a high-level Token instance
            return new Token(
                new PointerWrapper(tokenNative.get_context()),
                unit,
                new PointerWrapper(tokenNative.get_data()),
                tokenNative.get_token_index(),
                tokenNative.get_trivia_index(),
                kind,
                text,
                slocRange
            );
        }

        /**
         * Unwrap the token in the given NI pointer.
         *
         * @param tokenNative The NI pointer to the native structure to
         * fill.
         */
        public void unwrap(
            final TokenNative tokenNative
        ) {
            tokenNative.set_context(this.contextRef.ni());
            tokenNative.set_data(this.tokenDataHandler.ni());
            tokenNative.set_token_index(this.tokenIndex);
            tokenNative.set_trivia_index(this.triviaIndex);
        }

        // ----- Getters -----

        public String getText() {
            return this.text;
        }

        public boolean isTrivia() {
            return this.triviaIndex != 0;
        }

        public boolean isNone() {
            return false;
        }

        // ----- Class methods -----

        /**
         * Get a none token for the given unit.
         *
         * @param unit The unit to get a none token for.
         * @return The none token for the given unit.
         */
        public static Token NONE(
            final AnalysisUnit unit
        ) {
            return NoToken.getInstance(unit);
        }

        /**
         * Get the text from the start token to the end token.
         *
         * @param start The start token.
         * @param end The end token.
         * @return The text between the two tokens.
         */
        @CompilerDirectives.TruffleBoundary
        public static String textRange(
            final Token start,
            final Token end
        ) {

            if(ImageInfo.inImageCode()) {
                final TokenNative startNative = StackValue.get(
                    TokenNative.class
                );
                start.unwrap(startNative);

                final TokenNative endNative = StackValue.get(
                    TokenNative.class
                );
                end.unwrap(endNative);

                final TextNative textNative = StackValue.get(
                    TextNative.class
                );
                Text.NONE.unwrap(textNative);
                NI_LIB.${nat("token_range_text")}(
                    startNative,
                    endNative,
                    textNative
                );
                try(final Text resText = Text.wrap(textNative)) {
                    return resText.getContent();
                }
            } else {
                try(
                    final Text resText = JNI_LIB.${nat("token_range_text")}(
                        start,
                        end
                    )
                ) {
                    return resText.getContent();
                }
            }

        }

        // ----- Instance methods -----

        /**
         * Get the next token.
         *
         * @return The next token in the source.
         */
        public Token next() {

            if(ImageInfo.inImageCode()) {
                final TokenNative tokenNative = StackValue.get(
                    TokenNative.class
                );
                this.unwrap(tokenNative);

                final TokenNative nextNative = StackValue.get(
                    TokenNative.class
                );
                NI_LIB.${nat("token_next")}(
                    tokenNative,
                    nextNative
                );
                return wrap(nextNative, this.unit);
            } else {
                return JNI_LIB.${nat("token_next")}(this);
            }

        }

        /**
         * Get the previous token.
         *
         * @return The previous token in the source.
         */
        public Token previous() {

            if(ImageInfo.inImageCode()) {
                final TokenNative tokenNative = StackValue.get(
                    TokenNative.class
                );
                this.unwrap(tokenNative);

                final TokenNative previousNative = StackValue.get(
                    TokenNative.class
                );
                NI_LIB.${nat("token_previous")}(
                    tokenNative,
                    previousNative
                );
                return wrap(previousNative, this.unit);
            } else {
                return JNI_LIB.${nat("token_previous")}(this);
            }

        }

        /**
         * Check of the token is equivalent to the other one.
         *
         * @param other The other token to compare with.
         */
        public boolean isEquivalent(
            final Token other
        ) {

            if(ImageInfo.inImageCode()) {
                final TokenNative leftNative = StackValue.get(
                    TokenNative.class
                );
                this.unwrap(leftNative);

                final TokenNative rightNative = StackValue.get(
                    TokenNative.class
                );
                other.unwrap(rightNative);

                return NI_LIB.${nat("token_is_equivalent")}(
                    leftNative,
                    rightNative
                ) != 0;
            } else {
                return JNI_LIB.${nat("token_is_equivalent")}(this, other);
            }

        }

        // ----- Override methods -----

        @Override
        @CompilerDirectives.TruffleBoundary
        public String toString() {
            return "<Token Kind="
                + this.kind.name
                + " Text=\""
                + stringRepresentation(this.getText())
                + "\">";
        }

        @Override
        public boolean equals(
            Object o
        ) {
            if(o == this) return true;
            if(!(o instanceof Token)) return false;
            final Token other = (Token) o;
            return this.tokenDataHandler.equals(other.tokenDataHandler) &&
                    this.tokenIndex == other.tokenIndex &&
                    this.triviaIndex == other.triviaIndex;
        }

        // ----- Inner classes -----

        /**
        * This class represents the absence of token.
        */
        private static final class NoToken extends Token {

            // ----- Class attributes -----

            /** The map of the no token instances */
            private static final Map<AnalysisUnit, NoToken> instances
                = new HashMap<>();

            // ----- Constructors -----

            /**
            * Create a new no token for the given analysis unit.
            * The constructor is private to avoid too many instances.
            *
            * @param unit The analysis unit to create the no token for.
            */
            private NoToken(
                final PointerWrapper contextRef,
                final AnalysisUnit unit
            ) {
                super(
                    contextRef,
                    unit,
                    PointerWrapper.nullPointer(),
                    0,
                    0,
                    TokenKind.fromC(-1),
                    null,
                    SourceLocationRange.NONE
                );
            }

            /**
            * Get the no token instance for the given analysis unit.
            *
            * @param unit The unit to get the no token instance for.
            * @return The no token instance.
            */
            static NoToken getInstance(
                final AnalysisUnit unit
            ) {
                if(!instances.containsKey(unit)) {
                    try(AnalysisContext context = unit.getContext()) {
                        instances.put(
                            unit,
                            new NoToken(context.reference, unit)
                        );
                    }
                }
                return instances.get(unit);
            }

            // ----- Getters -----

            @Override
            public String getText() {
                return "";
            }

            // ----- Instance methods -----

            @Override
            public Token next() {
                return this;
            }

            @Override
            public Token previous() {
                return this;
            }

            @Override
            public boolean isEquivalent(
                final Token other
            ) {
                return other instanceof NoToken;
            }

            @Override
            public boolean isNone() {
                return true;
            }

            @Override
            public void unwrap(
                final TokenNative tokenNative
            ) {
                tokenNative.set_context(this.contextRef.ni());
                tokenNative.set_data(this.tokenDataHandler.ni());
                tokenNative.set_token_index(this.tokenIndex);
                tokenNative.set_trivia_index(this.triviaIndex);
            }

            // ----- Override methods -----

            @Override
            @CompilerDirectives.TruffleBoundary
            public String toString() {
                return "<Token Kind="
                    + this.kind.name
                    + " Text=\"\">";
            }

            @Override
            public boolean equals(
                Object o
            ) {
                return o == this;
            }

        }

    }

    ${java_doc('langkit.analysis_context_type', 4)}
    public static final
    class AnalysisContext extends LangkitSupport.AnalysisContext {

        // ----- Class attributes -----

        /** Singleton that represents the none analysis context. */
        public static final AnalysisContext NONE = new AnalysisContext(
            PointerWrapper.nullPointer(),
            null
        );

        /** This map contains all created analysis contexts. */
        private static final Map<PointerWrapper, AnalysisContext> contextCache
            = new ConcurrentHashMap<>();

        // ----- Instance attributes -----

        /** The reference to the native analysis context. */
        private final PointerWrapper reference;

        /** The event handler associated with the context. */
        private EventHandler eventHandler;

        % if ctx.generate_unparsers:
        /**
         * The rewriting context associated with this analysis context.
         * It can be the none value.
         */
        RewritingContext rewritingContext = RewritingContext.NONE;
        % endif

        // ----- Constructors -----

        /**
         * Create a new analysis unit from its reference.
         *
         * @param reference The native analysis context.
         * @param eventHandler The associated event handler.
         */
        private AnalysisContext(
            final PointerWrapper reference,
            final EventHandler eventHandler
        ) {
            this.reference = reference;
            this.eventHandler = eventHandler;

            increaseRefCounter(this);
        }

        /**
         * Create a new analysis context from scratch with its configuration.
         *
         * @param charset The charset for the analysis context, it can be null.
         * @param fileReader The file reader for the analysis context, it
         * can be null.
         * @param unitProvider The unit provider for the analysis context,
         * it can be null.
         * @param eventHandler The event handler for the analysis context,
         * it can be null.
         * @param withTrivia If the analysis context should include trivias.
         * @param tabStop The effect of the tabulations on the column number.
         */
        private AnalysisContext(
            final String charset,
            final FileReader fileReader,
            final UnitProvider unitProvider,
            final EventHandler eventHandler,
            final boolean withTrivia,
            final int tabStop
        ) {
            // Call the function to allocate the native analysis context
            final PointerWrapper reference;
            if(ImageInfo.inImageCode()) {
                reference = new PointerWrapper(
                    NI_LIB.${nat("allocate_analysis_context")}()
                );
            } else {
                reference = JNI_LIB.${nat("create_analysis_context")}(
                    charset,
                    (fileReader == null ?
                        FileReader.NONE :
                        fileReader),
                    (unitProvider == null ?
                        UnitProvider.NONE :
                        unitProvider),
                    (eventHandler == null ?
                        EventHandler.NONE :
                        eventHandler),
                    withTrivia,
                    tabStop
                );
            }

            // Place the context in the cache for potention callbacks during
            // context initialization.
            this.reference = reference;
            this.eventHandler = eventHandler;
            contextCache.put(this.reference, this);

            // Perform the context initialization
            if(ImageInfo.inImageCode()) {
                final CCharPointer charsetNative =
                    charset == null ?
                    WordFactory.nullPointer() :
                    toCString(charset);

                NI_LIB.${nat("initialize_analysis_context")}(
                    (AnalysisContextNative) reference.ni(),
                    charsetNative,
                    (fileReader == null ?
                        WordFactory.nullPointer() :
                        fileReader.reference.ni()),
                    (unitProvider == null ?
                        WordFactory.nullPointer() :
                        unitProvider.reference.ni()),
                    (eventHandler == null ?
                        WordFactory.nullPointer() :
                        eventHandler.reference.ni()),
                    (withTrivia ? 1 : 0),
                    tabStop
                );
                if(charset != null) UnmanagedMemory.free(charsetNative);
            }
        }

        /**
         * Create a new analysis context with the default parameters.
         *
         * @return The newly create analysis unit.
         */
        public static AnalysisContext create() {
            return new AnalysisContext(
                null,
                null,
                null,
                null,
                true,
                8
            );
        }

        /**
         * Create an analysis context with its parameters.
         *
         * @param charset The charset for the analysis context, it can be null.
         * @param fileReader The file reader for the analysis context, it
         * can be null.
         * @param unitProvider The unit provider for the analysis context,
         * it can be null.
         * @param eventHandler The event handler for the analysis context,
         * it can be null.
         * @param withTrivia If the analysis context should include trivias.
         * @param tabStop The effect of the tabulations on the column number.
         * @return The newly create analysis unit.
         */
        public static AnalysisContext create(
            final String charset,
            final FileReader fileReader,
            final UnitProvider unitProvider,
            final EventHandler eventHandler,
            final boolean withTrivia,
            final int tabStop
        ) {
            return new AnalysisContext(
                charset,
                fileReader,
                unitProvider,
                eventHandler,
                withTrivia,
                tabStop
            );
        }

        /**
         * Get the analysis context object from its reference.
         *
         * @param reference The native reference to the analysis context.
         * @param eventHandler The corresponding event handler.
         * @param setEventHandler Whether to set the result's eventHandler
         * field to eventHandler when there is already a cached
         * AnalysisContext.
         * @return The Java analysis unit associated with the reference.
         */
        static AnalysisContext fromReference(
            final PointerWrapper reference,
            final EventHandler eventHandler,
            final boolean setEventHandler
        ) {
            if(contextCache.containsKey(reference)) {
                final AnalysisContext res = contextCache.get(reference);
                increaseRefCounter(res);
                if(setEventHandler)
                    res.eventHandler = eventHandler;
                return res;
            } else {
                final AnalysisContext result =
                    new AnalysisContext(reference, eventHandler);
                contextCache.put(reference, result);
                return result;
            }
        }

        /**
         * Get the analysis context object from its reference.
         *
         * @param reference The native reference to the analysis context.
         * @return The Java analysis unit associated with the reference.
         */
        static AnalysisContext fromReference(
            final PointerWrapper reference
        ) {
            return fromReference(reference, null, false);
        }

        // ----- Graal C API methods -----

        /**
         * Wrap a native pointer to the native analysis context in the
         * Java class.
         *
         * @param pointer The pointer to the NI analysis context
         * native value.
         * @return The newly wrapped analysis context.
         */
        static AnalysisContext wrap(
            final WordPointer pointer
        ) {
            return wrap((AnalysisContextNative) pointer.read());
        }

        /**
         * Wrap an analysis context native value in the Java class.
         *
         * @param analysisContextNative The NI analysis context native value.
         * @return The newly wrapped analysis context.
         */
        static AnalysisContext wrap(
            final AnalysisContextNative analysisContextNative
        ) {
            return fromReference(new PointerWrapper(analysisContextNative));
        }

        /**
         * Unwrap the analysis context in the given native pointer.
         *
         * @param pointer The pointer to place the native analysis unit.
         */
        void unwrap(
            final WordPointer pointer
        ) {
            pointer.write(this.unwrap());
        }

        /**
         * Get the native value of the analysis context.
         *
         * @return The native analysis context.
         */
        AnalysisContextNative unwrap() {
            return (AnalysisContextNative) this.reference.ni();
        }

        // ----- Getters -----

        public EventHandler getEventHandler() {
            return this.eventHandler;
        }

        % if ctx.generate_unparsers:
        /**
         * Get the currently open rewriting context associated to this
         * analysis context. The None rewriting context is returned if the
         * current context hasn't started a rewriting session.
         * @see AnalysisContext#startRewriting()
         */
        public RewritingContext getRewritingContext() {
            return this.rewritingContext;
        }
        % endif

        // ----- Class methods -----

        /**
         * Increase the reference counter of the given context.
         *
         * @param context The context to increase the reference counter of.
         */
        private static void increaseRefCounter(
            final AnalysisContext context
        ) {
            // Increase the context reference counter of the context if not null
            if(!context.reference.isNull()) {
                if(ImageInfo.inImageCode()) {
                    NI_LIB.${nat("context_incref")}(context.reference.ni());
                } else {
                    JNI_LIB.${nat("context_incref")}(context.reference.jni());
                }
            }
        }

        // ----- Instance methods -----

        /**
         * Get an analysis unit from the given file in the current context.
         *
         * @param fileName The file to get the analysis unit from.
         * @return The new analysis unit.
         */
        public AnalysisUnit getUnitFromFile(
            final String fileName
        ) {
            return this.getUnitFromFile(
                fileName,
                null,
                false,
                DEFAULT_GRAMMAR_RULE
            );
        }

        /**
         * Get an analysis unit from the given file in the current context
         * with additional parameters.
         *
         * @param fileName The file to get the analysis unit from.
         * @param charset The charset of the given file.
         * @return The new analysis unit.
         */
        public AnalysisUnit getUnitFromFile(
            final String fileName,
            final String charset
        ) {
            return this.getUnitFromFile(
                fileName,
                charset,
                false,
                DEFAULT_GRAMMAR_RULE
            );
        }

        /**
         * Get an analysis unit from the given file in the current context
         * with additional parameters.
         *
         * @param fileName The file to get the analysis unit from.
         * @param charset The charset of the given file.
         * @param reparse If the file should be reparsed.
         * @return The new analysis unit.
         */
        public AnalysisUnit getUnitFromFile(
            final String fileName,
            final String charset,
            final boolean reparse
        ) {
            return this.getUnitFromFile(
                fileName,
                charset,
                reparse,
                DEFAULT_GRAMMAR_RULE
            );
        }

        /**
         * Get an analysis unit from the given file in the current context
         * with additional parameters.
         *
         * @param fileName The file to get the analysis unit from.
         * @param charset The charset of the given file.
         * @param reparse If the file should be reparsed.
         * @param rule The grammar rule to parse the source with.
         * @return The new analysis unit.
         */
        public AnalysisUnit getUnitFromFile(
            final String fileName,
            final String charset,
            final boolean reparse,
            final GrammarRule rule
        ) {

            if(ImageInfo.inImageCode()) {
                final CCharPointer fileNameNative = toCString(fileName);
                final CCharPointer charsetNative =
                    charset == null ?
                    WordFactory.nullPointer() :
                    toCString(charset);

                final AnalysisUnitNative resNative =
                    NI_LIB.${nat("get_analysis_unit_from_file")}(
                    this.reference.ni(),
                    fileNameNative,
                    charsetNative,
                    (reparse ? 1 : 0),
                    rule.toC()
                );
                UnmanagedMemory.free(fileNameNative);
                if(charset != null) UnmanagedMemory.free(charsetNative);
                return AnalysisUnit.wrap(resNative);
            } else {
                return JNI_LIB.${nat("get_analysis_unit_from_file")}(
                    this,
                    fileName,
                    charset,
                    reparse,
                    rule.toC()
                );
            }

        }

        /**
         * Get an analysis unit from the given buffer in the current context.
         *
         * @param buffer The buffer to parse.
         * @param name The name of the buffer.
         * @return The new analysis unit.
         */
        public AnalysisUnit getUnitFromBuffer(
            final String buffer,
            final String name
        ) {
            return this.getUnitFromBuffer(
                buffer,
                name,
                null,
                DEFAULT_GRAMMAR_RULE
            );
        }

        /**
         * Get an analysis unit from the given buffer in the current context
         * with additional parameters.
         *
         * @param buffer The buffer to parse.
         * @param name The name of the buffer.
         * @param charset The charset of the buffer.
         * @param rule The rule to parse the buffer with.
         * @return The new analysis unit.
         */
        public AnalysisUnit getUnitFromBuffer(
            final String buffer,
            final String name,
            final String charset,
            final GrammarRule rule
        ) {

            if(ImageInfo.inImageCode()) {
                final CCharPointer bufferNative = toCString(buffer);
                final CCharPointer nameNative = toCString(name);
                final CCharPointer charsetNative =
                    charset == null ?
                    WordFactory.nullPointer() :
                    toCString(charset);

                final AnalysisUnitNative resNative =
                    NI_LIB.${nat("get_analysis_unit_from_buffer")}(
                    this.reference.ni(),
                    nameNative,
                    charsetNative,
                    bufferNative,
                    buffer.length(),
                    rule.toC()
                );
                UnmanagedMemory.free(bufferNative);
                UnmanagedMemory.free(nameNative);
                if(charset != null) UnmanagedMemory.free(charsetNative);
                return AnalysisUnit.wrap(resNative);
            } else {
                return JNI_LIB.${nat("get_analysis_unit_from_buffer")}(
                    this,
                    name,
                    charset,
                    buffer,
                    buffer.length(),
                    rule.toC()
                );
            }

        }

        % if cfg.library.defaults.unit_provider:
        /**
         * Get an analysis unit from the given unit name and unit kind in the
         * current context with additional parameters.
         *
         * @param name Name of the unit.
         * @param kind Kind of the unit.
         * @param charset The charset of the buffer.
         * @param rule The rule to parse the buffer with.
         * @return The new analysis unit.
         */
        public AnalysisUnit getUnitFromProvider(
            final Text name,
            final AnalysisUnitKind kind,
            final String charset,
            final boolean reparse
        ) {
            if(ImageInfo.inImageCode()) {
                TextNative nameNative = StackValue.get(TextNative.class);
                name.unwrap(nameNative);
                final CCharPointer charsetNative =
                    charset == null ?
                    WordFactory.nullPointer() :
                    toCString(charset);
                final AnalysisUnitNative resNative =
                    NI_LIB.${nat("get_analysis_unit_from_provider")}(
                    this.reference.ni(),
                    nameNative,
                    kind.toC(),
                    charsetNative,
                    (reparse ? 1 : 0)
                );
                if(charset != null) UnmanagedMemory.free(charsetNative);
                return AnalysisUnit.wrap(resNative);
            } else {
                return JNI_LIB.${nat("get_analysis_unit_from_provider")}(
                    this,
                    name,
                    kind.toC(),
                    charset,
                    reparse
                );
            }
        }

        public AnalysisUnit getUnitFromProvider(
            final Text name,
            final AnalysisUnitKind kind
        ) {
            return this.getUnitFromProvider(name, kind, "", false);
        }
        % endif

        % if ctx.generate_unparsers:
        ${java_doc('langkit.rewriting.start_rewriting', 8)}
        public RewritingContext startRewriting() {
            final RewritingContext res;

            if(ImageInfo.inImageCode()) {
                final RewritingContextNative resNative =
                    NI_LIB.${nat("rewriting_start_rewriting")}(
                        this.reference.ni()
                    );
                res = RewritingContext.wrap(resNative);
            } else {
                res = JNI_LIB.${nat("rewriting_start_rewriting")}(this);
            }

            checkException();
            this.rewritingContext = res;
            return this.rewritingContext;
        }
        % endif

        /** @see java.lang.AutoCloseable#close() */
        @Override
        public void close() {

            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("context_decref")}(this.reference.ni());
            } else {
                JNI_LIB.${nat("context_decref")}(this.reference.jni());
            }
            checkException();

        }

        ${exts.include_extension(
            ctx.ext("java_api", "analysis_context_class")
        )}

    }

    ${java_doc('langkit.analysis_unit_type', 4)}
    public static final class AnalysisUnit extends LangkitSupport.AnalysisUnit {

        // ----- Class attributes -----

        /** Singleton that represents the none analysis unit. */
        public static final AnalysisUnit NONE = new AnalysisUnit(
            PointerWrapper.nullPointer()
        );

        // ----- Instance attributes -----

        /** The reference to the native analysis unit. */
        private final PointerWrapper reference;

        /** The cache for the unit root. */
        private ${root_node_type} root;

        // ----- Constructors -----

        /**
         * Create a new analysis unit from its value.
         *
         * @param reference The native analysis unit native value in
         * a pointer wrapper.
         */
        AnalysisUnit(
            final PointerWrapper reference
        ) {
            this.reference = reference;
        }

        // ----- Graal C API methods -----

        /**
         * Wrap a pointer to the native analysis unit in the Java class.
         *
         * @param pointer The pointer the native analysis unit value.
         * @return The newly wrapped analysis unit.
         */
        static AnalysisUnit wrap(
            final WordPointer pointer
        ) {
            return wrap((AnalysisUnitNative) pointer.read());
        }

        /**
         * Wrap a NI analysis unit native value in the Java class.
         *
         * @param analysisUnitNative The NI analysis unit native value.
         * @return The newly wrapped analysis unit.
         */
        static AnalysisUnit wrap(
            final AnalysisUnitNative analysisUnitNative
        ) {
            return new AnalysisUnit(new PointerWrapper(analysisUnitNative));
        }

        /**
         * Unwrap the analysis unit in the given pointer.
         *
         * @param pointer The pointer to place the native analysis unit in.
         */
        void unwrap(
            final WordPointer pointer
        ) {
            pointer.write(this.unwrap());
        }

        /**
         * Unwrap the analysis unit as a native value.
         *
         * @return The native analysis unit.
         */
        AnalysisUnitNative unwrap() {
            return (AnalysisUnitNative) this.reference.ni();
        }

        // ----- Instance methods -----

        /**
         * Get the root node of the analysis unit.
         *
         * @return The root node.
         */
        public ${root_node_type} getRoot() {
            if(this.root == null) {

                if(ImageInfo.inImageCode()) {
                    final EntityNative entityNative = StackValue.get(
                        EntityNative.class
                    );
                    NI_LIB.${nat("unit_root")}(
                        this.reference.ni(),
                        entityNative
                    );
                    this.root = ${root_node_type}.fromEntity(
                        Entity.wrap(entityNative)
                    );
                } else {
                    this.root = ${root_node_type}.fromEntity(
                        JNI_LIB.${nat("unit_root")}(this)
                    );
                }

            }
            return this.root;
        }

        /**
         * Get the analysis unit file name with its full path.
         *
         * @return The unit file name.
         */
        public String getFileName() {
            return this.getFileName(true);
        }

        /**
         * Get the unit's file name.
         *
         * @param fullPath If the method should return the
         * file full absolute path.
         * @return The file name.
         */
        @CompilerDirectives.TruffleBoundary
        public String getFileName(
            final boolean fullPath
        ) {
            final String absoluteFile;

            if(ImageInfo.inImageCode()) {
                final CCharPointer resNative = NI_LIB.${nat("unit_filename")}(
                    this.reference.ni()
                );
                absoluteFile = toJString(resNative);
                NI_LIB.${nat("free")}(resNative);
            } else {
                absoluteFile = JNI_LIB.${nat("unit_filename")}(this);
            }

            if(fullPath) {
                return absoluteFile;
            } else {
                return new File(absoluteFile).getName();
            }
        }

        /**
         * Get the number of tokens in the analysis unit.
         *
         * @return The number of token.
         */
        public int getTokenCount() {

            if(ImageInfo.inImageCode()) {
                return NI_LIB.${nat("unit_token_count")}(this.reference.ni());
            } else {
                return JNI_LIB.${nat("unit_token_count")}(this);
            }

        }

        /**
         * Get the number of trivia in the analysis unit.
         *
         * @return The number of trivia.
         */
        public int getTriviaCount() {

            if(ImageInfo.inImageCode()) {
                return NI_LIB.${nat("unit_trivia_count")}(
                    this.reference.ni()
                );
            } else {
                return JNI_LIB.${nat("unit_trivia_count")}(this);
            }

        }

        /**
         * Return the first token of the analysis unit.
         *
         * @return The first token.
         */
        public Token getFirstToken() {

            if(ImageInfo.inImageCode()) {
                final TokenNative tokenNative = StackValue.get(
                    TokenNative.class
                );
                NI_LIB.${nat("unit_first_token")}(
                    this.reference.ni(),
                    tokenNative
                );
                return Token.wrap(tokenNative, this);
            } else {
                return JNI_LIB.${nat("unit_first_token")}(this);
            }

        }

        /**
         * Return the last token of the analysis unit.
         *
         * @return The last token.
         */
        public Token getLastToken() {

            if(ImageInfo.inImageCode()) {
                final TokenNative tokenNative = StackValue.get(
                    TokenNative.class
                );
                NI_LIB.${nat("unit_last_token")}(
                    this.reference.ni(),
                    tokenNative
                );
                return Token.wrap(tokenNative, this);
            } else {
                return JNI_LIB.${nat("unit_last_token")}(this);
            }

        }

        /**
         * Get the text of the analysis unit in a string.
         *
         * @return The text of the analysis unit source.
         */
        public String getText() {
            return Token.textRange(
                this.getFirstToken(),
                this.getLastToken()
            );
        }

        /**
         * Get the analysis context that owns the unit.
         *
         * @return The owning analysis context.
         */
        public AnalysisContext getContext() {

            if(ImageInfo.inImageCode()) {
                final AnalysisContextNative contextNative =
                    NI_LIB.${nat("unit_context")}(this.reference.ni());
                return AnalysisContext.wrap(contextNative);
            } else {
                return JNI_LIB.${nat("unit_context")}(this);
            }

        }

        /**
         * Get the list of associated diagnostics. Those are parsing errors.
         *
         * @return The diagnostics of the unit.
         */
        public Diagnostic[] getDiagnostics() {
            final int diagnosticCount;

            if(ImageInfo.inImageCode()) {
                diagnosticCount = NI_LIB.${nat("unit_diagnostic_count")}(
                    this.reference.ni()
                );
            } else {
                diagnosticCount = JNI_LIB.${nat("unit_diagnostic_count")}(
                    this
                );
            }

            Diagnostic[] res = new Diagnostic[diagnosticCount];

            if(ImageInfo.inImageCode()) {
                final DiagnosticNative diagnosticNative = StackValue.get(
                    DiagnosticNative.class
                );
                for(int i = 0 ; i < diagnosticCount ; i++) {
                    NI_LIB.${nat("unit_diagnostic")}(
                        this.reference.ni(),
                        i,
                        diagnosticNative
                    );
                    res[i] = Diagnostic.wrap(diagnosticNative);
                }
            } else {
                for(int i = 0 ; i < diagnosticCount ; i++) {
                    res[i] = JNI_LIB.${nat("unit_diagnostic")}(this, i);
                }
            }

            return res;
        }

        % if ctx.generate_unparsers:
        ${java_doc('langkit.rewriting.unit_handle', 8)}
        public RewritingUnit getRewritingUnit() {
            final RewritingUnit res;

            if(ImageInfo.inImageCode()) {
                RewritingUnitNative rewritingUnitNative =
                    NI_LIB.${nat("rewriting_unit_to_handle")}(
                        this.unwrap()
                    );
                res = RewritingUnit.wrap(rewritingUnitNative);
            } else {
                res = JNI_LIB.${nat("rewriting_unit_to_handle")}(this);
            }

            checkException();
            return res;
        }
        % endif

        // ----- Override methods -----

        @Override
        public String toString() {
            return "<AnalysisUnit \"" + this.getFileName(false) + "\">";
        }

        @Override
        public boolean equals(Object o) {
            if(this == o) return true;
            if(!(o instanceof AnalysisUnit)) return false;
            final AnalysisUnit other = (AnalysisUnit) o;
            return this.reference.equals(other.reference);
        }

    }

    % if ctx.generate_unparsers:
    ## Generate this part only if the unparser is required
    ${java_doc('langkit.rewriting.rewriting_handle_type', 4)}
    public static final class RewritingContext implements AutoCloseable {

        // ----- Class attributes -----

        /** Singleton representing the none rewriting context */
        public static final RewritingContext NONE = new RewritingContext(
            PointerWrapper.nullPointer()
        );

        /**
         * This map contains all wrapped rewriting context associated to their
         * address.
         */
        private static final Map<PointerWrapper, RewritingContext> contextCache
            = new ConcurrentHashMap<>();

        // ----- Instance attributes -----

        /** The reference to the native rewriting context handle. */
        private final PointerWrapper reference;

        /**
         * Cache for the analysis context associated with this rewriting
         * context.
         */
        private AnalysisContext analysisContext;

        /** Whether the rewriting context has already been closed. */
        private boolean closed;

        // ----- Constructors -----

        /** Create a new rewriting context with its native reference. */
        private RewritingContext(
            final PointerWrapper reference
        ) {
            this.reference = reference;
            this.closed = false;
        }

        /**
         * From the given native reference, get the associate rewriting
         * context Java object. A native reference can only have one
         * associated Java instance.
         */
        @CompilerDirectives.TruffleBoundary
        static RewritingContext fromReference(
            final PointerWrapper reference
        ) {
            if(!contextCache.containsKey(reference)) {
                contextCache.put(
                    reference,
                    new RewritingContext(reference)
                );
            }
            final RewritingContext res = contextCache.get(reference);
            res.closed = false;
            return res;
        }

        // ----- Graal C API methods -----

        /** Wrap a native pointer to a native rewriting context. */
        static RewritingContext wrap(
            final Pointer pointer
        ) {
            return wrap((RewritingContextNative) pointer.readWord(0));
        }

        /** Wrap a native rewriting context. */
        static RewritingContext wrap(
            final RewritingContextNative rewritingContextNative
        ) {
            return fromReference(new PointerWrapper(rewritingContextNative));
        }

        /** Unwrap the analysis context in the given native pointer. */
        void unwrap(
            final Pointer pointer
        ) {
            pointer.writeWord(0, this.unwrap());
        }

        /** Unwrap the rewriting context as a native value. */
        RewritingContextNative unwrap() {
            return (RewritingContextNative) this.reference.ni();
        }

        // ----- Getters -----

        public boolean isClosed() {
            return this.closed;
        }


        ${java_doc('langkit.rewriting.handle_context', 8)}
        public AnalysisContext getAnalysisContext() {
            if(this.analysisContext == null) {

                if(ImageInfo.inImageCode()) {
                    this.analysisContext = AnalysisContext.wrap(
                        NI_LIB.${nat("rewriting_handle_to_context")}(
                            this.unwrap()
                        )
                    );
                } else {
                    this.analysisContext =
                        JNI_LIB.${nat("rewriting_handle_to_context")}(this);
                }

            }
            return this.analysisContext;
        }

        // ----- Instance methods -----

        ${java_doc('langkit.rewriting.unit_handles', 8)}
        public RewritingUnit[] rewritingUnits() {

            if(ImageInfo.inImageCode()) {
                // Get the native array
                final WordPointer unitArrayNative =
                    NI_LIB.${nat("rewriting_unit_handles")}(this.unwrap());

                // Fill the Java result list
                final List<RewritingUnit> resList = new ArrayList<>();
                int cursor = 0;
                while(((Pointer) unitArrayNative.read(cursor)).isNonNull()) {
                    resList.add(RewritingUnit.wrap(
                        (RewritingUnitNative) unitArrayNative.read(cursor)
                    ));
                    cursor++;
                }

                // Free the native array
                NI_LIB.${nat("free")}(unitArrayNative);

                // Return the Java list as an array
                return resList.toArray(new RewritingUnit[0]);
            } else {
                return JNI_LIB.${nat("rewriting_unit_handles")}(this);
            }

        }

        ${java_doc('langkit.rewriting.create_node', 8)}
        public RewritingNode createNode(
            final NodeKind kind
        ) {
            final RewritingNode res;

            if(ImageInfo.inImageCode()) {
                res = RewritingNode.wrap(
                    NI_LIB.${nat("rewriting_create_node")}(
                        this.unwrap(),
                        kind.toC()
                    )
                );
            } else {
                res = JNI_LIB.${nat("rewriting_create_node")}(
                    this,
                    kind.toC()
                );
            }

            checkException();
            return res;
        }

        ${java_doc('langkit.rewriting.create_regular_node', 8)}
        public RewritingNode createNode(
            final NodeKind kind,
            final RewritingNode... children
        ) {
            final RewritingNode res;

            if(ImageInfo.inImageCode()) {
                final WordPointer childrenNative =
                    rewritingNodesToNative(children);
                res = RewritingNode.wrap(
                    NI_LIB.${nat("rewriting_create_regular_node")}(
                        this.unwrap(),
                        kind.toC(),
                        childrenNative,
                        children.length
                    )
                );
                UnmanagedMemory.free(childrenNative);
            } else {
                res = JNI_LIB.${nat("rewriting_create_regular_node")}(
                    this,
                    kind.toC(),
                    children
                );
            }

            checkException();
            return res;
        }

        ${java_doc('langkit.rewriting.create_token_node', 8)}
        public RewritingNode createTokenNode(
            final NodeKind kind,
            final String text
        ) {
            try (
                final Text nodeText = Text.create(text);
            ) {
                final RewritingNode res;

                if(ImageInfo.inImageCode()) {
                    final TextNative nodeTextNative = StackValue.get(
                        TextNative.class
                    );
                    nodeText.unwrap(nodeTextNative);
                    res = RewritingNode.wrap(
                        NI_LIB.${nat("rewriting_create_token_node")}(
                            this.unwrap(),
                            kind.toC(),
                            nodeTextNative
                        )
                    );
                } else {
                    res = JNI_LIB.${nat("rewriting_create_token_node")}(
                        this,
                        kind.toC(),
                        nodeText
                    );
                }

                checkException();
                return res;
            }
        }

        ${java_doc('langkit.rewriting.create_from_template', 8)}
        public RewritingNode createFromTemplate(
            final String template,
            final GrammarRule rule,
            final RewritingNode... arguments
        ) {
            try (
                final Text templateText = Text.create(template);
            ) {
                final RewritingNode res;

                if(ImageInfo.inImageCode()) {
                    final WordPointer argumentsNative =
                        rewritingNodesToNative(arguments);
                    TextNative templateTextNative =
                        StackValue.get(TextNative.class);
                    templateText.unwrap(templateTextNative);
                    res = RewritingNode.wrap(
                        NI_LIB.${nat("rewriting_create_from_template")}(
                            this.unwrap(),
                            templateTextNative,
                            argumentsNative,
                            arguments.length,
                            rule.toC()
                        )
                    );
                    UnmanagedMemory.free(argumentsNative);
                } else {
                    res = JNI_LIB.${nat("rewriting_create_from_template")}(
                        this,
                        templateText,
                        arguments,
                        rule.toC()
                    );
                }

                checkException();
                return res;
            }
        }

        ${java_doc('langkit.rewriting.apply', 8)}
        public RewritingApplyResult apply() {
            final RewritingApplyResult res;

            if(ImageInfo.inImageCode()) {
                RewritingApplyResultNative resNative = StackValue.get(
                    RewritingApplyResultNative.class
                );
                NI_LIB.${nat("rewriting_apply")}(
                    this.unwrap(),
                    resNative
                );
                res = RewritingApplyResult.wrap(resNative);
            } else {
                res = JNI_LIB.${nat("rewriting_apply")}(this);
            }

            this.closed = res.success;
            return res;
        }

        ${java_doc('langkit.rewriting.abort_rewriting', 8)}
        public void close() {
            if(!this.closed) {
                if(this.analysisContext != null) {
                    this.analysisContext.close();
                    this.analysisContext = null;
                }

                if(ImageInfo.inImageCode()) {
                    NI_LIB.${nat("rewriting_abort_rewriting")}(this.unwrap());
                } else {
                    JNI_LIB.${nat("rewriting_abort_rewriting")}(this);
                }

                checkException();
                this.closed = true;
            }
        }

    }

    ${java_doc('langkit.rewriting.unit_rewriting_handle_type', 4)}
    public static final class RewritingUnit {

        // ----- Class methods -----

        /** Singleton representing the none value for the rewriting unit. */
        public static final RewritingUnit NONE = new RewritingUnit(
            PointerWrapper.nullPointer()
        );

        // ----- Instance methods -----

        /** Reference to the native rewriting unit. */
        private final PointerWrapper reference;

        /** Cache for the analysis unit associated to the rewriting unit. */
        private AnalysisUnit analysisUnit;

        /** Cache for the unit root rewriting node. */
        private RewritingNode root;

        // ----- Constructors -----

        /** Create a new rewriting unit with its native reference. */
        RewritingUnit(
            final PointerWrapper reference
        ) {
            this.reference = reference;
        }

        // ----- Graal C API methods -----

        /** Wrap the given pointer to the native rewriting unit. */
        static RewritingUnit wrap(
            final Pointer pointer
        ) {
            return wrap((RewritingUnitNative) pointer.readWord(0));
        }

        /** Wrap the native rewriting unit. */
        static RewritingUnit wrap(
            final RewritingUnitNative rewritingUnitNative
        ) {
            return new RewritingUnit(new PointerWrapper(rewritingUnitNative));
        }

        /** Unwrap the rewriting unit in the given pointer. */
        void unwrap(
            final Pointer pointer
        ) {
            pointer.writeWord(0, this.unwrap());
        }

        /** Unwrap the rewriting unit and return its native value. */
        RewritingUnitNative unwrap() {
            return (RewritingUnitNative) this.reference.ni();
        }

        // ----- Getters -----

        ${java_doc('langkit.rewriting.handle_unit', 8)}
        public AnalysisUnit getAnalysisUnit() {
            if(this.analysisUnit == null) {

                if(ImageInfo.inImageCode()) {
                    this.analysisUnit = AnalysisUnit.wrap(
                        NI_LIB.${nat("rewriting_handle_to_unit")}(
                            this.unwrap()
                        )
                    );
                } else {
                    this.analysisUnit =
                        JNI_LIB.${nat("rewriting_handle_to_unit")}(this);
                }

            }
            return this.analysisUnit;
        }

        ${java_doc('langkit.rewriting.root', 8)}
        public RewritingNode getRoot() {
            if(this.root == null) {

                if(ImageInfo.inImageCode()) {
                    this.root = RewritingNode.wrap(
                        NI_LIB.${nat("rewriting_unit_root")}(this.unwrap())
                    );
                } else {
                    this.root = JNI_LIB.${nat("rewriting_unit_root")}(this);
                }

            }
            return this.root;
        }

        // ----- Setters -----

        ${java_doc('langkit.rewriting.set_root', 8)}
        public void setRoot(
            final RewritingNode root
        ) {
            this.root = root;

            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("rewriting_unit_set_root")}(
                    this.unwrap(),
                    root.unwrap()
                );
            } else {
                JNI_LIB.${nat("rewriting_unit_set_root")}(this, root);
            }
        }

        // ----- Instance methods -----

        ${java_doc('langkit.rewriting.unit_unparse', 8)}
        public String unparse() {
            final Text unparseText;

            if(ImageInfo.inImageCode()) {
                final TextNative textNative = StackValue.get(
                    TextNative.class
                );
                NI_LIB.${nat("rewriting_unit_unparse")}(
                    this.unwrap(),
                    textNative
                );
                unparseText = Text.wrap(textNative);
            } else {
                unparseText = JNI_LIB.${nat("rewriting_unit_unparse")}(this);
            }

            String res = unparseText.getContent();
            unparseText.close();
            return res;
        }

        // ----- Override methods -----

        @Override
        public boolean equals(Object o) {
            if(this == o) return true;
            if(!(o instanceof RewritingUnit)) return false;
            final RewritingUnit other = (RewritingUnit) o;
            return this.reference.equals(other.reference);
        }

    }

    ${java_doc('langkit.rewriting.node_rewriting_handle_type', 4)}
    public static final class RewritingNode {

        // ----- Class attributes -----

        /** Singleton representing the none value for rewriting node. */
        public static final RewritingNode NONE = new RewritingNode(
            PointerWrapper.nullPointer()
        );

        // ----- Instance attributes -----

        /** The reference to the native rewriting node. */
        private final PointerWrapper reference;

        /** Kind of the rewriting node. */
        private NodeKind kind;

        /** Cache for the associated parsed node. */
        private ${root_node_type} parsedNode;

        /** Cache for the rewriting context containing this node. */
        private RewritingContext rewritingContext;

        // ----- Constructors -----

        /** Create a new rewriting node with its native reference. */
        RewritingNode(
            final PointerWrapper reference
        ) {
            this.reference = reference;
        }

        // ----- Graal C API -----

        /** Wrap a pointer to a native rewriting node. */
        static RewritingNode wrap(
            final Pointer pointer
        ) {
            return wrap((RewritingNodeNative) pointer.readWord(0));
        }

        /** Wrap the native rewriting node. */
        static RewritingNode wrap(
            final RewritingNodeNative rewritingNodeNative
        ) {
            return new RewritingNode(new PointerWrapper(rewritingNodeNative));
        }

        /** Unwrap the rewriting node in the given pointer. */
        void unwrap(
            final Pointer pointer
        ) {
            pointer.writeWord(0, this.unwrap());
        }

        /** Unwrap the rewriting node and return its native value. */
        RewritingNodeNative unwrap() {
            return (RewritingNodeNative) this.reference.ni();
        }

        // ----- Getters -----

        ${java_doc('langkit.rewriting.kind',  8)}
        public NodeKind getKind() {
            if(this.kind == null) {
                final int kindNative;

                if(ImageInfo.inImageCode()) {
                    kindNative = NI_LIB.${nat("rewriting_kind")}(
                        this.unwrap()
                    );
                } else {
                    kindNative = JNI_LIB.${nat("rewriting_kind")}(this);
                }

                this.kind = NodeKind.fromC(kindNative);
            }
            return this.kind;
        }

        ${java_doc('langkit.rewriting.handle_node', 8)}
        public ${root_node_type} getParsedNode() {
            if(this.parsedNode == null) {
                final Entity nodeEntity;

                if(ImageInfo.inImageCode()) {
                    final Pointer nodeNative =
                        NI_LIB.${nat("rewriting_handle_to_node")}(
                            this.unwrap()
                        );
                    final EntityNative entityNative = StackValue.get(
                        EntityNative.class
                    );
                    NI_LIB.${nat("create_bare_entity")}(
                        nodeNative,
                        entityNative
                    );
                    nodeEntity = Entity.wrap(entityNative);
                } else {
                    nodeEntity = JNI_LIB.${nat("rewriting_handle_to_node")}(
                        this
                    );
                }

                this.parsedNode = ${root_node_type}.fromEntity(nodeEntity);
            }
            return this.parsedNode;
        }

        ${java_doc('langkit.rewriting.node_context', 8)}
        public RewritingContext getRewritingContext() {
            if(this.rewritingContext == null) {

                if(ImageInfo.inImageCode()) {
                    this.rewritingContext = RewritingContext.wrap(
                        NI_LIB.${nat("rewriting_node_to_context")}(
                            this.unwrap()
                        )
                    );
                } else {
                    this.rewritingContext =
                        JNI_LIB.${nat("rewriting_node_to_context")}(this);
                }

            }
            return this.rewritingContext;
        }

        /** Get whether the rewriting node is a none node. */
        public boolean isNone() {
            return this.reference.isNull();
        }

        // ----- Instance methods -----

        ${java_doc('langkit.rewriting.clone', 8)}
        public RewritingNode clone() {

            if(ImageInfo.inImageCode()) {
                return RewritingNode.wrap(
                    NI_LIB.${nat("rewriting_clone")}(this.unwrap())
                );
            } else {
                return JNI_LIB.${nat("rewriting_clone")}(this);
            }

        }

        ${java_doc('langkit.rewriting.unparse', 8)}
        public String unparse() {
            final Text text;

            if(ImageInfo.inImageCode()) {
                TextNative textNative = StackValue.get(TextNative.class);
                NI_LIB.${nat("rewriting_node_unparse")}(
                    this.unwrap(),
                    textNative
                );
                text = Text.wrap(textNative);
            } else {
                text = JNI_LIB.${nat("rewriting_node_unparse")}(this);
            }

            final String res = text.getContent();
            text.close();
            return res;
        }

        ${java_doc('langkit.rewriting.node_image', 8)}
        public String image() {
            final Text text;

            if(ImageInfo.inImageCode()) {
                TextNative textNative = StackValue.get(TextNative.class);
                NI_LIB.${nat("rewriting_node_image")}(
                    this.unwrap(),
                    textNative
                );
                text = Text.wrap(textNative);
            } else {
                text = JNI_LIB.${nat("rewriting_node_image")}(this);
            }

            final String res = text.getContent();
            text.close();
            return res;
        }

        ${java_doc('langkit.rewriting.tied', 8)}
        public boolean isTied() {

            if(ImageInfo.inImageCode()) {
                return NI_LIB.${nat("rewriting_tied")}(
                    this.unwrap()
                ) > 0;
            } else {
                return JNI_LIB.${nat("rewriting_tied")}(this);
            }

        }

        ${java_doc('langkit.rewriting.parent', 8)}
        public RewritingNode parent() {

            if(ImageInfo.inImageCode()) {
                return RewritingNode.wrap(
                    NI_LIB.${nat("rewriting_parent")}(
                        this.unwrap()
                    )
                );
            } else {
                return JNI_LIB.${nat("rewriting_parent")}(this);
            }

        }

        ${java_doc('langkit.rewriting.children', 8)}
        public RewritingNode[] children() {

            if(ImageInfo.inImageCode()) {
                // Call the native function
                final WordPointer childrenPointer =
                    StackValue.get(WordPointer.class);
                final CIntPointer countPointer =
                    StackValue.get(CIntPointer.class);
                NI_LIB.${nat("rewriting_children")}(
                    this.unwrap(),
                    childrenPointer,
                    countPointer
                );
                final WordPointer children = childrenPointer.read();
                final int count = countPointer.read();

                // Create the Java array and fill it
                final RewritingNode[] res = new RewritingNode[count];
                for(int i = 0; i < res.length; i++) {
                    res[i] = RewritingNode.wrap(
                        (RewritingNodeNative) children.read(i)
                    );
                }

                // Free the native children
                NI_LIB.${nat("free")}(children);

                // Return the result
                return res;
            } else {
                return JNI_LIB.${nat("rewriting_children")}(this);
            }

        }

        ${java_doc('langkit.rewriting.child_by_ref', 8)}
        public RewritingNode getChild(
            final MemberReference childMember
        ) {
            final RewritingNode res;

            if(ImageInfo.inImageCode()) {
                res = RewritingNode.wrap(
                    NI_LIB.${nat("rewriting_child")}(
                        this.unwrap(),
                        childMember.toC()
                    )
                );
            } else {
                res = JNI_LIB.${nat("rewriting_child")}(
                    this,
                    childMember.toC()
                );
            }

            checkException();
            return res;
        }

        ${java_doc('langkit.rewriting.set_child_by_ref', 8)}
        public void setChild(
            MemberReference childMember,
            RewritingNode child
        ) {

            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("rewriting_set_child")}(
                    this.unwrap(),
                    childMember.toC(),
                    child.unwrap()
                );
            } else {
                JNI_LIB.${nat("rewriting_set_child")}(
                    this,
                    childMember.toC(),
                    child
                );
            }
            checkException();

        }

        ${java_doc('langkit.rewriting.replace', 8)}
        public void replace(
            final RewritingNode newNode
        ) {

            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("rewriting_replace")}(
                    this.unwrap(),
                    newNode.unwrap()
                );
            } else {
                JNI_LIB.${nat("rewriting_replace")}(this, newNode);
            }
            checkException();

        }

        ${java_doc('langkit.rewriting.first_child', 8)}
        public RewritingNode firstChild() {

            if(ImageInfo.inImageCode()) {
                return RewritingNode.wrap(
                    NI_LIB.${nat("rewriting_first_child")}(
                        this.unwrap()
                    )
                );
            } else {
                return JNI_LIB.${nat("rewriting_first_child")}(this);
            }

        }

        ${java_doc('langkit.rewriting.last_child', 8)}
        public RewritingNode lastChild() {

            if(ImageInfo.inImageCode()) {
                return RewritingNode.wrap(
                    NI_LIB.${nat("rewriting_last_child")}(
                        this.unwrap()
                    )
                );
            } else {
                return JNI_LIB.${nat("rewriting_last_child")}(this);
            }

        }

        ${java_doc('langkit.rewriting.next_child', 8)}
        public RewritingNode nextChild() {

            if(ImageInfo.inImageCode()) {
                return RewritingNode.wrap(
                    NI_LIB.${nat("rewriting_next_child")}(
                        this.unwrap()
                    )
                );
            } else {
                return JNI_LIB.${nat("rewriting_next_child")}(this);
            }

        }

        ${java_doc('langkit.rewriting.previous_child', 8)}
        public RewritingNode previousChild() {

            if(ImageInfo.inImageCode()) {
                return RewritingNode.wrap(
                    NI_LIB.${nat("rewriting_previous_child")}(
                        this.unwrap()
                    )
                );
            } else {
                return JNI_LIB.${nat("rewriting_previous_child")}(this);
            }

        }

        ${java_doc('langkit.rewriting.insert_before', 8)}
        public void insertBefore(
            final RewritingNode toInsert
        ) {

            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("rewriting_insert_before")}(
                    this.unwrap(),
                    toInsert.unwrap()
                );
            } else {
                JNI_LIB.${nat("rewriting_insert_before")}(this, toInsert);
            }
            checkException();

        }

        ${java_doc('langkit.rewriting.insert_after', 8)}
        public void insertAfter(
            final RewritingNode toInsert
        ) {

            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("rewriting_insert_after")}(
                    this.unwrap(),
                    toInsert.unwrap()
                );
            } else {
                JNI_LIB.${nat("rewriting_insert_after")}(this, toInsert);
            }
            checkException();

        }

        ${java_doc('langkit.rewriting.insert_first', 8)}
        public void insertFirst(
            final RewritingNode toInsert
        ) {

            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("rewriting_insert_first")}(
                    this.unwrap(),
                    toInsert.unwrap()
                );
            } else {
                JNI_LIB.${nat("rewriting_insert_first")}(this, toInsert);
            }
            checkException();

        }

        ${java_doc('langkit.rewriting.insert_last', 8)}
        public void insertLast(
            final RewritingNode toInsert
        ) {

            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("rewriting_insert_last")}(
                    this.unwrap(),
                    toInsert.unwrap()
                );
            } else {
                JNI_LIB.${nat("rewriting_insert_last")}(this, toInsert);
            }
            checkException();

        }

        ${java_doc('langkit.rewriting.remove_child', 8)}
        public void removeFromParent() {

            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("rewriting_remove_child")}(this.unwrap());
            } else {
                JNI_LIB.${nat("rewriting_remove_child")}(this);
            }
            checkException();

        }

        /** Get the text of the token node. */
        public String getText() {
            final Text resultText;

            if(ImageInfo.inImageCode()) {
                final TextNative textNative = StackValue.get(
                    TextNative.class
                );
                NI_LIB.${nat("rewriting_text")}(
                    this.unwrap(),
                    textNative
                );
                resultText = Text.wrap(textNative);
            } else {
                resultText = JNI_LIB.${nat("rewriting_text")}(this);
            }

            final String res = resultText.getContent();
            resultText.close();
            return res;
        }

        ${java_doc('langkit.rewriting.set_text', 8)}
        public void setText(
            final String text
        ) {
            try (
                final Text nodeText = Text.create(text);
            ) {

                if(ImageInfo.inImageCode()) {
                    final TextNative textNative = StackValue.get(
                        TextNative.class
                    );
                    nodeText.unwrap(textNative);
                    NI_LIB.${nat("rewriting_set_text")}(
                        this.unwrap(),
                        textNative
                    );
                } else {
                    JNI_LIB.${nat("rewriting_set_text")}(this, nodeText);
                }
                checkException();

            }
        }

        // ----- Override methods -----

        @Override
        public String toString() {
            return this.image();
        }

        @Override
        public boolean equals(Object o) {
            if(o == this) return true;
            if(!(o instanceof RewritingNode)) return false;
            final RewritingNode other = (RewritingNode) o;
            return this.reference.equals(other.reference);
        }

    }
    ## End of the rewriting API part
    % endif

    // ===== Generated structure wrapping classes =====

    % for struct_type in ctx.struct_types:
        % if struct_type.is_entity_type:
            % if struct_type is root_entity:
    ${struct.wrapping_class(struct_type)}
            % endif
        % else:
        <%
        emit_struct = (
            struct_type is T.entity_info
            or struct_type is T.env_md
            or struct_type.exposed
        )
        %>
            % if emit_struct:
    ${struct.wrapping_class(struct_type)}
            % endif
        % endif
    % endfor

    // ===== Generated array utility classes =====

    % for array_type in ctx.array_types:
        % if array_type.exposed:
    ${array.wrapping_class(array_type)}
        % endif
    % endfor

    // ===== Generated iterator wrapping classes =====

    % for iterator_type in ctx.iterator_types:
        % if iterator_type.exposed and iterator_type.emit_c_type:
    ${iterator.wrapping_class(iterator_type)}
        % endif
    % endfor

    // ===== Node classes =====

    <%
        from langkit.java_api import format_name
        root_implements = api.make_implements(T.root_node.implements())
    %>

    ${java_doc('langkit.node_type', 4)}
    public static abstract
    class ${root_node_type} ${root_implements}{

        // ----- Static -----

        ${ast_node.static_decl(T.root_node)}

        // ----- Attributes -----

        /** Singleton that represents the none node. */
        public static final ${root_node_type} NONE = new NoneNode();

        /** The entity of the node. */
        public final Entity entity;

        % if ctx.generate_unparsers:
        /** Cache for the associated rewriting node. */
        protected RewritingNode rewritingNode;
        % endif

        /** The analysis unit that owns the node. */
        protected AnalysisUnit unit;

        /** The cache for the image of the node. */
        protected String image;

        // ----- Constructors -----

        /**
         * Create a new node with its entity.
         *
         * @param entity The node's entity.
         */
        protected ${root_node_type}(
            final Entity entity
        ) {
            this.entity = entity;
            this.unit = null;
            this.image = null;
        }

        /**
         * Get a node from the given entity.
         *
         * @param entity The entity to get the node from.
         * @return The newly created node.
         */
        public static ${root_node_type} fromEntity(
            final Entity entity
        ) {
            return entity.node.isNull() ?
                ${root_node_type}.NONE :
                dispatchNodeCreation(entity);
        }

        /**
         * Dispatch the node creation to return the valid Java object
         * according to the node kind.
         *
         * @param entity The entity to create the node from.
         * @return The wrapped node in the correct class.
         */
        protected static ${root_node_type} dispatchNodeCreation(
            final Entity entity
        ) {
            int nodeKind = -1;

            if(ImageInfo.inImageCode()) {
                EntityNative entityNative = StackValue.get(
                    EntityNative.class
                );
                entity.unwrap(entityNative);
                nodeKind = NI_LIB.${nat("node_kind")}(entityNative);
            } else {
                nodeKind = JNI_LIB.${nat("node_kind")}(entity);
            }

            switch(nodeKind) {
                % for subclass in ctx.astnode_types:
                    % if not subclass.abstract:
                case ${ctx.node_kind_constants[subclass]}:
                    return entity.node.isNull() ?
                        ${api.wrapping_type(subclass)}.NONE :
                        new ${api.wrapping_type(subclass)}(entity);
                    % endif
                % endfor
                default:
                    throw new EnumException(
                        "Cannot find the node type from " + nodeKind
                    );
            }
        }

        // ----- Graal C API methods -----

        /**
         * Util internal method to unwrap a node to a native entity struct
         */
        void unwrap(
            final EntityNative entityNative
        ) {
            this.entity.unwrap(entityNative);
        }

        // ----- Getters -----

        public Reflection.Node getDescription() {
            return ${root_node_type}.description;
        }

        public NodeKind getKind() {
            return this.getDescription().kind;
        }

        public String getClassName() {
            return this.getDescription().className;
        }

        public boolean isTokenNode() {
            return this.getDescription().isTokenNode;
        }

        public boolean isListNode() {
            return this.getDescription().isListNode;
        }

        public String[] getFieldNames() {
            return this.getDescription().fields;
        }

        public Map<String, Reflection.Field> getFieldDescriptions()
        {
            return this.getDescription().fieldDescriptions;
        }

        @CompilerDirectives.TruffleBoundary
        public Reflection.Field getFieldDescription(
            final String name
        ) {
            return this.getDescription()
                .fieldDescriptions
                .get(name);
        }

        public boolean isNone() {
            return this.entity.node.isNull();
        }

        // ----- Instance methods -----

        /**
         * Get the analysis unit of the node.
         *
         * @return The unit which owns the node.
         */
        public AnalysisUnit getUnit() {
            if(this.unit == null) {

                if(ImageInfo.inImageCode()) {
                    final EntityNative entityNative = StackValue.get(
                        EntityNative.class
                    );
                    this.entity.unwrap(entityNative);

                    final AnalysisUnitNative unitNative =
                        NI_LIB.${nat("node_unit")}(entityNative);
                    this.unit = AnalysisUnit.wrap(unitNative);
                } else {
                    this.unit = JNI_LIB.${nat("node_unit")}(this.entity);
                }

            }
            return this.unit;
        }

        /**
         * Get the children count of the node.
         *
         * @return The children count.
         */
        public int getChildrenCount() {

            if(ImageInfo.inImageCode()) {
                final EntityNative thisNative = StackValue.get(
                    EntityNative.class
                );
                this.entity.unwrap(thisNative);

                return NI_LIB.${nat("node_children_count")}(thisNative);
            } else {
                return JNI_LIB.${nat("node_children_count")}(this.entity);
            }

        }

        /**
         * Get the child at the given position.
         *
         * @param n The index of the child to get.
         * @return The child at the given index.
         */
        public ${root_node_type} getChild(
            final int n
        ) {

            if(ImageInfo.inImageCode()) {
                final EntityNative thisNative = StackValue.get(
                    EntityNative.class
                );
                this.entity.unwrap(thisNative);

                final EntityNative resNative = StackValue.get(
                    EntityNative.class
                );
                NI_LIB.${nat("node_child")}(
                    thisNative,
                    n,
                    resNative
                );

                return fromEntity(Entity.wrap(resNative));
            } else {
                return fromEntity(JNI_LIB.${nat("node_child")}(
                    this.entity,
                    n
                ));
            }

        }

        /**
         * Get the text of the node.
         *
         * @return The text of the node.
         */
        public String getText() {

            if(ImageInfo.inImageCode()) {
                final EntityNative thisNative = StackValue.get(
                    EntityNative.class
                );
                this.entity.unwrap(thisNative);

                final TextNative resNative = StackValue.get(TextNative.class);
                Text.NONE.unwrap(resNative);
                NI_LIB.${nat("node_text")}(
                    thisNative,
                    resNative
                );

                try(final Text resText = Text.wrap(resNative)) {
                    return resText.getContent();
                }
            } else {
                try(
                    final Text resText = JNI_LIB.${nat("node_text")}(
                        this.entity
                    );
                ) {
                    return resText.getContent();
                }
            }

        }

        /**
         * Get the image of the node.
         *
         * @return The node's image.
         */
        public String getImage() {
            if(this.image == null) {

                if(ImageInfo.inImageCode()) {
                    final EntityNative thisNative = StackValue.get(
                        EntityNative.class
                    );
                    this.entity.unwrap(thisNative);

                    final TextNative resNative = StackValue.get(
                        TextNative.class
                    );
                    Text.NONE.unwrap(resNative);
                    NI_LIB.${nat("node_image")}(thisNative, resNative);

                    try(final Text resText = Text.wrap(resNative)) {
                        this.image = resText.getContent();
                    }
                } else {
                    try(
                        final Text resText = JNI_LIB.${nat("node_image")}(
                            this.entity
                        )
                    ) {
                        this.image = resText.getContent();
                    }
                }

            }

            return this.image;
        }

        /**
         * Get the source location range of the node.
         *
         * @return The source location range of the node.
         */
        public SourceLocationRange getSourceLocationRange() {

            if(ImageInfo.inImageCode()) {
                final EntityNative thisNative = StackValue.get(
                    EntityNative.class
                );
                this.entity.unwrap(thisNative);

                final SourceLocationRangeNative resNative = StackValue.get(
                    SourceLocationRangeNative.class
                );
                NI_LIB.${nat("node_sloc_range")}(
                    thisNative,
                    resNative
                );

                return SourceLocationRange.wrap(resNative);
            } else {
                return JNI_LIB.${nat("node_sloc_range")}(this.entity);
            }

        }

        % if ctx.generate_unparsers:
        ${java_doc('langkit.rewriting.node_handle', 8)}
        public RewritingNode getRewritingNode() {
            if(this.rewritingNode == null) {
                final RewritingNode tmp;

                if(ImageInfo.inImageCode()) {
                    tmp = RewritingNode.wrap(
                        NI_LIB.${nat("rewriting_node_to_handle")}(
                            (Pointer) this.entity.node.ni()
                        )
                    );
                } else {
                    tmp = JNI_LIB.${nat("rewriting_node_to_handle")}(
                        this.entity
                    );
                }
                checkException();
                this.rewritingNode = tmp;

            }
            return this.rewritingNode;
        }
        % endif

        // ----- Dumping methods -----

        /**
         * Return the parsing tree in a string.
         *
         * @return The string containing the representation of the parsing tree
         * from the node.
         */
        @CompilerDirectives.TruffleBoundary
        public String dumpTree() {
            final StringBuilder builder = new StringBuilder();
            this.dumpTree(builder);
            return builder.toString();
        }

        /**
         * Dump the parse tree in the given string builder.
         *
         * @param builder The builder to dump the parse tree in.
         */
        @CompilerDirectives.TruffleBoundary
        public void dumpTree(
            final StringBuilder builder
        ) {
            this.dumpTree(builder, "");
        }

        /**
         * Dump a node field in the given string builder.
         *
         * @param builder The string builder to put the file in.
         * @param indent The current indentation string.
         * @param name The name of the field.
         * @param value The value of the field.
         */
        protected static void dumpField(
            final StringBuilder builder,
            final String indent,
            final String name,
            final ${root_node_type} value
        ) {
            builder.append(indent)
                .append(name)
                .append(":\n");
            value.dumpTree(builder, indent + "  ");
        }

        /**
         * Dump the parse tree in the given string builder with the indent
         * level.
         *
         * @param builder The builder to dump the tree in.
         * @param indent The starting indent level.
         */
        @CompilerDirectives.TruffleBoundary
        protected void dumpTree(
            final StringBuilder builder,
            String indent
        ) {
            // Prepare the working variables
            String image = this.getImage();
            image = image.substring(1, image.length());
            final int childrenCount = this.getChildrenCount();

            // Print the node
            builder.append(indent)
                .append(image);
            if(this.isTokenNode()) {
                builder.append(": ")
                    .append(this.getText());
            }
            builder.append('\n');

            // Print the field of the node
            indent = indent + "|";
            if(this.isListNode()) {
                for(int i = 0 ; i < childrenCount ; i++) {
                    final ${root_node_type} child = this.getChild(i);
                    dumpField(builder, indent, "item_" + i, child);
                }
            } else {
                for(int i = 0 ; i < childrenCount ; i++) {
                    final ${root_node_type} child = this.getChild(i);
                    final String name = this.getFieldNames()[i];
                    dumpField(builder, indent, name, child);
                }
            }
        }

        // ----- Visitor methods -----

        /**
         * Accept the given visitor.
         *
         * @param visitor The visitor to accept.
         * @return The result of the visit.
         */
        public <T> T accept(BasicVisitor<T> visitor) {
            return visitor.visit(this);
        }

        /**
         * Accept the given visitor.
         *
         * @param visitor The visitor to accept.
         * @param param The parameter of the visit.
         * @return The result of the visit.
         */
        public <T, P> T accept(ParamVisitor<T, P> visitor, P param) {
            return visitor.visit(this, param);
        }

        // ----- Field accessors -----

        % for field in T.root_node.fields_with_accessors():
        ${ast_node.field_accessor(field)}
        % endfor

        // ----- Override methods -----

        @Override
        public String toString() {
            return this.getImage();
        }

        @Override
        public boolean equals(Object o) {
            if(this == o) return true;
            if(!(o instanceof ${root_node_type})) return false;
            final ${root_node_type} other = (${root_node_type}) o;

            if(ImageInfo.inImageCode()) {
                final EntityNative thisNative = StackValue.get(
                    EntityNative.class
                );
                this.entity.unwrap(thisNative);

                final EntityNative otherNative = StackValue.get(
                    EntityNative.class
                );
                other.entity.unwrap(otherNative);

                return NI_LIB.${nat("node_is_equivalent")}(
                    thisNative,
                    otherNative
                ) != 0;
            } else {
                return JNI_LIB.${nat("node_is_equivalent")}(
                    this.entity, other.entity
                ) != 0;
            }
        }

        @Override
        public int hashCode() {
            if(ImageInfo.inImageCode()) {
                final EntityNative thisNative = StackValue.get(
                    EntityNative.class
                );
                this.entity.unwrap(thisNative);

                return NI_LIB.${nat("node_hash")}(thisNative);
            } else {
                return JNI_LIB.${nat("node_hash")}(this.entity);
            }

        }

        // ----- Inner classes -----

        /**
         * This class represents the none node without any concrete type.
         */
        private static final class NoneNode extends ${root_node_type} {
            NoneNode() {super(Entity.NONE);}
        }

    }

    // ===== Generated node wrapping classes =====

    % for astnode in ctx.astnode_types:
        % if astnode != T.root_node:
    ${ast_node.wrapping_class(astnode)}
        % endif
    % endfor

}
