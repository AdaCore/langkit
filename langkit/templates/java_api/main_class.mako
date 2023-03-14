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

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.Objects;

import java.lang.StringBuilder;
import java.lang.Iterable;
import java.lang.reflect.Method;

import java.math.BigInteger;

import java.io.File;
import java.nio.ByteOrder;

import com.oracle.truffle.api.CompilerDirectives;

import org.graalvm.nativeimage.StackValue;
import org.graalvm.nativeimage.UnmanagedMemory;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CFunction;
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
public class ${ctx.lib_name.camel} {

    // ==========
    // Macros
    // ==========

    /** The default grammar rule to parse the inputs. */
    protected static final GrammarRule DEFAULT_GRAMMAR_RULE =
        GrammarRule.${ctx.main_rule_api_name.upper};

    /** The os name in lower case. */
    private static final String OS =
            System.getProperty("os.name").toLowerCase();

    /** If the system is big endian, else it's little endian. */
    protected static final boolean BIG_ENDIAN =
        ByteOrder.nativeOrder().equals(ByteOrder.BIG_ENDIAN);

    /** A map to store the nodes classes from their name. */
    public static final Map<String, Class<? extends ${root_node_type}>>
        NODE_CLASS_MAP = new HashMap<>();

    static {
        // Populate the node class map
        % for node_class in ctx.astnode_types:
        NODE_CLASS_MAP.put(
            "${node_class.kwless_raw_name.camel}",
            ${api.wrapping_type(node_class)}.class
        );
        % endfor
    }

    // ==========
    // Util functions
    // ==========

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
        UnsignedWord size = WordFactory.unsigned(jString.length() + 1);
        CCharPointer res = UnmanagedMemory.calloc(size);
        if(jString.length() > 0) {
            CTypeConversion.toCString(
                jString,
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
        final int[] chars
    ) {
        final byte[] realContent = new byte[chars.length];
        for(int i = 0 ; i < chars.length ; i++) {
            realContent[i] = (byte) (chars[i] & 0xFF);
        }
        return new String(realContent);
    }

    /**
     * This function encode a given string to a int array
     * according to the utf 32 standard.
     *
     * @param toEncode The string to encode.
     * @return The encoded string in an int array.
     */
    @CompilerDirectives.TruffleBoundary
    private static int[] encodeUTF32(
        final String toEncode
    ) {
        final byte[] src = toEncode.getBytes();
        final int[] res = new int[src.length];
        for(int i = 0 ; i < src.length ; i++) {
            res[i] = (int) src[i];
        }
        return res;
    }

    /**
     * This function turn an int array to a byte array.
     *
     * @param intArray The integer array to translate.
     * @return The byte array.
     */
    private static byte[] intToByteArray(
        final int[] intArray
    ) {
        final byte[] res = new byte[intArray.length * 4];
        for(int i = 0 ; i < res.length ; i+=4) {
            final int currentInt = intArray[i / 4];
            if(BIG_ENDIAN) {
                res[i] = (byte) ((currentInt >> 24) & 0xFF);
                res[i + 1] = (byte) ((currentInt >> 16) & 0xFF);
                res[i + 2] = (byte) ((currentInt >> 8) & 0xFF);
                res[i + 3] = (byte) (currentInt & 0xFF);
            } else {
                res[i + 3] = (byte) ((currentInt >> 24) & 0xFF);
                res[i + 2] = (byte) ((currentInt >> 16) & 0xFF);
                res[i + 1] = (byte) ((currentInt >> 8) & 0xFF);
                res[i] = (byte) (currentInt & 0xFF);
            }
        }
        return res;
    }

    /**
     * Check the last exception raised by langkit.
     *
     * @return The last exception wrapped in the LangkitException class
     * if there is an exception, null if there is none.
     */
    private static LangkitException checkException() {
        LangkitException res = null;

        if(ImageInfo.inImageCode()) {
            final LangkitExceptionNative exceptionNative =
                NI_LIB.${nat("get_last_exception")}();
            if(exceptionNative.isNonNull()) {
                res = new LangkitException(
                    exceptionNative.get_kind(),
                    toJString(exceptionNative.get_information())
                );
            }
        } else {
            res = JNI_LIB.${nat("get_last_exception")}();
        }

        return res;
    }

    // ==========
    // Util interfaces
    // ==========

    /**
     * Interface to visit the AST.
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
     * Interface to visit the AST with a parameter.
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

        // ----- Attributes -----

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
                return this.jni() == other.jni();
            }
        }

    }

    /**
     * This class represents the description of a node field.
     */
    public static final class ${ctx.lib_name.camel}Field {

        // ----- Attributes -----

        /** The Java method for the field */
        public final Method javaMethod;

        /** The parameters of the method */
        public final List<Param> params;

        // ----- Constructors -----

        /**
         * Create a new field description.
         *
         * @param method The Java method to access the field.
         * @param params The parameters of the field call.
         */
        public ${ctx.lib_name.camel}Field(
            final Method javaMethod,
            final List<Param> params
        ) {
            this.javaMethod = javaMethod;
            this.params = params;
        }

    }

    /**
     * This class represents a parameter description.
     */
    public static class Param {

        // ----- Attributes -----

        /** The type of the argument */
        public final Class<?> type;

        /** The name of the parameter */
        public final String name;

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
        }

    }

    /**
     * This class represents an parameter in a langkit function description
     * which has a default value.
     */
    public static final class ParamWithDefaultValue extends Param {

        // ----- Attributes -----

        /** The default value of the parameter */
        public final Object defaultValue;

        // ----- Constructors -----

        /**
         * Create a new langkit parameter.
         *
         * @param type The type of the parameter.
         * @param name The name of the parameter.
         * @param defaultValue The default value of the parameter.
         */
        public ParamWithDefaultValue(
            final Class<?> type,
            final String name,
            final Object defaultValue
        ) {
            super(type, name);
            this.defaultValue = defaultValue;
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
     * This class wraps the exceptions from the langkit native library.
     */
    public static class LangkitException extends RuntimeException {

        // ----- Attributes -----

        /** The kind of the langkit exception. */
        public final ExceptionKind kind;

        // ----- Constructors -----

        /**
         * Create a new langkit exception.
         *
         * @param kind The kind of the exception represented by an integer
         * which will be mapped to an enum value.
         * @param message The message of the exception.
         */
        public LangkitException(
            final int kind,
            final String message
        ) {
            super(message);
            this.kind = ExceptionKind.fromC(kind);
        }

    }

    // ==========
    // Enum definitions
    // ==========

    // ===== Constants enumeration =====

    ${java_doc('langkit.token_kind', 4)}
    public enum TokenKind {

        // ----- Enum values -----

        NO_TOKEN(-1),
        % for i, t in enumerate(ctx.lexer.sorted_tokens):
        ${t.c_name}(${t.value}),
        % endfor
        ;

        // ----- Attributes -----

        /** The value of the enum instance. */
        private final int value;

        /** The map from int to enum values. */
        private static final Map<Integer, TokenKind> map = new HashMap<>();

        // ----- Constructors -----

        static {
            // Initialise the lookup map
            for(TokenKind elem : TokenKind.values()) {
                map.put(elem.value, elem);
            }
        }

        /** Private constructor. */
        private TokenKind(
            final int value
        ) {
            this.value = value;
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
        ${ctx.sorted_exception_types[i].kind_name.upper}(${i}),
        % endfor
        ;

        // ----- Attributes -----

        /** The value of the enum instance. */
        private final int value;

        /** The map from int to enum values. */
        private static final Map<Integer, ExceptionKind> map =
            new HashMap<>();

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

    // ===== Generated enums =====

    % for enum_type in ctx.enum_types:
    ${enum.decl(enum_type)}
    % endfor

    // ==========
    // Java wrapping classes
    // ==========

    // ===== Constant structure wrapping classes =====

    /**
     * This class wraps the langkit characters which are 32 bit wide.
     */
    public static final class Char {

        // ----- Attributes -----

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
         * @param niPointer The NI pointer to wrap.
         * @return The wrapped character.
         */
        static Char wrap(
            final CIntPointer niPointer
        ) {
            return wrap(niPointer.read());
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

        // ----- Instance methods -----

        /**
         * Return the value as a Java character.
         *
         * @return The character value as a Java char.
         */
        public char toChar() {
            return (char) this.value;
        }

        // ----- Override methods -----

        @Override
        public String toString() {
            return new String(new char[]{this.toChar()});
        }

    }

    ${java_doc('langkit.big_integer_type', 4)}
    static final class BigIntegerWrapper {

        // ----- Graal C API methods -----

        /**
         * Wrap a pointer which points to a native big integer.
         *
         * @param pointer The pointer to the native big integer.
         * @return The Java big integer.
         */
        static BigInteger wrap(
            final Pointer pointer
        ) {
            return wrap((BigIntegerNative) pointer.readWord(0));
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
         * Unwrap the given big integer in the given pointer as a native
         * big integer.
         *
         * @param bigInteger The big integer to unwrap.
         * @param pointer The pointer to place the big integer in.
         */
        static void unwrap(
            final BigInteger bigInteger,
            final Pointer pointer
        ) {
            final BigIntegerNative bigIntegerNative = unwrap(bigInteger);
            pointer.writeWord(0, bigIntegerNative);
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
            final Text bigIntegerText = Text.create(representation);
            TextNative bigIntegerTextNative = StackValue.get(
                TextNative.class
            );
            bigIntegerText.unwrap(bigIntegerTextNative);

            // Create the big intger by calling the native function
            final BigIntegerNative res = NI_LIB.${nat("create_big_integer")}(
                bigIntegerTextNative
            );

            // Close the text
            bigIntegerText.close();

            // Return the result
            return res;
        }

        /**
         * Release the big integer pointed by the given pointer.
         *
         * @param pointer The pointer to the big integer to release.
         */
        static void release(
            final Pointer pointer
        ) {
            release((BigIntegerNative) pointer.readWord(0));
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
            Text.defaultValue(bigIntegerTextNative);

            // Call the native function
            NI_LIB.${nat("big_integer_text")}(
                bigIntegerNative,
                bigIntegerTextNative
            );

            // Wrap the text and return the result
            final Text bigIntegerText = Text.wrap(bigIntegerTextNative);
            final String res = bigIntegerText.getContent();
            bigIntegerText.close();
            return res;
        }

    }

    ${java_doc('langkit.symbol_type', 4)}
    public static final class Symbol {

        // ----- Attributes -----

        /** The text of the symbol. */
        public final String text;

        // ----- Constructors -----

        /**
         * Create a new symbol from its text.
         *
         * @param text The symbol text.
         */
        Symbol(
            String text
        ) {
            this.text = text;
        }

        /**
         * Public access to the symbol creation.
         *
         * @param text The text of the symbol.
         */
        public static Symbol create(String text) {
            return new Symbol(text);
        }

        // ----- Class methods -----

        /**
         * Wrap the given symbol native value in a Java value.
         *
         * @param symbolNative The symbol native value.
         * @return The wrapped symbol.
         */
        static Symbol wrap(SymbolNative symbolNative) {
            // Get the symbol text
            TextNative textNative = StackValue.get(TextNative.class);
            Text.defaultValue(textNative);
            NI_LIB.${nat("symbol_text")}(
                symbolNative,
                textNative
            );

            // Return the new symbol
            try(Text text = Text.wrap(textNative)) {
                return new Symbol(text.getContent());
            }
        }

        /**
         * Fill the given NI native value with default values.
         *
         * @param symbolNative The NI value to fill.
         */
        static void defaultValue(SymbolNative symbolNative) {
            symbolNative.set_data(WordFactory.nullPointer());
            symbolNative.set_bounds(WordFactory.nullPointer());
        }

        // ----- Instance methods -----

        /**
         * Unwrap the symbol in the given native structure.
         *
         * @param symbolNative The native structure to unwrap in.
         */
        void unwrap(
            SymbolNative symbolNative,
            AnalysisContext context
        ) {
            // Unwrap the symbol text
            try(Text text = Text.create(this.text)) {
                TextNative textNative = StackValue.get(TextNative.class);
                text.unwrap(textNative);

                // Call the symbol creation
                int resCode = NI_LIB.${nat("context_symbol")}(
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
    public static final class StringWrapper implements AutoCloseable {

        // ----- Attributes -----

        /** The reference to the native string. */
        public final PointerWrapper reference;

        /** The array of the string content. */
        public final int[] contentArray;

        /** The content of the string. */
        private String content;

        // ----- Constructors -----

        /**
         * Create a new string wrapper from its reference.
         *
         * @param reference The reference to the native string value.
         */
        StringWrapper(
            PointerWrapper reference
        ) {
            this(
                reference,
                null
            );
        }

        /**
         * Create a new string wrapper.
         *
         * @param reference The pointer that represents the native
         * string value.
         * @param contentArray The content of the string in an int array
         * (this param is null in native-image mode).
         */
        StringWrapper(
            PointerWrapper reference,
            int[] contentArray
        ) {
            this.reference = reference;
            this.contentArray = contentArray;
            this.content = null;
        }

        /**
         * Create a new string wrapper from its content.
         *
         * @param content The content of the string wrapper.
         * @return The newly created string wrapper.
         */
        public static StringWrapper create(
            String content
        ) {
            byte[] contentArray = intToByteArray(encodeUTF32(content));

            if(ImageInfo.inImageCode()) {
                CTypeConversion.CCharPointerHolder holder =
                    CTypeConversion.toCBytes(contentArray);
                return StringWrapper.wrap(
                    NI_LIB.${nat("create_string")}(
                        (CIntPointer) holder.get(),
                        content.length()
                    )
                );
            } else {
                return JNI_LIB.${nat("create_string")}(contentArray);
            }
        }

        // ----- Cleaning methods/classes -----

        /**
         * Release the given string reference.
         *
         * @param stringRef The reference to the string to release.
         */
        private static void release(
            PointerWrapper stringRef
        ) {

            /*
            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("string_dec_ref")}(stringRef.ni());
            } else {
                JNI_LIB.${nat("string_dec_ref")}(stringRef.jni());
            }
            */

        }

        /** @see java.lang.AutoCloseable#close() */
        @Override
        public void close() {
            // DO NOTHING FOR NOW
        }

        // ----- Class methods -----

        /**
         * Wrap a pointer that points to the native string wrapper in
         * the Java class.
         *
         * @param niPointer The pointer that leads to the string NI value.
         * @return the newly created string wrapper or null if the pointer
         * or the native string are null.
         */
        static StringWrapper wrap(
            Pointer niPointer
        ) {
            if(niPointer.isNull()) return null;
            else return wrap((StringNative) niPointer.readWord(0));
        }

        /**
         * Wrap a native string wrapper in the Java class.
         *
         * @param stringNative The NI string wrapper native value.
         * @return The newly created string wrapper or null if the value
         * is null.
         */
        static StringWrapper wrap(
            StringNative stringNative
        ) {
            if(((PointerBase) stringNative).isNull()) return null;
            else return new StringWrapper(
                new PointerWrapper(stringNative)
            );
        }

        /**
         * Get the content of the string in a integer array.
         * Internal function used for the NI mode.
         *
         * @return The content of the string in an integer array.
         */
        private static int[] getContentArray(
            StringNative stringWrapperPointer
        ) {
            Pointer niPointer = (Pointer) stringWrapperPointer;
            int length = niPointer.readInt(0);
            int[] res = new int[length];
            for(int i = 0 ; i < length ; i++) {
                res[i] = niPointer.readInt(
                    8 + (i * 4)
                );
            }
            return res;
        }

        // ----- Instance methods -----

        /**
         * Get the content of the string.
         *
         * @return The content of the langkit string in a Java string.
         */
        public String getContent() {
            if(this.content == null) {
                int[] contentArray;

                if(ImageInfo.inImageCode()) {
                    contentArray = getContentArray(this.reference.ni());
                } else {
                    contentArray = this.contentArray != null ?
                        this.contentArray :
                        new int[0];
                }

                this.content = decodeUTF32(contentArray);
            }
            return this.content;
        }

        // ----- Override methods -----

        @Override
        public String toString() {
            return this.getContent();
        }

    }

    ${java_doc('langkit.text_type', 4)}
    public static final class Text implements AutoCloseable {

        // ----- Attributes -----

        /** The pointer to the characters. */
        public final PointerWrapper charPointer;

        /** The size of the text. */
        public final long length;

        /** If the text is allocated. */
        public final boolean isAllocated;

        /** If the text object is the owner of its buffer. */
        private final boolean isOwner;

        /** The content of the text in a Java array. */
        public final int[] contentArray;

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
            PointerWrapper charPointer,
            long length,
            boolean isAllocated
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
         * @param contentArray The characters of the text
         * (as strings, this is only used in JNI mode).
         */
        Text(
            PointerWrapper charPointer,
            long length,
            boolean isAllocated,
            int[] contentArray
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
            PointerWrapper charPointer,
            long length,
            boolean isAllocated,
            boolean isOwner,
            int[] contentArray
        ) {
            this.charPointer = charPointer;
            this.length = length;
            this.isAllocated = isAllocated;
            this.isOwner = isOwner;
            this.contentArray = contentArray;

            this.content = null;
        }

        /**
         * Create a new langkit text from its string content.
         *
         * @param content The content of the text in a Java string.
         * @return The newly created text.
         */
        public static Text create(
            String content
        ) {
            byte[] contentArray = intToByteArray(encodeUTF32(content));

            if(ImageInfo.inImageCode()) {
                PointerWrapper charPointer = new PointerWrapper(
                    toCBytes(contentArray)
                );
                return new Text(
                    charPointer,
                    (long) content.length(),
                    false,
                    true,
                    null
                );
            } else {
                return JNI_LIB.${nat("create_text")}(contentArray);
            }
        }

        // ----- Cleaning methods/classes -----

        /**
         * Release the given text buffer.
         *
         * @param textBuffer The buffer to free.
         */
        private static void release(
            PointerWrapper textBuffer,
            long length,
            boolean isAllocated,
            boolean isOwner
        ) {

            /*
            if(ImageInfo.inImageCode()) {
                if(isOwner) {
                    UnmanagedMemory.free(textBuffer.ni());
                } else {
                    TextNative textNative = StackValue.get(TextNative.class);
                    textNative.set_chars(textBuffer.ni());
                    textNative.set_length(length);
                    textNative.set_is_allocated(isAllocated ? 1 : 0);
                    NI_LIB.${nat("destroy_text")}(textNative);
                }
            } else {
                JNI_LIB.${nat("destroy_text")}(
                    textBuffer.jni(),
                    length,
                    isAllocated,
                    isOwner
                );
            }
            */

        }

        /** @see java.lang.AutoCloseable#close() */
        @Override
        public void close() {
            // DO NOTHING FOR NOW
        }

        // ----- Class methods -----

        /**
         * Wrap a text native value in the Java class.
         *
         * @param textNative The text native NI value.
         * @return The newly created text or null if the text value
         * is null.
         */
        static Text wrap(
            TextNative textNative
        ) {
            if(((PointerBase) textNative).isNull()) return null;
            else return new Text(
                new PointerWrapper(textNative.get_chars()),
                textNative.get_length(),
                textNative.get_is_allocated() != 0
            );
        }

        // ----- Instance methods -----

        /**
         * Get the content of the text in a Java string.
         *
         * @return the content of the text.
         */
        public String getContent() {
            if(this.content == null) {
                int[] contentArray;

                if(ImageInfo.inImageCode()) {
                    contentArray = new int[(int) this.length];
                    for(int i = 0 ; i < this.length ; i++) {
                        contentArray[i] = (
                            (CIntPointer) this.charPointer.ni()
                        ).read(i);
                    }
                } else {
                    contentArray = this.contentArray != null ?
                        this.contentArray :
                        new int[0];
                }
                this.content = decodeUTF32(contentArray);
            }
            return this.content;
        }

        /**
         * Unwrap the text in the given NI pointer.
         *
         * @param textNative The NI pointer to the native text structure.
         */
        void unwrap(TextNative textNative) {
            textNative.set_chars(this.charPointer.ni());
            textNative.set_length(this.length);
            textNative.set_is_allocated(this.isAllocated ? 1 : 0);
        }

        /**
         * Set the given text native structure at the text default value.
         *
         * @param textNative The native structure to fill.
         */
        static void defaultValue(TextNative textNative) {
            textNative.set_chars(WordFactory.nullPointer());
            textNative.set_length(0);
            textNative.set_is_allocated(0);
        }

        // ----- Override methods -----

        @Override
        public String toString() {
            return this.getContent();
        }

    }

    ${java_doc('langkit.sloc_type', 4)}
    public static final class SourceLocation {

        // ----- Attributes -----

        /** The location instance for null source location */
        public static final SourceLocation NULL_LOCATION =
            new SourceLocation(0, (short) 0);

        /** The line of the source location. */
        public final int line;

        /** The column of the source location. */
        public final short column;

        // ----- Constructors -----

        /**
         * Create a new source location from a line and a column.
         *
         * @param line The line of the source location.
         * @param column The column of the source location.
         */
        SourceLocation(
            int line,
            short column
        ) {
            this.line = line;
            this.column = column;
        }

        /**
         * Create a source location from its line and column.
         *
         * @param line The line.
         * @param column The column.
         * @return The newly create source location.
         */
        public static SourceLocation create(
            int line,
            short column
        ) {
            return new SourceLocation(
                line,
                column
            );
        }

        // ----- Class methods -----

        /**
         * Wrap a source location native value in the Java class.
         *
         * @param sourceLocationNative The source location NI native value.
         * @return The newly created source location or null if the native
         * value is null.
         */
        static SourceLocation wrap(
            SourceLocationNative sourceLocationNative
        ) {
            if(((PointerBase) sourceLocationNative).isNull()) return null;
            else return new SourceLocation(
                sourceLocationNative.get_line(),
                sourceLocationNative.get_column()
            );
        }

        // ----- Instance methods -----

        /**
         * Uwrap the source location in the given NI pointer.
         *
         * @param sourceLocationNative The NI pointer to the native structure
         *  to fill.
         */
        public void unwrap(SourceLocationNative sourceLocationNative) {
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
    public static final class SourceLocationRange {

        // ----- Attributes -----

        /** The location range instance for null source location range */
        public static final SourceLocationRange NULL_LOCATION_RANGE =
            new SourceLocationRange(
                SourceLocation.NULL_LOCATION,
                SourceLocation.NULL_LOCATION
            );

        /** The start of the range. */
        public final SourceLocation start;

        /** The end of the range. */
        public final SourceLocation end;

        // ----- Constructors -----

        /**
         * Create a source location range from its bounds.
         *
         * @param start The start of the range.
         * @param end The end of the range.
         */
        SourceLocationRange(
            SourceLocation start,
            SourceLocation end
        ) {
            this.start = start;
            this.end = end;
        }

        /**
         * Create a new source location range from its bounds.
         *
         * @param start The starting bound.
         * @param end The ending bound.
         * @return The newly created source location range.
         */
        public static SourceLocationRange create(
            SourceLocation start,
            SourceLocation end
        ) {
            return new SourceLocationRange(
                start,
                end
            );
        }

        // ----- Class methods -----

        /**
         * Wrap a source location range native value in the Java class.
         *
         * @param sourceLocationRangeNative The source location range NI
         * native value.
         * @return The newly created source location range or null if the
         * native value is null.
         */
        static SourceLocationRange wrap(
            SourceLocationRangeNative sourceLocationRangeNative
        ) {
            if(((PointerBase) sourceLocationRangeNative).isNull()) return null;
            else return new SourceLocationRange(
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

        // ----- Instance methods -----

        /**
         * Uwrap the source location range in the given NI pointer.
         *
         * @param sourceLocationRangeNative The NI pointer to the native
         * structure to fill.
         */
        public void unwrap(
            SourceLocationRangeNative sourceLocationRangeNative
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
    public static final class Diagnostic {

        // ----- Attributes -----

        /** The source location range of the diagnostic. */
        public final SourceLocationRange sourceLocationRange;

        /** The message of the diagnostic. */
        public final Text message;

        // ----- Constructors -----

        /**
         * Create a diagnostic from its content.
         *
         * @param sourceLocationRange The range of the diagnostic.
         * @param message The message of the diagnostic.
         */
        Diagnostic(
            SourceLocationRange sourceLocationRange,
            Text message
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
            SourceLocationRange sourceLocationRange,
            Text message
        ) {
            return new Diagnostic(
                sourceLocationRange,
                message
            );
        }

        // ----- Class methods -----

        /**
         * Wrap a diagnostic native value in the Java class.
         *
         * @param diagnosticNative The diagnostic NI native value.
         * @return The newly created diagnostic or null if the native value
         * is null.
         */
        static Diagnostic wrap(
            DiagnosticNative diagnosticNative
        ) {
            if(((PointerBase) diagnosticNative).isNull()) return null;
            else return new Diagnostic(
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

        // ----- Instance methods -----

        /**
         * Unwrap the diagnostic in the given NI pointer.
         *
         * @param diagnosticNative The pointer to the native structure to
         * fill.
         */
        public void unwrap(DiagnosticNative diagnosticNative) {
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

    ${java_doc('langkit.file_reader_type', 4)}
    public static final class FileReader {

        // ----- Attributes -----

        /** The reference to the file reader */
        public final PointerWrapper reference;

        // ----- Constructors -----

        FileReader(
            PointerWrapper reference
        ) {
            this.reference = reference;
        }

        // ----- Class methods -----

        static FileReader wrap(
            FileReaderNative fileReaderNative
        ) {
            if(((PointerBase) fileReaderNative).isNull()) return null;
            else return new FileReader(
                new PointerWrapper(fileReaderNative)
            );
        }

    }

    ${c_doc('langkit.unit_provider_type')}
    public static final class UnitProvider implements AutoCloseable {

        // ----- Attributes -----

        /** The reference to the unit provider */
        public final PointerWrapper reference;

        // ----- Constructors -----

        UnitProvider(
            PointerWrapper reference
        ) {
            this.reference = reference;
        }

        static UnitProvider wrap(
            UnitProviderNative unitProviderNative
        ) {
            if(((PointerBase) unitProviderNative).isNull()) return null;
            else return new UnitProvider(
                new PointerWrapper(unitProviderNative)
            );
        }

        // ----- Cleaning methods/classes -----

        /**
         * Release the native resource
         */
        private static void release(
            PointerWrapper providerRef
        ) {

            /*
            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("dec_ref_unit_provider")}(
                    providerRef.ni()
                );
            } else {
                JNI_LIB.${nat("dec_ref_unit_provider")}(
                    providerRef.jni()
                );
            }
            */

        }

        /** @see java.lang.AutoCloseable#close() */
        public void close() {
            // DO NOTHING FOR NOW
        }

    }

    ${java_doc('langkit.event_handler_type')}
    public static final class EventHandler {

        // ----- Attributes -----

        /** The reference to the event handler */
        public final PointerWrapper reference;

        // ----- Constructors -----

        EventHandler(
            PointerWrapper reference
        ) {
            this.reference = reference;
        }

        // ----- Class methods -----

        static EventHandler wrap(
            EventHandlerNative eventHandlerNative
        ) {
            if(((PointerBase) eventHandlerNative).isNull()) return null;
            else return new EventHandler(
                new PointerWrapper(eventHandlerNative)
            );
        }

    }

    ${java_doc('langkit.token_reference_type', 4)}
    public static class Token {

        // ----- Attributes -----

        /**
         * We only store the reference to the context to avoid ref-count
         * problems. To access token context go throught the analysis unit.
         */
        protected final PointerWrapper contextRef;

        /** The unit of the token. */
        public final AnalysisUnit unit;

        /** The pointer to the token data handler. */
        public final PointerWrapper tokenDataHandler;

        /** The index of the token. */
        public final int tokenIndex;

        /** The trivia index. */
        public final int triviaIndex;

        /** The kind of the token. */
        public final TokenKind kind;

        /** The text of the token. */
        public final Text text;

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
            PointerWrapper contextRef,
            AnalysisUnit unit,
            PointerWrapper tokenDataHandler,
            int tokenIndex,
            int triviaIndex,
            TokenKind kind,
            Text text,
            SourceLocationRange sourceLocationRange
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

        // ----- Getters -----

        public TokenKind getKind() {
            return this.kind;
        }

        public String getText() {
            return this.text.getContent();
        }

        public boolean isTrivia() {
            return this.triviaIndex != 0;
        }

        // ----- Class methods -----

        /**
         * Wrap a native token value in the Java class.
         *
         * @param tokenNative The native NI token value.
         * @param unit The analysis unit that owns the token.
         * @return The newly created token or null if the native value
         * is null.
         */
        static Token wrap(
            TokenNative tokenNative,
            AnalysisUnit unit
        ) {
            if(((PointerBase) tokenNative).isNull()) return null;
            else if(tokenNative.get_data().isNull())
                return NoToken.getInstance(unit);
            else return new Token(
                new PointerWrapper(tokenNative.get_context()),
                unit,
                new PointerWrapper(tokenNative.get_data()),
                tokenNative.get_token_index(),
                tokenNative.get_trivia_index(),
                TokenKind.fromC(tokenNative.get_kind()),
                new Text(
                    new PointerWrapper(tokenNative.get_text_chars()),
                    tokenNative.get_text_length(),
                    tokenNative.get_text_is_allocated() != 0
                ),
                new SourceLocationRange(
                    new SourceLocation(
                        tokenNative.get_start_line(),
                        tokenNative.get_start_column()
                    ),
                    new SourceLocation(
                        tokenNative.get_end_line(),
                        tokenNative.get_end_column()
                    )
                )
            );
        }

        /**
         * Get the text from the start token to the end token.
         *
         * @param start The start token.
         * @param end The end token.
         * @return The text between the two tokens.
         */
        @CompilerDirectives.TruffleBoundary
        public static String textRange(Token start, Token end) {
            Text resText;

            if(ImageInfo.inImageCode()) {
                TokenNative startNative = StackValue.get(TokenNative.class);
                start.unwrap(startNative);

                TokenNative endNative = StackValue.get(TokenNative.class);
                end.unwrap(endNative);

                TextNative textNative = StackValue.get(TextNative.class);
                Text.defaultValue(textNative);
                NI_LIB.${nat("token_range_text")}(
                    startNative,
                    endNative,
                    textNative
                );
                resText = Text.wrap(textNative);
            } else {
                resText = JNI_LIB.${nat("token_range_text")}(start, end);
            }

            String res = resText.getContent();
            resText.close();
            return res;
        }

        // ----- Instance methods -----

        /**
         * Get the next token.
         *
         * @return The next token in the source.
         */
        public Token next() {

            if(ImageInfo.inImageCode()) {
                TokenNative tokenNative = StackValue.get(TokenNative.class);
                this.unwrap(tokenNative);
                TokenNative nextNative = StackValue.get(TokenNative.class);
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
                TokenNative tokenNative = StackValue.get(TokenNative.class);
                this.unwrap(tokenNative);
                TokenNative previousNative = StackValue.get(
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
        public boolean isEquivalent(Token other) {

            if(ImageInfo.inImageCode()) {
                TokenNative leftNative = StackValue.get(TokenNative.class);
                this.unwrap(leftNative);
                TokenNative rightNative = StackValue.get(TokenNative.class);
                other.unwrap(rightNative);
                return NI_LIB.${nat("token_is_equivalent")}(
                    leftNative,
                    rightNative
                ) != 0;
            } else {
                return JNI_LIB.${nat("token_is_equivalent")}(this, other);
            }

        }

        /**
         * Unwrap the token in the given NI pointer.
         *
         * @param tokenNative The NI pointer to the native structure to
         * fill.
         */
        public void unwrap(TokenNative tokenNative) {
            tokenNative.set_context(this.contextRef.ni());
            tokenNative.set_data(this.tokenDataHandler.ni());
            tokenNative.set_token_index(this.tokenIndex);
            tokenNative.set_trivia_index(this.triviaIndex);
            tokenNative.set_kind(this.kind.toC());
            tokenNative.set_text_chars(this.text.charPointer.ni());
            tokenNative.set_text_length(this.text.length);
            tokenNative.set_text_is_allocated(this.text.isAllocated ? 1 : 0);
            tokenNative.set_start_line(this.sourceLocationRange.start.line);
            tokenNative.set_start_column(
                this.sourceLocationRange.start.column
            );
            tokenNative.set_end_line(this.sourceLocationRange.end.line);
            tokenNative.set_end_column(this.sourceLocationRange.end.column);
        }

        // ----- Override methods -----

        @Override
        @CompilerDirectives.TruffleBoundary
        public String toString() {
            return "<Token "
                + this.getKind().toString()
                + " "
                + this.sourceLocationRange.toString()
                + " text=\""
                + this.getText()
                + "\">";
        }

        @Override
        public boolean equals(Object o) {
            if(o == this) return true;
            if(!(o instanceof Token)) return false;
            Token other = (Token) o;
            return other.tokenDataHandler.equals(this.tokenDataHandler) &&
                    other.tokenIndex == this.tokenIndex &&
                    other.triviaIndex == this.triviaIndex;
        }

    }

    /**
     * This class represents the absence of token.
     */
    public static final class NoToken extends Token {

        // ----- Attributes -----

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
            PointerWrapper contextRef,
            AnalysisUnit unit
        ) {
            super(
                contextRef,
                unit,
                PointerWrapper.nullPointer(),
                0,
                0,
                TokenKind.fromC(-1),
                null,
                SourceLocationRange.NULL_LOCATION_RANGE
            );
        }

        /**
         * Get the no token instance for the given analysis unit.
         *
         * @param unit The unit to get the no token instance for.
         * @return The no token instance.
         */
        static NoToken getInstance(AnalysisUnit unit) {
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

        /**
         * Get the next token. In other words, the same no token.
         *
         * @return The instance.
         */
         @Override
        public Token next() {
            return this;
        }

        /**
         * Get the previous token. In other words, the same no token.
         *
         * @return The instance.
         */
        public Token previous() {
            return this;
        }

        /**
         * Check of the token is equivalent to the other one.
         *
         * @param other The other token to compare with.
         */
        public boolean isEquivalent(Token other) {
            return other instanceof NoToken;
        }

        /**
         * Unwrap the token in the given NI pointer.
         *
         * @param tokenNative The NI pointer to the native structure to
         * fill.
         */
        public void unwrap(TokenNative tokenNative) {
            tokenNative.set_context(this.contextRef.ni());
            tokenNative.set_data(this.tokenDataHandler.ni());
            tokenNative.set_token_index(this.tokenIndex);
            tokenNative.set_trivia_index(this.triviaIndex);
            tokenNative.set_kind(0);
            tokenNative.set_text_chars(
                (CIntPointer) WordFactory.nullPointer()
            );
            tokenNative.set_text_length(0);
            tokenNative.set_text_is_allocated(0);
            tokenNative.set_start_line(this.sourceLocationRange.start.line);
            tokenNative.set_start_column(
                this.sourceLocationRange.start.column
            );
            tokenNative.set_end_line(this.sourceLocationRange.end.line);
            tokenNative.set_end_column(this.sourceLocationRange.end.column);
        }

        // ----- Override methods -----

        @Override
        @CompilerDirectives.TruffleBoundary
        public String toString() {
            return "<Token "
                + this.getKind().toString()
                + " "
                + this.sourceLocationRange.toString()
                + " text=\""
                + this.getText()
                + "\">";
        }

        @Override
        public boolean equals(Object o) {
            return o == this;
        }

    }

    ${java_doc('langkit.analysis_context_type', 4)}
    public static final class AnalysisContext implements AutoCloseable {

        // ----- Attributes -----

        /** The reference to the native analysis context. */
        public final PointerWrapper reference;

        /** If the context has been closed. */
        private boolean closed;

        // ----- Constructors -----

        /**
         * Create a new analysis context from it value.
         *
         * @param reference The native value of the analysis context in
         * a pointer wrapper.
         */
        AnalysisContext(
            PointerWrapper reference
        ) {
            this.reference = reference;
            this.closed = false;

            if(ImageInfo.inImageCode()) {
                NI_LIB.${nat("context_incref")}(this.reference.ni());
            } else {
                JNI_LIB.${nat("context_incref")}(this.reference.jni());
            }
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
            String charset,
            FileReader fileReader,
            UnitProvider unitProvider,
            EventHandler eventHandler,
            boolean withTrivia,
            int tabStop
        ) {
            PointerWrapper reference;

            if(ImageInfo.inImageCode()) {
                CCharPointer charsetNative =
                    charset == null ?
                    WordFactory.nullPointer() :
                    toCString(charset);
                AnalysisContextNative resNative =
                    NI_LIB.${nat("allocate_analysis_context")}();

                NI_LIB.${nat("initialize_analysis_context")}(
                    resNative,
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
                reference = new PointerWrapper(resNative);
            } else {
                reference = JNI_LIB.${nat("create_analysis_context")}(
                    charset,
                    fileReader,
                    unitProvider,
                    eventHandler,
                    withTrivia,
                    tabStop
                );
            }

            this.reference = reference;
            this.closed = false;
        }

        /**
         * Create a new analysis context with the default parameters
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
            String charset,
            FileReader fileReader,
            UnitProvider unitProvider,
            EventHandler eventHandler,
            boolean withTrivia,
            int tabStop
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

        // ----- Cleaning methods/classes -----

        /** @see java.lang.AutoCloseable#close() */
        @Override
        public void close() {

            if(!this.closed) {
                if(ImageInfo.inImageCode()) {
                    NI_LIB.${nat("context_decref")}(this.reference.ni());
                } else {
                    JNI_LIB.${nat("context_decref")}(this.reference.jni());
                }
                this.closed = true;
            }

        }

        // ----- Class methods -----

        /**
         * Wrap a native pointer to the native analysis context in the
         * Java class.
         *
         * @param niPointer The pointer to the NI analysis context
         * native value.
         * @return The newly created analysis context or null if the given
         * pointer is null.
         */
        static AnalysisContext wrap(
            Pointer niPointer
        ) {
            if(niPointer.isNull()) return null;
            else return wrap((AnalysisContextNative) niPointer.readWord(0));
        }

        /**
         * Wrap an analysis context native value in the Java class.
         *
         * @param analysisContextNative The NI analysis context native value.
         * @return The newly created analysis context or null if the native
         * is null.
         */
        static AnalysisContext wrap(
            AnalysisContextNative analysisContextNative
        ) {
            if(((PointerBase) analysisContextNative).isNull()) return null;
            else return new AnalysisContext(
                new PointerWrapper(analysisContextNative)
            );
        }

        // ----- Instance methods -----

        /**
         * Get an analysis unit from the given file in the current context.
         *
         * @param fileName The file to get the analysis unit from.
         * @return The new analysis unit.
         */
        public AnalysisUnit getUnitFromFile(
            String fileName
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
         * @param reparse If the file should be reparsed.
         * @param rule The grammar rule to parse the source with.
         * @return The new analysis unit.
         */
        public AnalysisUnit getUnitFromFile(
            String fileName,
            String charset,
            boolean reparse,
            GrammarRule rule
        ) {

            if(ImageInfo.inImageCode()) {
                CCharPointer fileNameNative = toCString(fileName);
                CCharPointer charsetNative =
                    charset == null ?
                    WordFactory.nullPointer() :
                    toCString(charset);

                AnalysisUnitNative resNative =
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
            String buffer,
            String name
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
            String buffer,
            String name,
            String charset,
            GrammarRule rule
        ) {

            if(ImageInfo.inImageCode()) {
                CCharPointer bufferNative = toCString(buffer);
                CCharPointer nameNative = toCString(name);
                CCharPointer charsetNative =
                    charset == null ?
                    WordFactory.nullPointer() :
                    toCString(charset);

                AnalysisUnitNative resNative =
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

    }

    ${java_doc('langkit.analysis_unit_type', 4)}
    public static final class AnalysisUnit {

        // ----- Attributes -----

        /** The reference to the native analysis unit. */
        public final PointerWrapper reference;

        // ----- Constructors -----

        /**
         * Create a new analysis unit from its value.
         *
         * @param reference The native analysis unit native value in
         * a pointer wrapper.
         */
        AnalysisUnit(
            PointerWrapper reference
        ) {
            this.reference = reference;
        }

        // ----- Class methods -----

        /**
         * Wrap a pointer to the native analysis unit in the Java class.
         *
         * @param niPointer The pointer the native analysis unit value.
         * @return The newly created analysis unit or null if the given
         * pointer is null.
         */
        static AnalysisUnit wrap(
            Pointer niPointer
        ) {
            if(niPointer.isNull()) return null;
            else return wrap((AnalysisUnitNative) niPointer.readWord(0));
        }

        /**
         * Wrap a NI analysis unit native value in the Java class.
         *
         * @param analysisUnitNative The NI analysis unit native value.
         * @return The newly created analysis unit or null if the given
         * native value is null.
         */
        static AnalysisUnit wrap(
            AnalysisUnitNative analysisUnitNative
        ) {
            if(((PointerBase) analysisUnitNative).isNull()) return null;
            else return new AnalysisUnit(
                new PointerWrapper(analysisUnitNative)
            );
        }

        // ----- Instance methods -----

        /**
         * Get the root node of the analysis unit.
         *
         * @return The root node.
         */
        public ${root_node_type} getRoot() {
            Entity entity;

            if(ImageInfo.inImageCode()) {
                EntityNative entityNative = StackValue.get(
                    EntityNative.class
                );
                NI_LIB.${nat("unit_root")}(
                    this.reference.ni(),
                    entityNative
                );
                entity = Entity.wrap(entityNative);
            } else {
                entity = JNI_LIB.${nat("unit_root")}(
                    this
                );
            }

            return ${root_node_type}.fromEntity(entity);
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
        public String getFileName(boolean fullPath) {
            String absoluteFile;

            if(ImageInfo.inImageCode()) {
                CCharPointer resNative = NI_LIB.${nat("unit_filename")}(
                    this.reference.ni()
                );
                absoluteFile = toJString(resNative);
                UnmanagedMemory.free(resNative);
            } else {
                absoluteFile = JNI_LIB.${nat("unit_filename")}(this);
            }

            if(!fullPath) {
                File unitFile = new File(absoluteFile);
                return unitFile.getName();
            } else {
                return absoluteFile;
            }
        }

        /**
         * Get the number of tokens in the analysis unit.
         *
         * @return The number of token.
         */
        public int getTokenCount() {

            if(ImageInfo.inImageCode()) {
                return NI_LIB.${nat("unit_token_count")}(
                    this.reference.ni()
                );
            } else {
                return JNI_LIB.${nat("unit_token_count")}(
                    this
                );
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
                return JNI_LIB.${nat("unit_trivia_count")}(
                    this
                );
            }

        }

        /**
         * Return the first token of the analysis unit.
         *
         * @return The first token.
         */
        public Token getFirstToken() {

            if(ImageInfo.inImageCode()) {
                TokenNative tokenNative = StackValue.get(TokenNative.class);
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
                TokenNative tokenNative = StackValue.get(TokenNative.class);
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
            return Token.textRange(this.getFirstToken(), this.getLastToken());
        }

        /**
         * Get the analysis context that owns the unit.
         *
         * @return The owning analysis context.
         */
        public AnalysisContext getContext() {

            if(ImageInfo.inImageCode()) {
                AnalysisContextNative contextNative =
                    NI_LIB.${nat("unit_context")}(
                    this.reference.ni()
                );
                return AnalysisContext.wrap(contextNative);
            } else {
                return JNI_LIB.${nat("unit_context")}(this);
            }

        }

        /**
         * Get the list of assiated diagnositcs. Those are parsing errors.
         *
         * @return The diagnositcs of the unit.
         */
        public List<Diagnostic> getDiagnostics() {
            int diagnosticCount;

            if(ImageInfo.inImageCode()) {
                diagnosticCount = NI_LIB.${nat("unit_diagnostic_count")}(
                    this.reference.ni()
                );
            } else {
                diagnosticCount = JNI_LIB.${nat("unit_diagnostic_count")}(
                    this
                );
            }

            List<Diagnostic> res = new ArrayList<>(diagnosticCount);

            if(ImageInfo.inImageCode()) {
                DiagnosticNative diagnosticNative = StackValue.get(
                    DiagnosticNative.class
                );
                for(int i = 0 ; i < diagnosticCount ; i++) {
                    NI_LIB.${nat("unit_diagnostic")}(
                        this.reference.ni(),
                        i,
                        diagnosticNative
                    );
                    res.add(Diagnostic.wrap(diagnosticNative));
                }
            } else {
                for(int i = 0 ; i < diagnosticCount ; i++) {
                    res.add(
                        JNI_LIB.${nat("unit_diagnostic")}(
                            this,
                            i
                        )
                    );
                }
            }

            return res;
        }

        // ----- Override methods -----

        @Override
        public String toString() {
            return "<AnalysisUnit \"" + this.getFileName(false) + "\">";
        }

        @Override
        public boolean equals(Object o) {
            if(this == o) return true;
            if(!(o instanceof AnalysisUnit)) return false;
            AnalysisUnit other = (AnalysisUnit) o;
            return other.reference.equals(other.reference);
        }

    }

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

    // ===== Generated array wrapping classes =====

    /**
     * This class is the base of all array wrapping class.
     */
    public static abstract class ArrayBase<T> implements Iterable<T> {

        // ----- Attributes -----

        /** The content of the array. */
        protected final T[] content;

        // ----- Constructors -----

        /**
         * Protected constructor.
         */
        protected ArrayBase(
            T[] content
        ) {
            this.content = content;
        }

        // ----- Instance methods -----

        /**
         * Get the size of the array.
         *
         * @return The size of the native array.
         */
        public int size() {
            return this.content.length;
        }

        /**
         * Get the element at the given place in the array.
         *
         * @param i The index of the element to get.
         * @return The element at the given index.
         * @throws ArrayIndexOutOfBoundsException If the requested index is
         * out of bounds.
         */
        public T get(int i) {
            return this.content[i];
        }

        /**
         * Set the element at the given index.
         *
         * @param i The index of the element to set.
         * @param elem The element to place in the array.
         * @throws ArrayIndexOutOfBoundsException If the requested index is
         * out of bounds.
         */
        public void set(int i, T elem) {
            this.content[i] = elem;
        }

        /** @see java.lang.Iterable#iterator() */
        @Override
        public Iterator<T> iterator() {
            return new LangkitArrayIterator<T>(this);
        }

        // ----- Override methods -----

        @Override
        public String toString() {
            StringBuilder res = new StringBuilder("[");
            for(int i = 0 ; i < this.size() ; i++) {
                res.append(this.get(i).toString());
                if(i < this.size() - 1) res.append(", ");
            }
            res.append(']');
            return res.toString();
        }

        // ----- Inner classes -----

        /**
         * The iterator class for the langkit arrays
         */
        protected static class LangkitArrayIterator<U> implements Iterator<U> {

            // ----- Attributes -----

            /** The array to iterate on. */
            private final ArrayBase<U> array;

            /** The current index. */
            private int index;

            // ----- Constructors -----

            /**
             * Create a new array iterator.
             *
             * @param array The array to iterate on.
             */
            LangkitArrayIterator(
                ArrayBase<U> array
            ) {
                this.array = array;
                this.index = 0;
            }

            // ----- Instance methods -----

            /** @see java.util.Iterator#hasNext() */
            @Override
            public boolean hasNext() {
                return this.index < this.array.size();
            }

            /** @see java.util.Iterator#next() */
            @Override
            public U next() {
                return this.array.get(this.index++);
            }

        }

    }

    % for array_type in ctx.array_types:
        % if array_type.exposed and array_type.emit_c_type:
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

    ${java_doc('langkit.node_type', 4)}
    public static abstract class ${root_node_type} {

        // ----- Static -----

        ${ast_node.static_decl(T.root_node)}

        // ----- Attributes -----

        /** The entity of the node. */
        public final Entity entity;

        /** The analysis unit that owns the node. */
        private AnalysisUnit unit;

        /** The cache for the image of the node. */
        private String image;

        // ----- Constructors -----

        /**
         * Create a new node with its entity.
         *
         * @param entity The node's entity.
         */
        protected ${root_node_type}(
            Entity entity
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
            Entity entity
        ) {
            if(entity == null || entity.node.isNull()) {
                return null;
            } else {
                return dispatchNodeCreation(entity);
            }
        }

        /**
         * Dispatch the node creation to return the valid Java object
         * according to the node kind.
         *
         * @param entity The entity to create the node from.
         * @return The wrapped node in the correct class.
         */
        protected static ${root_node_type} dispatchNodeCreation(
            Entity entity
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
                    return new ${api.wrapping_type(subclass)}(entity);
                    % endif
                % endfor
                default:
                    throw new EnumException(
                        "Cannot find the node type from " + nodeKind
                    );
            }
        }

        // ----- Getters -----

        public String getKindName() {
            return ${root_node_type}.kindName;
        }

        public String[] getFieldNames() {
            return ${root_node_type}.fieldNames;
        }

        public boolean isListType() {
            return ${root_node_type}.isListType;
        }

        public boolean isTokenNode() {

            if(ImageInfo.inImageCode()) {
                EntityNative entityNative = StackValue.get(
                    EntityNative.class
                );
                this.entity.unwrap(entityNative);
                return NI_LIB.${nat("node_is_token_node")}(
                    entityNative
                ) != 0;
            } else {
                return JNI_LIB.${nat("node_is_token_node")}(this.entity);
            }

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
                    EntityNative entityNative = StackValue.get(
                        EntityNative.class
                    );
                    this.entity.unwrap(entityNative);
                    AnalysisUnitNative unitNative = NI_LIB.${nat("node_unit")}(
                        entityNative
                    );
                    this.unit = AnalysisUnit.wrap(unitNative);
                } else {
                    this.unit = JNI_LIB.${nat("node_unit")}(this.entity);
                }

            }
            return this.unit;
        }

        /**
         * Get the descritpion of a field from its name.
         *
         * @param name The langkit field name to get the description for.
         * @return The Java description of the langkit field.
         */
        @CompilerDirectives.TruffleBoundary
        public ${ctx.lib_name.camel}Field getFieldDescription(String name) {
            return ${root_node_type}.fieldDescriptions.getOrDefault(name, null);
        }

        /**
         * Get the children count of the node.
         *
         * @return The children count.
         */
        public int getChildrenCount() {

            if(ImageInfo.inImageCode()) {
                EntityNative thisNative = StackValue.get(EntityNative.class);
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
         * @return The child at the given index or null.
         */
        public ${root_node_type} getChild(int n) {

            if(ImageInfo.inImageCode()) {
                EntityNative thisNative = StackValue.get(EntityNative.class);
                this.entity.unwrap(thisNative);
                EntityNative resNative = StackValue.get(EntityNative.class);
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
            Text resText;

            if(ImageInfo.inImageCode()) {
                EntityNative thisNative = StackValue.get(EntityNative.class);
                this.entity.unwrap(thisNative);
                TextNative resNative = StackValue.get(TextNative.class);
                Text.defaultValue(resNative);
                NI_LIB.${nat("node_text")}(
                    thisNative,
                    resNative
                );
                resText = Text.wrap(resNative);
            } else {
                resText = JNI_LIB.${nat("node_text")}(
                    this.entity
                );
            }

            String res = resText.toString();
            resText.close();
            return res;
        }

        /**
         * Get the image of the node.
         *
         * @return The node's image.
         */
        public String getImage() {
            if(this.image == null) {
                Text resText;

                if(ImageInfo.inImageCode()) {
                    EntityNative thisNative = StackValue.get(
                        EntityNative.class
                    );
                    this.entity.unwrap(thisNative);
                    TextNative resNative = StackValue.get(TextNative.class);
                    Text.defaultValue(resNative);
                    NI_LIB.${nat("node_image")}(
                        thisNative,
                        resNative
                    );
                    resText = Text.wrap(resNative);
                } else {
                    resText = JNI_LIB.${nat("node_image")}(this.entity);
                }

                this.image = resText.toString();
                resText.close();
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
                EntityNative thisNative = StackValue.get(
                    EntityNative.class
                );
                this.entity.unwrap(thisNative);
                SourceLocationRangeNative resNative = StackValue.get(
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

        // ----- Dumping methods -----

        /**
         * Return the AST in a string.
         *
         * @return The string containing the representation of the AST
         * from the node.
         */
        @CompilerDirectives.TruffleBoundary
        public String dump() {
            StringBuilder builder = new StringBuilder();
            this.dump(builder);
            return builder.toString();
        }

        /**
         * Dump the AST in the given string builder.
         *
         * @param builder The builder to dump the AST in.
         */
        @CompilerDirectives.TruffleBoundary
        public void dump(StringBuilder builder) {
            this.dump(builder, 0);
        }

        /**
         * Dump the AST in the given string builder with the indent level.
         *
         * @param builder The builder to dump the AST in.
         * @param indent The starting indent level.
         */
        @CompilerDirectives.TruffleBoundary
        public void dump(StringBuilder builder, int indent) {
            // Indent and add the node name and position
            indent(builder, indent);
            builder.append(this.getKindName())
                .append(" <")
                .append(this.getSourceLocationRange().toString())
                .append('>');

            // If the node is a token node add its text to the builder
            if(this.isTokenNode()) {
                builder.append(" : ").append(this.getText());
            }

            // If the node is a list type, display all the children
            if(this.isListType()) {
                builder.append(" [list_node]");
                int childrenCount = this.getChildrenCount();
                for(int i = 0 ; i < childrenCount ; i++) {
                    builder.append('\n');
                    indent(builder, indent);
                    builder.append("|-[").append(i).append("]\n");
                    this.getChild(i).dump(builder, indent + 1);
                }

                // If there is nothing in the list
                if(childrenCount == 0) {
                    builder.append(" EMPTY");
                }
            }

            // Else display all fields of the node
            else {
                for(String field : this.getFieldNames()) {
                    try {
                        Method fieldGetter = this.getClass().getMethod(field);
                        ${root_node_type} node =
                            (${root_node_type}) fieldGetter.invoke(this);
                        if(node != null) {
                            builder.append("\n");
                            indent(builder, indent);
                            builder.append("|-").append(field).append("\n");
                            node.dump(builder, indent + 1);
                        }
                    } catch(Exception e) {
                        e.printStackTrace();
                        System.err.println("This should not happen");
                    }
                }
            }
        }

        /**
         * Indent the given string builder at the given level.
         *
         * @param builder The string builder to indent.
         * @param level The level of indentation.
         */
        protected static void indent(StringBuilder builder, int level) {
            for(int i = 0 ; i < level * 4 ; i++) {
                if(i % 4 == 0) builder.append("|"); else builder.append(" ");
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
            ${root_node_type} other = (${root_node_type}) o;
            return this.entity.node.equals(other.entity.node) &&
                this.entity.info.rebindings.equals(
                    other.entity.info.rebindings
                );
        }

        @Override
        public int hashCode() {

            if(ImageInfo.inImageCode()) {
                return Objects.hash(
                    this.entity.node.ni().rawValue(),
                    this.entity.info.rebindings.ni().rawValue()
                );
            } else {
                return Objects.hash(
                    this.entity.node.jni(),
                    this.entity.info.rebindings.jni()
                );
            }

        }

    }

    // ===== Generated AST node wrapping classes =====

    % for astnode in ctx.astnode_types:
        % if astnode != T.root_node:
    ${ast_node.wrapping_class(astnode)}
        % endif
    % endfor

}
