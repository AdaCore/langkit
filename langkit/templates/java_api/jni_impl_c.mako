<%namespace name="enum" file="enum.mako" />
<%namespace name="ast_node" file="ast_node.mako" />
<%namespace name="struct" file="struct.mako" />
<%namespace name="array" file="array.mako" />
<%namespace name="iterator" file="iterator.mako" />
<%namespace name="exts" file="/extensions.mako" />
<%
api = java_api
nat = c_api.get_name

h_file = f"com_adacore_{ctx.lib_name.lower}_{ctx.lib_name.camel}_JNI_LIB.h"
lib_file = f"{ctx.lib_name.lower}.h"
sig_base = f"com/adacore/{ctx.lib_name.lower}/{ctx.lib_name.camel}"
ptr_sig = f"{sig_base}$PointerWrapper"
%>

// This file contains the native implementations for JNI
// functions used in ${ctx.lib_name.camel}.java

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <jni.h>

#include "${h_file}"
#include "${lib_file}"

// ==========
// Function delcarations
// ==========

void * PointerWrapper_new_value();
jobject PointerWrapper_wrap(JNIEnv *, void *);
void * PointerWrapper_unwrap(JNIEnv *, jobject);

${exception_type} LangkitException_new_value();
jthrowable LangkitException_wrap(JNIEnv *, ${exception_type});

% for enum_type in ctx.enum_types:
${enum.jni_c_decl(enum_type)}
% endfor

uint32_t Char_new_value();
jobject Char_wrap(JNIEnv *, uint32_t);
uint32_t Char_unwrap(JNIEnv *, jobject);

${big_integer_type} BigInteger_new_value();
jobject BigInteger_wrap(JNIEnv *, ${big_integer_type});
${big_integer_type} BigInteger_unwrap(JNIEnv *, jobject);

${symbol_type} Symbol_new_value();
jobject Symbol_wrap(JNIEnv *, ${symbol_type});
${symbol_type} Symbol_unwrap(JNIEnv *, jobject, ${analysis_context_type});
jthrowable new_symbol_exception(JNIEnv *, jstring);

${string_type} StringWrapper_new_value();
jobject StringWrapper_wrap(JNIEnv *, ${string_type});
${string_type} StringWrapper_unwrap(JNIEnv *, jobject);

${text_type} Text_new_value();
jobject Text_wrap(JNIEnv *, ${text_type});
${text_type} Text_unwrap(JNIEnv *, jobject);
jstring get_text_content(JNIEnv *, jobject);
jobject text_from_content(JNIEnv *, jstring);

${sloc_type} SourceLocation_new_value();
jobject SourceLocation_wrap(JNIEnv *, ${sloc_type});
${sloc_type} SourceLocation_unwrap(JNIEnv *, jobject);

${sloc_range_type} SourceLocationRange_new_value();
jobject SourceLocationRange_wrap(JNIEnv *, ${sloc_range_type});
${sloc_range_type} SourceLocationRange_unwrap(JNIEnv *, jobject);

${diagnostic_type} Diagnostic_new_value();
jobject Diagnostic_wrap(JNIEnv *, ${diagnostic_type});
${diagnostic_type} Diagnostic_unwrap(JNIEnv *, jobject);

${file_reader_type} FileReader_new_value();
jobject FileReader_wrap(JNIEnv *, ${file_reader_type});
${file_reader_type} FileReader_unwrap(JNIEnv *, jobject);

${unit_provider_type} UnitProvider_new_value();
jobject UnitProvider_wrap(JNIEnv *, ${unit_provider_type});
${unit_provider_type} UnitProvider_unwrap(JNIEnv *, jobject);

${event_handler_type} EventHandler_new_value();
jobject EventHandler_wrap(JNIEnv *, ${event_handler_type});
${event_handler_type} EventHandler_unwrap(JNIEnv *, jobject);

${token_type} Token_new_value();
jobject Token_wrap(JNIEnv *, ${token_type}, jobject);
${token_type} Token_unwrap(JNIEnv *, jobject);
jobject Token_get_unit(JNIEnv *, jobject);
jobject NoToken_wrap(JNIEnv *, jobject);

${analysis_context_type} AnalysisContext_new_value();
jobject AnalysisContext_wrap(JNIEnv *, ${analysis_context_type});
${analysis_context_type} AnalysisContext_unwrap(JNIEnv *, jobject);

${analysis_unit_type} AnalysisUnit_new_value();
jobject AnalysisUnit_wrap(JNIEnv *, ${analysis_unit_type});
${analysis_unit_type} AnalysisUnit_unwrap(JNIEnv *, jobject);

% for struct_type in ctx.struct_types:
    % if struct_type.is_entity_type:
        % if struct_type is root_entity:
${struct.jni_c_decl(struct_type)}
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
${struct.jni_c_decl(struct_type)}
        % endif
    % endif
% endfor

% for array_type in ctx.array_types:
    % if array_type.exposed and array_type.emit_c_type:
${array.jni_c_decl(array_type)}
    % endif
% endfor

% for iterator_type in ctx.iterator_types:
    % if iterator_type.exposed and iterator_type.emit_c_type:
${iterator.jni_c_decl(iterator_type)}
    % endif
% endfor

// ==========
// Util functions
// ==========

// Get the native reference in a Java object
void * get_reference(
    JNIEnv *env,
    jobject object
) {
    // Null protection
    if(object == NULL) return NULL;

    // Get the object class
    jclass clazz = (*env)->GetObjectClass(env, object);

    // Get the reference field
    jfieldID reference_field = (*env)->GetFieldID(
        env,
        clazz,
        "reference",
        "L${ptr_sig};"
    );
    jobject reference = (*env)->GetObjectField(env, object, reference_field);

    // Unwrap the reference field to get pointer
    return PointerWrapper_unwrap(env, reference);
}

// Get the analysis unit of the given node
jobject get_node_unit(
    JNIEnv *env,
    jobject entity
) {
    // Unwrap the entity
    ${entity_type} native_entity = Entity_unwrap(env, entity);

    // Call the native function
    ${analysis_unit_type} native_unit = ${nat("node_unit")}(
        &native_entity
    );

    // Return the wrapped analysis unit
    return AnalysisUnit_wrap(env, native_unit);
}

// Translate a Java string to a C char pointer with the UTF-8 encoding
const char * to_c_string(
    JNIEnv *env,
    jstring j_string
) {
    return (*env)->GetStringUTFChars(env, j_string, NULL);
}

// Create a Java string from a C char pointer
jstring to_j_string(
    JNIEnv *env,
    const char *c_string
) {
    return (*env)->NewStringUTF(env, c_string);
}

// ==========
// Language specific extensions
// ==========

${exts.include_extension(ctx.ext("java_api", "jni_impl"))}

// ==========
// Custom pointer functions
// ==========

// Create a new value for a custom pointer
void * PointerWrapper_new_value() {
    return NULL;
}

// Wrap a C pointer in a Java custom pointer
jobject PointerWrapper_wrap(
    JNIEnv *env,
    void *pointer
) {
    // Get the custom pointer class
    jclass clazz = (*env)->FindClass(env, "${ptr_sig}");
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(J)V"
    );

    // Create the new custom pointer
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        (jlong) pointer
    );
}

// Get the C pointer from the Java custom pointer
void * PointerWrapper_unwrap(
    JNIEnv *env,
    jobject custom_pointer
) {
    // Null protection
    if(custom_pointer == NULL) return NULL;

    // Get the custom pointer class
    jclass clazz = (*env)->GetObjectClass(env, custom_pointer);

    // Get the getting method
    jmethodID getter = (*env)->GetMethodID(
        env,
        clazz,
        "jni",
        "()J"
    );

    // Return the C pointer
    return (void *) (*env)->CallLongMethod(
        env,
        custom_pointer,
        getter
    );
}

// ==========
// Exception functions
// ==========

// Create a new value for an exception
${exception_type} LangkitException_new_value() {
    ${exception_type} res = {
        0,
        NULL
    };
    return res;
}

// Wrap a native langkit exception in a Java exception
jthrowable LangkitException_wrap(
    JNIEnv *env,
    ${exception_type} exception
) {
    // Get the langkit exception Java class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$LangkitException");

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(ILjava/lang/String;)V"
    );

    // Return the new exception instance
    return (jthrowable) (*env)->NewObject(
        env,
        clazz,
        constructor,
        (jint) exception.kind,
        to_j_string(env, exception.information)
    );
}

${api.jni_func_sig("get_last_exception", "jthrowable")}(
    JNIEnv *env,
    jclass jni_lib
) {
    // Call the native function
    const ${exception_type} *last_exception = ${nat("get_last_exception")}();

    // If last exception is null, return a null value, else wrap it
    if(last_exception == NULL) {
        return NULL;
    }
    return LangkitException_wrap(
        env,
        *last_exception
    );
}

// ==========
// Token kind functions
// ==========

// Create a new value for a token kind
${token_kind} TokenKind_new_value() {
    return 0;
}

// Wrap a native token kind in a Java class
jobject TokenKind_wrap(
    JNIEnv *env,
    ${token_kind} enum_value_native
) {
    // Get the token kind class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$TokenKind");

    // Get the constructing static method
    jmethodID from_c_method = (*env)->GetStaticMethodID(
        env,
        clazz,
        "fromC",
        "(I)L${sig_base}$TokenKind;"
    );

    // Call the static method
    return (*env)->CallStaticObjectMethod(
        env,
        clazz,
        from_c_method,
        (jint) enum_value_native
    );
}

// Unwrap a Java token kind as a native one
${token_kind} TokenKind_unwrap(
    JNIEnv *env,
    jobject enum_value
) {
    // Get the object class
    jclass clazz = (*env)->GetObjectClass(env, enum_value);

    // Get the method
    jmethodID to_c_method = (*env)->GetMethodID(
        env,
        clazz,
        "toC",
        "()I"
    );

    // Call the Java method
    return (${token_kind}) (*env)->CallIntMethod(
        env,
        enum_value,
        to_c_method
    );
}

// ==========
// Enumeration generated functions
// ==========

% for enum_type in ctx.enum_types:
${enum.jni_c_impl(enum_type)}
% endfor

// ==========
// Character functions
// ==========

// Create a new value for a character
uint32_t Char_new_value() {
    return 0;
}

// Wrap a native character in the Java wrapping class
jobject Char_wrap(
    JNIEnv *env,
    uint32_t native_character
) {
    // Get the char class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$Char");

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(I)V"
    );

    // Return the new object
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        native_character
    );
}

// Get a native character from a Java wrapping instance
uint32_t Char_unwrap(
    JNIEnv *env,
    jobject character
) {
    // Get the object class
    jclass clazz = (*env)->GetObjectClass(env, character);

    // Get the value field
    jfieldID value_field = (*env)->GetFieldID(
        env,
        clazz,
        "value",
        "I"
    );

    // Return the result field
    return (uint32_t) (*env)->GetIntField(
        env,
        character,
        value_field
    );
}

// ==========
// Big integer functions
// ==========

// Create a new value for a big integer
${big_integer_type} BigInteger_new_value() {
    return NULL;
}

// Wrap a native big integer in the Java class
jobject BigInteger_wrap(
    JNIEnv *env,
    ${big_integer_type} native_bi
) {
    // Check the nullity
    if(native_bi == NULL) return NULL;

    // Get the big integer class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$BigInteger");

    // Get the object constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(L${ptr_sig};)V"
    );

    // Return the new big integer
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        PointerWrapper_wrap(env, (void *) native_bi)
    );
}

// Get the native big integer from the Java wrapping instance
${big_integer_type} BigInteger_unwrap(
    JNIEnv *env,
    jobject big_integer
) {
    return (${big_integer_type}) get_reference(env, big_integer);
}

// Create a big integer from its text
${api.jni_func_sig("create_big_integer", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject text
) {
    // Get the native text
    ${text_type} native_text = Text_unwrap(env, text);

    // Call the native function
    ${big_integer_type} res = ${nat("create_big_integer")}(
        &native_text
    );

    // Return the big integer
    return BigInteger_wrap(env, res);
}

// Get the text from a big integer
${api.jni_func_sig("big_integer_text", "jobject")} (
    JNIEnv *env,
    jclass jni_lib,
    jobject big_integer
) {
    // Create the text result struct
    ${text_type} res_struct = Text_new_value();

    // Call the native function
    ${nat("big_integer_text")}(
        BigInteger_unwrap(env, big_integer),
        &res_struct
    );

    // Return the text Java object
    return Text_wrap(env, res_struct);
}

// Decrease the reference of a big integer
${api.jni_func_sig("big_integer_decref", "void")} (
    JNIEnv *env,
    jclass jni_lib,
    jlong big_integer
) {
    // Just call the native function with the reference
    ${nat("big_integer_decref")}((${big_integer_type}) big_integer);
}

// ==========
// Symbol functions
// ==========

// Create a new value for a symbol
${symbol_type} Symbol_new_value() {
    ${symbol_type} res = {
        NULL,
        NULL
    };
    return res;
}

// Wrap a native symbol in the Java class
jobject Symbol_wrap(
    JNIEnv *env,
    ${symbol_type} native_symbol
) {
    // Get the symbol class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$Symbol");

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(Ljava/lang/String;)V"
    );

    // Get the text of the symbol
    ${text_type} text_native = Text_new_value();
    ${nat("symbol_text")}(
        &native_symbol,
        &text_native
    );
    jobject text = Text_wrap(env, text_native);
    jstring symbol_text = get_text_content(env, text);

    // Destroy the text
    ${nat("destroy_text")}(
        &text_native
    );

    // Return the new symbol
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        symbol_text
    );
}

// Get the native symbol from the Java wrapping instance
${symbol_type} Symbol_unwrap(
    JNIEnv *env,
    jobject symbol,
    ${analysis_context_type} context_native
) {
    // Create the result structure
    ${symbol_type} res = Symbol_new_value();

    // Get the object class
    jclass clazz = (*env)->GetObjectClass(env, symbol);

    // Get the fields id
    jfieldID text_field = (*env)->GetFieldID(
        env,
        clazz,
        "text",
        "Ljava/lang/String;"
    );

    // Get the fields value
    jstring str = (jstring) (*env)->GetObjectField(env, symbol, text_field);

    // Create a text from the symbol content
    jobject text = text_from_content(env, str);
    ${text_type} text_native = Text_unwrap(env, text);

    // Call the symbol creation
    int ret_code = ${nat("context_symbol")}(
        context_native,
        &text_native,
        &res
    );

    if(ret_code == 0) {
        // Throw a new exception
        jthrowable exception = new_symbol_exception(env, str);
        (*env)->Throw(env, exception);
    }

    // Return the result
    return res;
}

// Create a new symbol exception
jthrowable new_symbol_exception(
    JNIEnv *env,
    jstring symbol_str
) {
    // Get the symbol exception class
    jclass clazz = (*env)->FindClass(
        env,
        "${sig_base}$SymbolException"
    );

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(Ljava/lang/String;)V"
    );

    // Return the exception
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        symbol_str
    );
}

// ==========
// String functions
// ==========

// Create a new value for a langkit string
${string_type} StringWrapper_new_value() {
    return NULL;
}

// Wrap a native langkit string in the Java wrapper class
jobject StringWrapper_wrap(
    JNIEnv *env,
    ${string_type} native_string
) {
    // Get the string wrapper class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$StringWrapper");

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(L${ptr_sig};[I)V"
    );

    // Get the string content
    jintArray content = (*env)->NewIntArray(
        env,
        (jsize) native_string->length
    );
    (*env)->SetIntArrayRegion(
        env,
        content,
        0,
        (jsize) native_string->length,
        (jint *) native_string->content
    );

    // Return the new string wrapper
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        PointerWrapper_wrap(env, (void *) native_string),
        content
    );
}

// Get the native langkit string from a Java wrapping instance
${string_type} StringWrapper_unwrap(
    JNIEnv *env,
    jobject string
) {
    return (${string_type}) get_reference(env, string);
}

// Create a new string wrapper
${api.jni_func_sig("create_string", "jobject")} (
    JNIEnv *env,
    jclass jni_lib,
    jbyteArray content
) {
    // Get the content length
    int length = (int) (*env)->GetArrayLength(env, content) / 4;
    uint32_t *native_content = (uint32_t *) malloc(
        length * sizeof(uint32_t)
    );
    (*env)->GetByteArrayRegion(
        env,
        content,
        0,
        length * 4,
        (jbyte *) native_content
    );

    // Call the native method
    ${string_type} res_native = ${nat("create_string")}(
        native_content,
        length
    );

    // Free the native array
    free(native_content);

    // Wrap the string wrapper
    return StringWrapper_wrap(env, res_native);
}

// Decrease the reference counting on a string
${api.jni_func_sig("string_dec_ref", "void")} (
    JNIEnv *env,
    jclass jni_lib,
    jlong string
) {
    ${nat("string_dec_ref")}((${string_type}) string);
}

// ==========
// Text functions
// ==========

// Create a new value for a langkit text
${text_type} Text_new_value() {
    ${text_type} res = {
        NULL,
        0,
        0
    };
    return res;
}

// Wrap a langkit text in the Java wrapping class
jobject Text_wrap(
    JNIEnv *env,
    ${text_type} text_native
) {
    // Get the text class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$Text");

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(L${ptr_sig};JZ[I)V"
    );

    // Get the int array from the structure and translate it into Java array
    jintArray content = (*env)->NewIntArray(env, (jsize) text_native.length);
    (*env)->SetIntArrayRegion(
        env,
        content,
        0,
        (jsize) text_native.length,
        (jint *) text_native.chars
    );

    // Return the new text
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        PointerWrapper_wrap(env, text_native.chars),
        (jlong) text_native.length,
        (jboolean) text_native.is_allocated,
        content
    );
}

// Unwrap a langit text from a Java wrapping instance
${text_type} Text_unwrap(
    JNIEnv *env,
    jobject text
) {
    // Create the result structure
    ${text_type} res = Text_new_value();

    // Get the object class
    jclass clazz = (*env)->GetObjectClass(env, text);

    // Get the field ids
    jfieldID chars_field = (*env)->GetFieldID(
        env,
        clazz,
        "charPointer",
        "L${ptr_sig};"
    );
    jfieldID length_field = (*env)->GetFieldID(
        env,
        clazz,
        "length",
        "J"
    );
    jfieldID is_allocated_field = (*env)->GetFieldID(
        env,
        clazz,
        "isAllocated",
        "Z"
    );

    // Get the values
    jobject chars = (*env)->GetObjectField(
        env,
        text,
        chars_field
    );
    jlong length = (*env)->GetLongField(
        env,
        text,
        length_field
    );
    jboolean is_allocated = (*env)->GetBooleanField(
        env,
        text,
        is_allocated_field
    );

    // Fill the structure with the object fields
    res.chars = (uint32_t *) PointerWrapper_unwrap(env, chars);
    res.length = (size_t) length;
    res.is_allocated = (int) is_allocated;

    // Return the result
    return res;
}

// Get the string from a text object
jstring get_text_content(
    JNIEnv *env,
    jobject text
) {
    // Get the text class
    jclass clazz = (*env)->GetObjectClass(env, text);

    // Get the method
    jmethodID get_content_method = (*env)->GetMethodID(
        env,
        clazz,
        "getContent",
        "()Ljava/lang/String;"
    );

    return (jstring) (*env)->CallObjectMethod(
        env,
        text,
        get_content_method
    );
}

// Create a text object from its string content
jobject text_from_content(
    JNIEnv *env,
    jstring content
) {
    // Get the text class
    jclass clazz = (*env)->FindClass(
        env,
        "${sig_base}$Text"
    );

    // Get the creationg method
    jmethodID create_method = (*env)->GetStaticMethodID(
        env,
        clazz,
        "create",
        "(Ljava/lang/String;)L${sig_base}$Text;"
    );

    // Call the creating method
    return (*env)->CallStaticObjectMethod(
        env,
        clazz,
        create_method,
        content
    );
}

// Create a text Java object from its content
${api.jni_func_sig("create_text", "jobject")} (
    JNIEnv *env,
    jclass jni_lib,
    jbyteArray content_utf32
) {
    // Get the content in a memory buffer
    size_t length = ((size_t) (*env)->GetArrayLength(env, content_utf32)) / 4;
    uint32_t *content_native = (uint32_t *) malloc(length * sizeof(uint32_t));
    (*env)->GetByteArrayRegion(
        env,
        content_utf32,
        0,
        length * 4,
        (jbyte *) content_native
    );

    // Get the text class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$Text");

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(L${ptr_sig};JZZ[I)V"
    );

    // Create the Java array of the content
    jintArray content = (*env)->NewIntArray(env, (jsize) length);
    (*env)->SetIntArrayRegion(
        env,
        content,
        0,
        (jsize) length,
        (jint *) content_native
    );

    // Return the new text
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        PointerWrapper_wrap(env, content_native),
        (jlong) length,
        (jboolean) 0,
        (jboolean) 1,
        content
    );
}

// Destroy a text object
${api.jni_func_sig("destroy_text", "void")} (
    JNIEnv *env,
    jclass jni_lib,
    jlong text_buffer,
    jlong length,
    jboolean is_allocated,
    jboolean is_owner
) {
    // If the object is the buffer owner
    if(is_owner) {
        free((void *) text_buffer);
    } else {
        // Create the new text structure
        ${text_type} text_native = Text_new_value();
        text_native.chars = (uint32_t *) text_buffer;
        text_native.length = (long) length;
        text_native.is_allocated = (int) is_allocated;

        // Call the destry function
        ${nat("destroy_text")}(
            &text_native
        );
    }

}

// ==========
// Source location functions
// ==========

// Create a new value for a source location
${sloc_type} SourceLocation_new_value() {
    ${sloc_type} res = {
        0,
        0
    };
    return res;
}

// Wrap a native source location in the Java wrapping class
jobject SourceLocation_wrap(
    JNIEnv *env,
    ${sloc_type} sloc_native
) {
    // Get the source location class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$SourceLocation");

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(IS)V"
    );

    // Return the new source location
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        (jint) sloc_native.line,
        (jshort) sloc_native.column
    );
}

// Get a native source location from a Java wrapping instance
${sloc_type} SourceLocation_unwrap(
    JNIEnv *env,
    jobject sloc
) {
    // Create the result structure
    ${sloc_type} res = SourceLocation_new_value();

    // Get the object class
    jclass clazz = (*env)->GetObjectClass(env, sloc);

    // Get the fields id
    jfieldID line_field = (*env)->GetFieldID(env, clazz, "line", "I");
    jfieldID column_field = (*env)->GetFieldID(env, clazz, "column", "S");

    // Get the fields value
    jint line = (*env)->GetIntField(env, sloc, line_field);
    jshort column = (*env)->GetShortField(env, sloc, column_field);

    // Fill the result structure
    res.line = (uint32_t) line;
    res.column = (uint16_t) column;

    // Return the result
    return res;
}

// ==========
// Source location range functions
// ==========

// Create a new value for a source location range
${sloc_range_type} SourceLocationRange_new_value() {
    ${sloc_range_type} res = {
        SourceLocation_new_value(),
        SourceLocation_new_value()
    };
    return res;
}

// Wrap a native source location range in the Java wrapping class
jobject SourceLocationRange_wrap(
    JNIEnv *env,
    ${sloc_range_type} native_slocr
) {
    // Get the source location range class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$SourceLocationRange");

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(L${sig_base}$SourceLocation;L${sig_base}$SourceLocation;)V"
    );

    // Return the new source location range
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        SourceLocation_wrap(env, native_slocr.start),
        SourceLocation_wrap(env, native_slocr.end)
    );
}

// Get a native source location range from a Java wrapping instance
${sloc_range_type} SourceLocationRange_unwrap(
    JNIEnv *env,
    jobject slocr
) {
    // Create the result structure
    ${sloc_range_type} res = SourceLocationRange_new_value();

    // Get the object class
    jclass clazz = (*env)->GetObjectClass(env, slocr);

    // Get the fields id
    jfieldID start_field = (*env)->GetFieldID(
        env,
        clazz,
        "start",
        "L${sig_base}$SourceLocation;"
    );
    jfieldID end_field = (*env)->GetFieldID(
        env,
        clazz,
        "end",
        "L${sig_base}$SourceLocation;"
    );

    // Get the fields value
    jobject start = (*env)->GetObjectField(
        env,
        slocr,
        start_field
    );
    jobject end = (*env)->GetObjectField(
        env,
        slocr,
        end_field
    );

    // Fill the result structure
    res.start = SourceLocation_unwrap(env, start);
    res.end = SourceLocation_unwrap(env, end);

    // Return the result
    return res;
}

// ==========
// Diagnostic functions
// ==========

// Create a new value for a diagnostic
${diagnostic_type} Diagnostic_new_value() {
    ${diagnostic_type} res = {
        SourceLocationRange_new_value(),
        Text_new_value()
    };
    return res;
}

// Wrap a native diagnostic in the Java wrapping class
jobject Diagnostic_wrap(
    JNIEnv *env,
    ${diagnostic_type} native_diagnostic
) {
    // Get the diagnostic class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$Diagnostic");

    // Get the class constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(L${sig_base}$SourceLocationRange;L${sig_base}$Text;)V"
    );

    // Return the new diagnostic
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        SourceLocationRange_wrap(env, native_diagnostic.sloc_range),
        Text_wrap(env, native_diagnostic.message)
    );
}

// Get a native diagnostic from a Java wrapping instance
${diagnostic_type} Diagnostic_unwrap(
    JNIEnv *env,
    jobject diagnostic
) {
    // Create the result structure
    ${diagnostic_type} res = Diagnostic_new_value();

    // Get the object class
    jclass clazz = (*env)->GetObjectClass(env, diagnostic);

    // Get the fields id
    jfieldID slocr_field = (*env)->GetFieldID(
        env,
        clazz,
        "sourceLocationRange",
        "L${sig_base}$SourceLocationRange;"
    );
    jfieldID text_field = (*env)->GetFieldID(
        env,
        clazz,
        "message",
        "L${sig_base}$Text;"
    );

    // Get the fields value
    jobject slocr = (*env)->GetObjectField(
        env,
        diagnostic,
        slocr_field
    );
    jobject text = (*env)->GetObjectField(
        env,
        diagnostic,
        text_field
    );

    // Fill the result structure
    res.sloc_range = SourceLocationRange_unwrap(env, slocr);
    res.message = Text_unwrap(env, text);

    // Return the result
    return res;
}

// ==========
// File reader functions
// ==========

// Create a new value for a file reader
${file_reader_type} FileReader_new_value() {
    return NULL;
}

// Wrap a native file reader in the Java wrapping class
jobject FileReader_wrap(
    JNIEnv *env,
    ${file_reader_type} native_fr
) {
    // Get the file reader class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$FileReader");

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(L${ptr_sig};)V"
    );

    // Return the new file reader
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        PointerWrapper_wrap(env, (void *) native_fr)
    );
}

// Get a native file reader from a Java wrapping instance
${file_reader_type} FileReader_unwrap(
    JNIEnv *env,
    jobject file_reader
) {
    return (${file_reader_type}) get_reference(env, file_reader);
}

// ==========
// Unit provider functions
// ==========

// Create a new value for a unit provider
${unit_provider_type} UnitProvider_new_value() {
    return NULL;
}

// Wrap a native unit provider in the Java wrapping class
jobject UnitProvider_wrap(
    JNIEnv *env,
    ${unit_provider_type} native_up
) {
    // Get the unit provider class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$UnitProvider");

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(L${ptr_sig};)V"
    );

    // Return the new unit provider
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        PointerWrapper_wrap(env, (void *) native_up)
    );
}

// Get a native unit provider from a Java wrapping instance
${unit_provider_type} UnitProvider_unwrap(
    JNIEnv *env,
    jobject unit_provider
) {
    return (${unit_provider_type}) get_reference(env, unit_provider);
}

${api.jni_func_sig("dec_ref_unit_provider", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jlong provider_ref
) {
    ${nat("dec_ref_unit_provider")}((${unit_provider_type}) provider_ref);
}

// ==========
// Event handler functions
// ==========

// Create a new value for a event handler
${event_handler_type} EventHandler_new_value() {
    return NULL;
}

// Wrap a native event handler in the Java wrapping class
jobject EventHandler_wrap(
    JNIEnv *env,
    ${event_handler_type} native_eh
) {
    // Get the event handler class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$EventHandler");

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(L${ptr_sig};)V"
    );

    // Return the new event handler
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        PointerWrapper_wrap(env, (void *) native_eh)
    );
}

// Get the native event handler from a Java wrapping instance
${event_handler_type} EventHandler_unwrap(
    JNIEnv *env,
    jobject event_handler
) {
    return (${event_handler_type}) get_reference(env, event_handler);
}

// ==========
// Token functions
// ==========

// Create a new value for a token
${token_type} Token_new_value() {
    ${token_type} res = {
        NULL,
        NULL,
        0,
        0,
        0,
        Text_new_value(),
        SourceLocationRange_new_value()
    };
    return res;
}

// Wrap a native token in the Java wrapping class
jobject Token_wrap(
    JNIEnv *env,
    ${token_type} native_token,
    jobject analysis_unit
) {
    // Handle the no tokens
    if(native_token.token_data == NULL) {
        return NoToken_wrap(env, analysis_unit);
    }

    // Get the token class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$Token");

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(L${ptr_sig};L${sig_base}$AnalysisUnit;L${ptr_sig};"
        "IIL${sig_base}$TokenKind;L${sig_base}$Text;"
        "L${sig_base}$SourceLocationRange;)V"
    );

    return (*env)->NewObject(env, clazz, constructor,
        PointerWrapper_wrap(env, native_token.context),
        analysis_unit,
        PointerWrapper_wrap(env, native_token.token_data),
        (jint) native_token.token_index,
        (jint) native_token.trivia_index,
        TokenKind_wrap(env, native_token.kind),
        Text_wrap(env, native_token.text),
        SourceLocationRange_wrap(env, native_token.sloc_range)
    );
}

// Get a native token from the Java wrapping instance
${token_type} Token_unwrap(
    JNIEnv *env,
    jobject token
) {
    // Prepare the result structure
    ${token_type} res = Token_new_value();

    // Get if the token is instance of no token
    jclass nt_clazz = (*env)->FindClass(env, "${sig_base}$NoToken");
    if((*env)->IsInstanceOf(env, token, nt_clazz)) {

        // Get the field ids
        jfieldID context_field = (*env)->GetFieldID(
            env,
            nt_clazz,
            "contextRef",
            "L${ptr_sig};"
        );
        jfieldID tdh_field = (*env)->GetFieldID(
            env,
            nt_clazz,
            "tokenDataHandler",
            "L${ptr_sig};"
        );

        // Get the value from the object
        jobject context_value = (*env)->GetObjectField(
            env,
            token,
            context_field
        );
        jobject tdh_value = (*env)->GetObjectField(
            env,
            token,
            tdh_field
        );

        // Fill the result structure
        res.context = (${analysis_context_type}) PointerWrapper_unwrap(
            env,
            context_value
        );
        res.token_data = (${tdh_ptr_type}) PointerWrapper_unwrap(
            env,
            tdh_value
        );
        res.token_index = 0;
        res.trivia_index = 0;
        res.kind = -1;

        // Return the result
        return res;

    } else {

        // Get the token Java class
        jclass clazz = (*env)->GetObjectClass(env, token);

        // Get the field ids
        jfieldID context_field = (*env)->GetFieldID(
            env,
            clazz,
            "contextRef",
            "L${ptr_sig};"
        );
        jfieldID tdh_field = (*env)->GetFieldID(
            env,
            clazz,
            "tokenDataHandler",
            "L${ptr_sig};"
        );
        jfieldID token_index_field = (*env)->GetFieldID(
            env,
            clazz,
            "tokenIndex",
            "I"
        );
        jfieldID trivia_index_field = (*env)->GetFieldID(
            env,
            clazz,
            "triviaIndex",
            "I"
        );
        jfieldID token_kind_field = (*env)->GetFieldID(
            env,
            clazz,
            "kind",
            "L${sig_base}$TokenKind;"
        );
        jfieldID text_field = (*env)->GetFieldID(
            env,
            clazz,
            "text",
            "L${sig_base}$Text;"
        );
        jfieldID sloc_range_field = (*env)->GetFieldID(
            env,
            clazz,
            "sourceLocationRange",
            "L${sig_base}$SourceLocationRange;"
        );

        // Get the value from the object
        jobject context_value = (*env)->GetObjectField(
            env,
            token,
            context_field
        );
        jobject tdh_value = (*env)->GetObjectField(
            env,
            token,
            tdh_field
        );
        jint token_index_value = (*env)->GetIntField(
            env,
            token,
            token_index_field
        );
        jint trivia_index_value = (*env)->GetIntField(
            env,
            token,
            trivia_index_field
        );
        jobject token_kind_value = (*env)->GetObjectField(
            env,
            token,
            token_kind_field
        );
        jobject text_value = (*env)->GetObjectField(
            env,
            token,
            text_field
        );
        jobject sloc_range_value = (*env)->GetObjectField(
            env,
            token,
            sloc_range_field
        );

        // Fill the result structure
        res.context = (${analysis_context_type}) PointerWrapper_unwrap(
            env,
            context_value
        );
        res.token_data = (${tdh_ptr_type}) PointerWrapper_unwrap(
            env,
            tdh_value
        );
        res.token_index = (int) token_index_value;
        res.trivia_index = (int) trivia_index_value;
        res.kind = TokenKind_unwrap(env, token_kind_value);
        res.text = Text_unwrap(env, text_value);
        res.sloc_range = SourceLocationRange_unwrap(env, sloc_range_value);

        // Return the result
        return res;

    }
}

// Get the analysis unit from a token
jobject Token_get_unit(
    JNIEnv *env,
    jobject token
) {
    // Get the token class
    jclass clazz = (*env)->GetObjectClass(env, token);

    // Get the field id
    jfieldID unit_field = (*env)->GetFieldID(
        env,
        clazz,
        "unit",
        "L${sig_base}$AnalysisUnit;"
    );

    // Return the field
    return (*env)->GetObjectField(
        env,
        token,
        unit_field
    );
}

// Get the no token instance for the given analysis unit
jobject NoToken_wrap(
    JNIEnv *env,
    jobject analysis_unit
) {
    // Get the no token class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$NoToken");

    // Get the instance getting method
    jmethodID instance_getter = (*env)->GetStaticMethodID(
        env,
        clazz,
        "getInstance",
        "(L${sig_base}$AnalysisUnit;)L${sig_base}$NoToken;"
    );

    // Call the instance getter and return the result
    return (*env)->CallStaticObjectMethod(
        env,
        clazz,
        instance_getter,
        analysis_unit
    );
}

// Get the next token from the given token
${api.jni_func_sig("token_next", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject token
) {
    // Get the token native value
    ${token_type} native_token = Token_unwrap(env, token);

    // Prepare the result
    ${token_type} res = Token_new_value();

    // Call the native function
    ${nat("token_next")}(
        &native_token,
        &res
    );

    // Return the result token
    return Token_wrap(env, res, Token_get_unit(env, token));
}

// Get the previous token from the given token
${api.jni_func_sig("token_previous", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject token
) {
    // Get the token native value
    ${token_type} native_token = Token_unwrap(env, token);

    // Prepare the result
    ${token_type} res = Token_new_value();

    // Call the native function
    ${nat("token_previous")}(
        &native_token,
        &res
    );

    // Return the result token
    return Token_wrap(env, res, Token_get_unit(env, token));
}

// Get if the given token are equivalents
${api.jni_func_sig("token_is_equivalent", "jboolean")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject left,
    jobject right
) {
    // Get the token native values
    ${token_type} native_left = Token_unwrap(env, left);
    ${token_type} native_right = Token_unwrap(env, right);

    // Return the result of the native call
    return (jboolean) ${nat("token_is_equivalent")}(
        &native_left,
        &native_right
    );
}

// Get the text in the token interval
${api.jni_func_sig("token_range_text", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject start,
    jobject end
) {
    // Get the token native values
    ${token_type} native_start = Token_unwrap(env, start);
    ${token_type} native_end = Token_unwrap(env, end);

    // Prepare the result
    ${text_type} res = Text_new_value();

    // Call the native function
    ${nat("token_range_text")}(
        &native_start,
        &native_end,
        &res
    );

    // Return the result text
    return Text_wrap(env, res);
}

// ==========
// Analysis context functions
// ==========

// Create a new value for an analysis context
${analysis_context_type} AnalysisContext_new_value() {
    return NULL;
}

// Wrap a native analysis context in the Java wrapping class
jobject AnalysisContext_wrap(
    JNIEnv *env,
    ${analysis_context_type} native_context
) {
    // Get the analysis context class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$AnalysisContext");

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(L${ptr_sig};)V"
    );

    // Return the new analysis context
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        PointerWrapper_wrap(env, (void *) native_context)
    );
}

// Get a native analysis context from a Java wrapping instance
${analysis_context_type} AnalysisContext_unwrap(
    JNIEnv *env,
    jobject analysis_context
) {
    return (${analysis_context_type}) get_reference(env, analysis_context);
}

// Create a new analysis context
${api.jni_func_sig("create_analysis_context", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jstring charset,
    jobject file_reader,
    jobject unit_provider,
    jobject event_handler,
    jboolean with_trivia,
    jint tab_stop
) {
    // Translate the charset string
    const char* native_charset = NULL;
    if(charset != NULL) native_charset = to_c_string(env, charset);

    // Allocate the analysis context
    ${analysis_context_type} res = ${nat("allocate_analysis_context")}();

    // Call the native function
    ${nat("initialize_analysis_context")}(
        res,
        native_charset,
        FileReader_unwrap(env, file_reader),
        UnitProvider_unwrap(env, unit_provider),
        EventHandler_unwrap(env, event_handler),
        (int) with_trivia,
        (int) tab_stop
    );

    // Release the allocated string
    if(charset != NULL)
        (*env)->ReleaseStringUTFChars(env, charset, native_charset);

    // Return the new custom pointer to the analysis context
    return PointerWrapper_wrap(env, res);
}

// Increase the reference counter of an analysis context
${api.jni_func_sig("context_incref", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jlong analysis_context
) {
    // Call the native function
    ${nat("context_incref")}(
        (${analysis_context_type}) analysis_context
    );
}

// Decrease the reference counter of an analysis context
${api.jni_func_sig("context_decref", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jlong analysis_context
) {
    // Call the native funtion
    ${nat("context_decref")}(
        (${analysis_context_type}) analysis_context
    );
}

// ==========
// Analysis unit functions
// ==========

// Create a new value for an analysis unit
${analysis_unit_type} AnalysisUnit_new_value() {
    return NULL;
}

// Wrap a native analysis unit in the Java wrapping class
jobject AnalysisUnit_wrap(
    JNIEnv *env,
    ${analysis_unit_type} native_unit
) {
    // Get the analysis unit class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$AnalysisUnit");

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(L${ptr_sig};)V"
    );

    // Return the new analysis unit
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        PointerWrapper_wrap(env, (void *) native_unit)
    );
}

// Get a native analysis unit from a Java wrapping instance
${analysis_unit_type} AnalysisUnit_unwrap(
    JNIEnv *env,
    jobject analysis_unit
) {
    return (${analysis_unit_type}) get_reference(env, analysis_unit);
}

// Create an analysis unit from a file
${api.jni_func_sig("get_analysis_unit_from_file", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject analysis_context,
    jstring filename,
    jstring charset,
    jboolean reparse,
    jint grammar_rule
) {
    // Translate the Java strings
    const char *native_filename = to_c_string(env, filename);
    const char *native_charset = NULL;
    if(charset != NULL) native_charset = to_c_string(env, charset);

    // Call the native function
    ${analysis_unit_type} res = ${nat("get_analysis_unit_from_file")}(
        AnalysisContext_unwrap(env, analysis_context),
        native_filename,
        native_charset,
        (int) reparse,
        (int) grammar_rule
    );

    // Release the strings
    (*env)->ReleaseStringUTFChars(env, filename, native_filename);
    if(charset != NULL)
        (*env)->ReleaseStringUTFChars(env, charset, native_charset);

    // Return the new Analysis unit
    return AnalysisUnit_wrap(env, res);
}

// Create an analysis unit from a buffer
${api.jni_func_sig("get_analysis_unit_from_buffer", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject analysis_context,
    jstring filename,
    jstring charset,
    jstring buffer,
    jlong buffer_size,
    jint grammar_rule
) {
    // Translate the Java strings
    const char *native_filename = to_c_string(env, filename);
    const char *native_charset = NULL;
    if(charset != NULL) native_charset = to_c_string(env, charset);
    const char *native_buffer = to_c_string(env, buffer);

    // Call the native function
    ${analysis_unit_type} res = ${nat("get_analysis_unit_from_buffer")}(
        AnalysisContext_unwrap(env, analysis_context),
        native_filename,
        native_charset,
        native_buffer,
        (long) buffer_size,
        (int) grammar_rule
    );

    // Release the strings
    (*env)->ReleaseStringUTFChars(env, filename, native_filename);
    if(charset != NULL)
        (*env)->ReleaseStringUTFChars(env, charset, native_charset);
    (*env)->ReleaseStringUTFChars(env, buffer, native_buffer);

    // Return the new analysis unit
    return AnalysisUnit_wrap(env, res);
}

// Get the root entity from an analysis unit
${api.jni_func_sig("unit_root", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject analysis_unit
) {
    // Prepare the result
    ${entity_type} res = Entity_new_value();

    // Call the native function
    ${nat("unit_root")}(
        AnalysisUnit_unwrap(env, analysis_unit),
        &res
    );

    // Return the new entity
    return Entity_wrap(env, res);
}

// Get the name of the file related to the analysis unit
${api.jni_func_sig("unit_filename", "jstring")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject analysis_unit
) {
    // Call the native function
    char *res_native = ${nat("unit_filename")}(
        AnalysisUnit_unwrap(env, analysis_unit)
    );

    // Return the new java string
    jstring res = to_j_string(env, res_native);

    // Free the native allocated string
    free((void *) res_native);

    // Return the result
    return res;
}

// Get the token count of the analysis unit
${api.jni_func_sig("unit_token_count", "jint")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject analysis_unit
) {
    return (jint) ${nat("unit_token_count")}(
        AnalysisUnit_unwrap(env, analysis_unit)
    );
}

${api.jni_func_sig("unit_trivia_count", "jint")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject analysis_unit
) {
    return (jint) ${nat("unit_trivia_count")}(
        AnalysisUnit_unwrap(env, analysis_unit)
    );
}

// Get the first token of the analysis unit
${api.jni_func_sig("unit_first_token", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject analysis_unit
) {
    // Prepare the result
    ${token_type} res = Token_new_value();

    // Call the native function
    ${nat("unit_first_token")}(
        AnalysisUnit_unwrap(env, analysis_unit),
        &res
    );

    // Return the result token
    return Token_wrap(env, res, analysis_unit);
}

// Get the last token of the analysis unit
${api.jni_func_sig("unit_last_token", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject analysis_unit
) {
    // Prepare the result
    ${token_type} res = Token_new_value();

    // Call the native function
    ${nat("unit_last_token")}(
        AnalysisUnit_unwrap(env, analysis_unit),
        &res
    );

    // Return the result token
    return Token_wrap(env, res, analysis_unit);
}

// Get the analysis context for an analysis unit
${api.jni_func_sig("unit_context", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject analysis_unit
) {
    // Call the native result
    ${analysis_context_type} res = ${nat("unit_context")}(
        AnalysisUnit_unwrap(env, analysis_unit)
    );

    // Return the result context
    return AnalysisContext_wrap(env, res);
}

// Get the count of the diagnostic in the analysis unit
${api.jni_func_sig("unit_diagnostic_count", "jint")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject analysis_unit
) {
    // Return the casted native call
    return (jint) ${nat("unit_diagnostic_count")}(
        AnalysisUnit_unwrap(env, analysis_unit)
    );
}

// Get the nth diagnostic in the analysis unit
${api.jni_func_sig("unit_diagnostic", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject analysis_unit,
    jint n
) {
    // Prepare the result
    ${diagnostic_type} res = Diagnostic_new_value();

    // Call the native function
    ${nat("unit_diagnostic")}(
        AnalysisUnit_unwrap(env, analysis_unit),
        (unsigned) n,
        &res
    );

    // Return the diagnostic
    return Diagnostic_wrap(env, res);
}

// ==========
// Generated structure functions
// ==========

% for struct_type in ctx.struct_types:
    % if struct_type.is_entity_type:
        % if struct_type is root_entity:
${struct.jni_c_impl(struct_type)}
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
${struct.jni_c_impl(struct_type)}
        % endif
    % endif
% endfor

// ==========
// Generated array functions
// ==========

% for array_type in ctx.array_types:
    % if array_type.exposed and array_type.emit_c_type:
${array.jni_c_impl(array_type)}
    % endif
% endfor

// ==========
// Generated iterator functions
// ==========

% for iterator_type in ctx.iterator_types:
    % if iterator_type.exposed and iterator_type.emit_c_type:
${iterator.jni_c_impl(iterator_type)}
    % endif
% endfor

// ==========
// AST node functions
// ==========

// Get the kind of a node
${api.jni_func_sig("node_kind", "jint")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject entity
) {
    // Unwrap the node
    ${entity_type} native_entity = Entity_unwrap(env, entity);

    // Call the native function
    return (jint) ${nat("node_kind")}(
        &native_entity
    );
}

// Get the text of a node
${api.jni_func_sig("node_text", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject entity
) {
    // Unwrap the node
    ${entity_type} native_entity = Entity_unwrap(env, entity);

    // Prepare the result
    ${text_type} res = Text_new_value();

    // Call the native function
    ${nat("node_text")}(
        &native_entity,
        &res
    );

    // Return the node text
    return Text_wrap(env, res);
}

// Get the source location range of a node
${api.jni_func_sig("node_sloc_range", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject entity
) {
    // Unwrap the node
    ${entity_type} native_entity = Entity_unwrap(env, entity);

    // Prepare the result
    ${sloc_range_type} res = SourceLocationRange_new_value();

    // Call the native function
    ${nat("node_sloc_range")}(
        &native_entity,
        &res
    );

    // Return the source location range
    return SourceLocationRange_wrap(env, res);
}

// Get the children count of a node
${api.jni_func_sig("node_children_count", "jint")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject entity
) {
    // Unwrap the node
    ${entity_type} native_entity = Entity_unwrap(env, entity);

    // Call the native function
    return (jint) ${nat("node_children_count")}(
        &native_entity
    );
}

// Get the nth child of a node
${api.jni_func_sig("node_child", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject entity,
    jint n
) {
    // Unwrap the node
    ${entity_type} native_entity = Entity_unwrap(env, entity);

    // Preapre the result
    ${entity_type} res = Entity_new_value();

    // Call the native function
    ${nat("node_child")}(
        &native_entity,
        (unsigned) n,
        &res
    );

    // Return the child entity
    return Entity_wrap(env, res);
}

// Get if a node is a token node
${api.jni_func_sig("node_is_token_node", "jboolean")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject entity
) {
    // Unwrap the node
    ${entity_type} native_entity = Entity_unwrap(env, entity);

    // Call the native function
    return (jboolean) ${nat("node_is_token_node")}(
        &native_entity
    );
}

// Get the text image of a node
${api.jni_func_sig("node_unit", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject entity
) {
    // Unwrap the node
    ${entity_type} native_entity = Entity_unwrap(env, entity);

    // Call the native function
    ${analysis_unit_type} res_native = ${nat("node_unit")}(
        &native_entity
    );

    // Return the result
    return AnalysisUnit_wrap(env, res_native);
}

// Get the text image of a node
${api.jni_func_sig("node_image", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject entity
) {
    // Unwrap the node
    ${entity_type} native_entity = Entity_unwrap(env, entity);

    // Prepare the result
    ${text_type} res = Text_new_value();

    // Call the native function
    ${nat("node_image")}(
        &native_entity,
        &res
    );

    // Return the image text
    return Text_wrap(env, res);
}

// ==========
// AST node field accessors
// ==========

% for astnode in ctx.astnode_types:
    % for field in astnode.fields_with_accessors():
${ast_node.jni_field_accessor(field)}
    % endfor
% endfor
