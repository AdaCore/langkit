## vim: filetype=makocpp

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
// Global declarations
// ==========

char jvm_running = 0;

// ==========
// Type declaration
// ==========

// Structure to store the information for event handler
typedef struct {
    JNIEnv *env;
    jobject unit_requested_callback;
    jobject unit_parsed_callback;
} event_handler_data;

// ==========
// Function delcarations
// ==========

jclass main_class_ref = NULL;
jmethodID encodeUTF32_method_id = NULL;
jmethodID decodeUTF32_method_id = NULL;
jmethodID check_exception_method_id = NULL;

void * PointerWrapper_new_value();
jobject PointerWrapper_wrap(JNIEnv *, void *);
void * PointerWrapper_unwrap(JNIEnv *, jobject);

jclass PointerWrapper_class_ref = NULL;
jmethodID PointerWrapper_constructor_id = NULL;
jmethodID PointerWrapper_getter_id = NULL;

${exception_type} LangkitException_new_value();
jthrowable LangkitException_wrap(JNIEnv *, ${exception_type});

jclass LangkitException_class_ref = NULL;
jmethodID LangkitException_constructor_id = NULL;

${token_kind} TokenKind_new_value();
jobject TokenKind_wrap(JNIEnv *, ${token_kind});
${token_kind} TokenKind_unwrap(JNIEnv *, jobject);

jclass TokenKind_class_ref = NULL;
jmethodID TokenKind_from_c_method_id = NULL;
jmethodID TokenKind_to_c_method_id = NULL;

% for enum_type in ctx.enum_types:
${enum.jni_c_decl(enum_type)}
% endfor

uint32_t Char_new_value();
jobject Char_wrap(JNIEnv *, uint32_t);
uint32_t Char_unwrap(JNIEnv *, jobject);

jclass Char_class_ref = NULL;
jmethodID Char_constructor_id = NULL;
jfieldID Char_value_field_id = NULL;

${big_integer_type} BigIntegerWrapper_new_value();
jobject BigIntegerWrapper_wrap(JNIEnv *, ${big_integer_type});
${big_integer_type} BigIntegerWrapper_unwrap(JNIEnv *, jobject);
void BigIntegerWrapper_release(${big_integer_type});

jclass BigInteger_class_ref = NULL;
jmethodID BigInteger_constructor_id = NULL;
jmethodID BigInteger_to_string_method_id = NULL;

${symbol_type} Symbol_new_value();
jobject Symbol_wrap(JNIEnv *, ${symbol_type});
${symbol_type} Symbol_unwrap(JNIEnv *, jobject, ${analysis_context_type});
jthrowable new_symbol_exception(JNIEnv *, jstring);

jclass Symbol_class_ref = NULL;
jmethodID Symbol_constructor_id = NULL;
jfieldID Symbol_text_field_id = NULL;
jclass SymbolException_class_ref = NULL;
jmethodID SymbolException_constructor_id = NULL;

${string_type} StringWrapper_new_value();
jobject StringWrapper_wrap(JNIEnv *, ${string_type});
${string_type} StringWrapper_unwrap(JNIEnv *, jobject);
void StringWrapper_release(${string_type});

${text_type} Text_new_value();
jobject Text_wrap(JNIEnv *, ${text_type});
${text_type} Text_unwrap(JNIEnv *, jobject);
jstring get_text_content(JNIEnv *, jobject);
jobject text_from_content(JNIEnv *, jstring);

jclass Text_class_ref = NULL;
jmethodID Text_constructor_id = NULL;
jmethodID Text_extended_constructor_id = NULL;
jmethodID Text_create_method_id = NULL;
jmethodID Text_get_content_method_id = NULL;
jfieldID Text_chars_field_id = NULL;
jfieldID Text_length_field_id = NULL;
jfieldID Text_is_allocated_field_id = NULL;
jfieldID Text_is_owner_field_id = NULL;

${sloc_type} SourceLocation_new_value();
jobject SourceLocation_wrap(JNIEnv *, ${sloc_type});
${sloc_type} SourceLocation_unwrap(JNIEnv *, jobject);

jclass SourceLocation_class_ref = NULL;
jmethodID SourceLocation_constructor_id = NULL;
jfieldID SourceLocation_line_field_id = NULL;
jfieldID SourceLocation_column_field_id = NULL;

${sloc_range_type} SourceLocationRange_new_value();
jobject SourceLocationRange_wrap(JNIEnv *, ${sloc_range_type});
${sloc_range_type} SourceLocationRange_unwrap(JNIEnv *, jobject);

jclass SourceLocationRange_class_ref = NULL;
jmethodID SourceLocationRange_constructor_id = NULL;
jfieldID SourceLocationRange_start_field_id = NULL;
jfieldID SourceLocationRange_end_field_id = NULL;

${diagnostic_type} Diagnostic_new_value();
jobject Diagnostic_wrap(JNIEnv *, ${diagnostic_type});
${diagnostic_type} Diagnostic_unwrap(JNIEnv *, jobject);

jclass Diagnostic_class_ref = NULL;
jmethodID Diagnostic_constructor_id = NULL;
jfieldID Diagnostic_sloc_range_field_id = NULL;
jfieldID Diagnostic_text_field_id = NULL;

${file_reader_type} FileReader_new_value();
jobject FileReader_wrap(JNIEnv *, ${file_reader_type});
${file_reader_type} FileReader_unwrap(JNIEnv *, jobject);

jclass FileReader_class_ref = NULL;
jmethodID FileReader_constructor_id = NULL;
jfieldID FileReader_reference_field_id = NULL;

${unit_provider_type} UnitProvider_new_value();
jobject UnitProvider_wrap(JNIEnv *, ${unit_provider_type});
${unit_provider_type} UnitProvider_unwrap(JNIEnv *, jobject);

jclass UnitProvider_class_ref = NULL;
jmethodID UnitProvider_constructor_id = NULL;
jfieldID UnitProvider_reference_field_id = NULL;

${event_handler_type} EventHandler_new_value();
jobject EventHandler_wrap(JNIEnv *, ${event_handler_type});
${event_handler_type} EventHandler_unwrap(JNIEnv *, jobject);

jclass EventHandler_class_ref = NULL;
jmethodID EventHandler_from_reference_id = NULL;
jfieldID EventHandler_reference_field_id = NULL;
jclass UnitRequestedCallback_class_id = NULL;
jmethodID UnitRequestedCallback_invoke_id = NULL;
jclass UnitParsedCallback_class_id = NULL;
jmethodID UnitParsedCallback_invoke_id = NULL;

${token_type} Token_new_value();
jobject Token_wrap(JNIEnv *, ${token_type}, jobject);
${token_type} Token_unwrap(JNIEnv *, jobject);
jobject Token_get_unit(JNIEnv *, jobject);
jobject NoToken_wrap(JNIEnv *, jobject);

jclass Token_class_ref = NULL;
jclass NoToken_class_ref = NULL;
jmethodID Token_constructor_id = NULL;
jmethodID Token_none_getter_method_id = NULL;
jfieldID Token_context_field_id = NULL;
jfieldID Token_tdh_field_id = NULL;
jfieldID Token_token_index_field_id = NULL;
jfieldID Token_trivia_index_field_id = NULL;
jfieldID Token_token_kind_field_id = NULL;
jfieldID Token_text_field_id = NULL;
jfieldID Token_sloc_range_field_id = NULL;
jfieldID Token_unit_field_id = NULL;

${analysis_context_type} AnalysisContext_new_value();
jobject AnalysisContext_wrap(JNIEnv *, ${analysis_context_type});
${analysis_context_type} AnalysisContext_unwrap(JNIEnv *, jobject);

jclass AnalysisContext_class_ref = NULL;
jmethodID AnalysisContext_from_reference_id = NULL;
jfieldID AnalysisContext_reference_field_id = NULL;

${analysis_unit_type} AnalysisUnit_new_value();
jobject AnalysisUnit_wrap(JNIEnv *, ${analysis_unit_type});
${analysis_unit_type} AnalysisUnit_unwrap(JNIEnv *, jobject);

jclass AnalysisUnit_class_ref = NULL;
jmethodID AnalysisUnit_constructor_id = NULL;
jfieldID AnalysisUnit_reference_field_id = NULL;

% if ctx.generate_unparser:
${rewriting_apply_result_type} RewritingApplyResult_new_value();
jobject RewritingApplyResult_wrap(JNIEnv *, ${rewriting_apply_result_type});
${rewriting_apply_result_type} RewritingApplyResult_unwrap(JNIEnv *, jobject);

jclass RewritingApplyResult_class_ref = NULL;
jmethodID RewritingApplyResult_contructor_id = NULL;
jmethodID RewritingApplyResult_success_id = NULL;
jfieldID RewritingApplyResult_success_field_id = NULL;
jfieldID RewritingApplyResult_unit_field_id = NULL;
jfieldID RewritingApplyResult_diagnostics_count_field_id = NULL;
jfieldID RewritingApplyResult_diagnostics_reference_field_id = NULL;

${rewriting_handle_type} RewritingContext_new_value();
jobject RewritingContext_wrap(JNIEnv *, ${rewriting_handle_type});
${rewriting_handle_type} RewritingContext_unwrap(JNIEnv *, jobject);

jclass RewritingContext_class_ref = NULL;
jmethodID RewritingContext_from_reference_id = NULL;
jfieldID RewritingContext_reference_field_id = NULL;

${unit_rewriting_handle_type} RewritingUnit_new_value();
jobject RewritingUnit_wrap(JNIEnv *, ${unit_rewriting_handle_type});
${unit_rewriting_handle_type} RewritingUnit_unwrap(JNIEnv *, jobject);

jclass RewritingUnit_class_ref = NULL;
jmethodID RewritingUnit_constructor_id = NULL;
jfieldID RewritingUnit_reference_field_id = NULL;

${node_rewriting_handle_type} RewritingNode_new_value();
jobject RewritingNode_wrap(JNIEnv *, ${node_rewriting_handle_type});
${node_rewriting_handle_type} RewritingNode_unwrap(JNIEnv *, jobject);

jclass RewritingNode_class_ref = NULL;
jmethodID RewritingNode_constructor_id = NULL;
jfieldID RewritingNode_reference_field_id = NULL;
% endif

% for struct_type in ctx.struct_types:
    % if api.should_emit_struct(struct_type):
${struct.jni_c_decl(struct_type)}
    % endif
% endfor

% for array_type in ctx.array_types:
    % if array_type.exposed:
${array.jni_c_decl(array_type)}
    % endif
% endfor

% for iterator_type in ctx.iterator_types:
    % if iterator_type.exposed and iterator_type.emit_c_type:
${iterator.jni_c_decl(iterator_type)}
    % endif
% endfor

jclass ${root_node_type}_class_ref = NULL;
jmethodID ${root_node_type}_from_entity_id = NULL;
jfieldID ${root_node_type}_entity_field_id = NULL;

// ==========
// Lifecycle functions
// ==========

${api.jni_func_sig("initialize", "void")}(
    JNIEnv *env,
    jclass jni_lib
) {
    jvm_running = 1;

    main_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}")
    );

    encodeUTF32_method_id = (*env)->GetStaticMethodID(
        env,
        main_class_ref,
        "encodeUTF32",
        "(Ljava/lang/String;)[B"
    );

    decodeUTF32_method_id = (*env)->GetStaticMethodID(
        env,
        main_class_ref,
        "decodeUTF32",
        "([B)Ljava/lang/String;"
    );

    check_exception_method_id = (*env)->GetStaticMethodID(
        env,
        main_class_ref,
        "checkException",
        "()V"
    );

    PointerWrapper_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${ptr_sig}")
    );

    PointerWrapper_constructor_id = (*env)->GetMethodID(
        env,
        PointerWrapper_class_ref,
        "<init>",
        "(J)V"
    );

    PointerWrapper_getter_id = (*env)->GetMethodID(
        env,
        PointerWrapper_class_ref,
        "jni",
        "()J"
    );

    LangkitException_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$LangkitException")
    );

    LangkitException_constructor_id = (*env)->GetMethodID(
        env,
        LangkitException_class_ref,
        "<init>",
        "(ILjava/lang/String;)V"
    );

    Symbol_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$Symbol")
    );

    Symbol_constructor_id = (*env)->GetMethodID(
        env,
        Symbol_class_ref,
        "<init>",
        "(Ljava/lang/String;)V"
    );

    Symbol_text_field_id = (*env)->GetFieldID(
        env,
        Symbol_class_ref,
        "text",
        "Ljava/lang/String;"
    );

    SymbolException_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$SymbolException")
    );

    SymbolException_constructor_id = (*env)->GetMethodID(
        env,
        SymbolException_class_ref,
        "<init>",
        "(Ljava/lang/String;)V"
    );

    Text_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$Text")
    );

    Text_constructor_id = (*env)->GetMethodID(
        env,
        Text_class_ref,
        "<init>",
        "(L${ptr_sig};JZ[B)V"
    );

    Text_extended_constructor_id = (*env)->GetMethodID(
        env,
        Text_class_ref,
        "<init>",
        "(L${ptr_sig};JZZ[B)V"
    );

    Text_create_method_id = (*env)->GetStaticMethodID(
        env,
        Text_class_ref,
        "create",
        "(Ljava/lang/String;)L${sig_base}$Text;"
    );

    Text_get_content_method_id = (*env)->GetMethodID(
        env,
        Text_class_ref,
        "getContent",
        "()Ljava/lang/String;"
    );

    Text_chars_field_id = (*env)->GetFieldID(
        env,
        Text_class_ref,
        "charPointer",
        "L${sig_base}$PointerWrapper;"
    );

    Text_length_field_id = (*env)->GetFieldID(
        env,
        Text_class_ref,
        "length",
        "J"
    );

    Text_is_allocated_field_id = (*env)->GetFieldID(
        env,
        Text_class_ref,
        "isAllocated",
        "Z"
    );

    Text_is_owner_field_id = (*env)->GetFieldID(
        env,
        Text_class_ref,
        "isOwner",
        "Z"
    );

    SourceLocation_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$SourceLocation")
    );

    SourceLocation_constructor_id = (*env)->GetMethodID(
        env,
        SourceLocation_class_ref,
        "<init>",
        "(IS)V"
    );

    SourceLocation_line_field_id = (*env)->GetFieldID(
        env,
        SourceLocation_class_ref,
        "line",
        "I"
    );

    SourceLocation_column_field_id = (*env)->GetFieldID(
        env,
        SourceLocation_class_ref,
        "column",
        "S"
    );

    TokenKind_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$TokenKind")
    );

    TokenKind_from_c_method_id = (*env)->GetStaticMethodID(
        env,
        TokenKind_class_ref,
        "fromC",
        "(I)L${sig_base}$TokenKind;"
    );

    TokenKind_to_c_method_id = (*env)->GetMethodID(
        env,
        TokenKind_class_ref,
        "toC",
        "()I"
    );

    Char_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$Char")
    );

    Char_constructor_id = (*env)->GetMethodID(
        env,
        Char_class_ref,
        "<init>",
        "(I)V"
    );

    Char_value_field_id = (*env)->GetFieldID(
        env,
        Char_class_ref,
        "value",
        "I"
    );

    BigInteger_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "java/math/BigInteger")
    );

    BigInteger_constructor_id = (*env)->GetMethodID(
        env,
        BigInteger_class_ref,
        "<init>",
        "(Ljava/lang/String;)V"
    );

    BigInteger_to_string_method_id = (*env)->GetMethodID(
        env,
        BigInteger_class_ref,
        "toString",
        "()Ljava/lang/String;"
    );

    SourceLocationRange_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$SourceLocationRange")
    );

    SourceLocationRange_constructor_id = (*env)->GetMethodID(
        env,
        SourceLocationRange_class_ref,
        "<init>",
        "(L${sig_base}$SourceLocation;L${sig_base}$SourceLocation;)V"
    );

    SourceLocationRange_start_field_id = (*env)->GetFieldID(
        env,
        SourceLocationRange_class_ref,
        "start",
        "L${sig_base}$SourceLocation;"
    );

    SourceLocationRange_end_field_id = (*env)->GetFieldID(
        env,
        SourceLocationRange_class_ref,
        "end",
        "L${sig_base}$SourceLocation;"
    );

    Diagnostic_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$Diagnostic")
    );

    Diagnostic_constructor_id = (*env)->GetMethodID(
        env,
        Diagnostic_class_ref,
        "<init>",
        "(L${sig_base}$SourceLocationRange;L${sig_base}$Text;)V"
    );

    Diagnostic_sloc_range_field_id = (*env)->GetFieldID(
        env,
        Diagnostic_class_ref,
        "sourceLocationRange",
        "L${sig_base}$SourceLocationRange;"
    );

    Diagnostic_text_field_id = (*env)->GetFieldID(
        env,
        Diagnostic_class_ref,
        "message",
        "L${sig_base}$Text;"
    );

    FileReader_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$FileReader")
    );

    FileReader_constructor_id = (*env)->GetMethodID(
        env,
        FileReader_class_ref,
        "<init>",
        "(L${ptr_sig};)V"
    );

    FileReader_reference_field_id = (*env)->GetFieldID(
        env,
        FileReader_class_ref,
        "reference",
        "L${ptr_sig};"
    );

    UnitProvider_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$UnitProvider")
    );

    UnitProvider_constructor_id = (*env)->GetMethodID(
        env,
        UnitProvider_class_ref,
        "<init>",
        "(L${ptr_sig};)V"
    );

    UnitProvider_reference_field_id = (*env)->GetFieldID(
        env,
        UnitProvider_class_ref,
        "reference",
        "L${ptr_sig};"
    );

    EventHandler_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$EventHandler")
    );

    EventHandler_from_reference_id = (*env)->GetStaticMethodID(
        env,
        EventHandler_class_ref,
        "fromReference",
        "(L${ptr_sig};)L${sig_base}$EventHandler;"
    );

    EventHandler_reference_field_id = (*env)->GetFieldID(
        env,
        EventHandler_class_ref,
        "reference",
        "L${ptr_sig};"
    );

    UnitRequestedCallback_class_id = (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(
            env,
            "${sig_base}$EventHandler$UnitRequestedCallback"
        )
    );

    UnitRequestedCallback_invoke_id = (*env)->GetMethodID(
        env,
        UnitRequestedCallback_class_id,
        "invoke",
        "(L${sig_base}$AnalysisContext;Ljava/lang/String;"
        "L${sig_base}$AnalysisUnit;ZZ)V"
    );

    UnitParsedCallback_class_id = (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(
            env,
            "${sig_base}$EventHandler$UnitParsedCallback"
        )
    );

    UnitParsedCallback_invoke_id = (*env)->GetMethodID(
        env,
        UnitParsedCallback_class_id,
        "invoke",
        "(L${sig_base}$AnalysisContext;L${sig_base}$AnalysisUnit;Z)V"
    );

    Token_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$Token")
    );

    NoToken_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$Token$NoToken")
    );

    Token_constructor_id = (*env)->GetMethodID(
        env,
        Token_class_ref,
        "<init>",
        "(L${ptr_sig};L${sig_base}$AnalysisUnit;L${ptr_sig};"
        "IIL${sig_base}$TokenKind;Ljava/lang/String;"
        "L${sig_base}$SourceLocationRange;)V"
    );

    Token_none_getter_method_id = (*env)->GetStaticMethodID(
        env,
        Token_class_ref,
        "NONE",
        "(L${sig_base}$AnalysisUnit;)L${sig_base}$Token;"
    );

    Token_context_field_id = (*env)->GetFieldID(
        env,
        Token_class_ref,
        "contextRef",
        "L${ptr_sig};"
    );

    Token_tdh_field_id = (*env)->GetFieldID(
        env,
        Token_class_ref,
        "tokenDataHandler",
        "L${ptr_sig};"
    );

    Token_token_index_field_id = (*env)->GetFieldID(
        env,
        Token_class_ref,
        "tokenIndex",
        "I"
    );

    Token_trivia_index_field_id = (*env)->GetFieldID(
        env,
        Token_class_ref,
        "triviaIndex",
        "I"
    );

    Token_token_kind_field_id = (*env)->GetFieldID(
        env,
        Token_class_ref,
        "kind",
        "L${sig_base}$TokenKind;"
    );

    Token_text_field_id = (*env)->GetFieldID(
        env,
        Token_class_ref,
        "text",
        "Ljava/lang/String;"
    );

    Token_sloc_range_field_id = (*env)->GetFieldID(
        env,
        Token_class_ref,
        "sourceLocationRange",
        "L${sig_base}$SourceLocationRange;"
    );

    Token_unit_field_id = (*env)->GetFieldID(
        env,
        Token_class_ref,
        "unit",
        "L${sig_base}$AnalysisUnit;"
    );

    AnalysisContext_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$AnalysisContext")
    );

    AnalysisContext_from_reference_id = (*env)->GetStaticMethodID(
        env,
        AnalysisContext_class_ref,
        "fromReference",
        "(L${ptr_sig};)L${sig_base}$AnalysisContext;"
    );

    AnalysisContext_reference_field_id = (*env)->GetFieldID(
        env,
        AnalysisContext_class_ref,
        "reference",
        "L${ptr_sig};"
    );

    AnalysisUnit_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$AnalysisUnit")
    );

    AnalysisUnit_constructor_id = (*env)->GetMethodID(
        env,
        AnalysisUnit_class_ref,
        "<init>",
        "(L${ptr_sig};)V"
    );

    AnalysisUnit_reference_field_id = (*env)->GetFieldID(
        env,
        AnalysisUnit_class_ref,
        "reference",
        "L${ptr_sig};"
    );

    % if ctx.generate_unparser:
    RewritingApplyResult_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$RewritingApplyResult")
    );

    RewritingApplyResult_contructor_id = (*env)->GetMethodID(
        env,
        RewritingApplyResult_class_ref,
        "<init>",
        "(ZL${sig_base}$AnalysisUnit;IL${ptr_sig};)V"
    );

    RewritingApplyResult_success_id = (*env)->GetStaticMethodID(
        env,
        RewritingApplyResult_class_ref,
        "success",
        "()L${sig_base}$RewritingApplyResult;"
    );

    RewritingApplyResult_success_field_id = (*env)->GetFieldID(
        env,
        RewritingApplyResult_class_ref,
        "success",
        "Z"
    );

    RewritingApplyResult_unit_field_id = (*env)->GetFieldID(
        env,
        RewritingApplyResult_class_ref,
        "unit",
        "L${sig_base}$AnalysisUnit;"
    );

    RewritingApplyResult_diagnostics_count_field_id = (*env)->GetFieldID(
        env,
        RewritingApplyResult_class_ref,
        "diagnosticsCount",
        "I"
    );

    RewritingApplyResult_diagnostics_reference_field_id = (*env)->GetFieldID(
        env,
        RewritingApplyResult_class_ref,
        "diagnosticsReference",
        "L${ptr_sig};"
    );

    RewritingContext_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$RewritingContext")
    );

    RewritingContext_from_reference_id = (*env)->GetStaticMethodID(
        env,
        RewritingContext_class_ref,
        "fromReference",
        "(L${ptr_sig};)L${sig_base}$RewritingContext;"
    );

    RewritingContext_reference_field_id = (*env)->GetFieldID(
        env,
        RewritingContext_class_ref,
        "reference",
        "L${ptr_sig};"
    );

    RewritingUnit_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$RewritingUnit")
    );

    RewritingUnit_constructor_id = (*env)->GetMethodID(
        env,
        RewritingUnit_class_ref,
        "<init>",
        "(L${ptr_sig};)V"
    );

    RewritingUnit_reference_field_id = (*env)->GetFieldID(
        env,
        RewritingUnit_class_ref,
        "reference",
        "L${ptr_sig};"
    );

    RewritingNode_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$RewritingNode")
    );

    RewritingNode_constructor_id = (*env)->GetMethodID(
        env,
        RewritingNode_class_ref,
        "<init>",
        "(L${ptr_sig};)V"
    );

    RewritingNode_reference_field_id = (*env)->GetFieldID(
        env,
        RewritingNode_class_ref,
        "reference",
        "L${ptr_sig};"
    );
    % endif

% for struct_type in ctx.struct_types:
    % if api.should_emit_struct(struct_type):
${struct.jni_init_global_refs(struct_type)}
    % endif
% endfor

% for enum_type in ctx.enum_types:
${enum.jni_init_global_refs(enum_type)}
% endfor

% for array_type in ctx.array_types:
    % if array_type.exposed:
${array.jni_init_global_refs(array_type)}
    % endif
% endfor

% for iterator_type in ctx.iterator_types:
    % if iterator_type.exposed and iterator_type.emit_c_type:
${iterator.jni_init_global_refs(iterator_type)}
    % endif
% endfor

    ${root_node_type}_class_ref = (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$${root_node_type}")
    );

    ${root_node_type}_from_entity_id = (*env)->GetStaticMethodID(
        env,
        ${root_node_type}_class_ref,
        "fromEntity",
        "(L${sig_base}$Entity;)L${sig_base}$${root_node_type};"
    );

    ${root_node_type}_entity_field_id = (*env)->GetFieldID(
        env,
        ${root_node_type}_class_ref,
        "entity",
        "L${sig_base}$Entity;"
    );
}

// Function to finalize the JNI library
${api.jni_func_sig("finalize", "void")}(
    JNIEnv *env,
    jclass jni_lib
) {
    jvm_running = 0;
}

// ==========
// Util functions
// ==========

// Get the native reference in a Java object
void * get_reference(
    JNIEnv *env,
    jobject object
) {
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

// Translate a Java string to a C char pointer with the UTF-8 encoding
const char * to_c_string(
    JNIEnv *env,
    jstring j_string
) {
    return (*env)->GetStringUTFChars(env, j_string, NULL);
}

// Release the given C string associated with the given Java string
void release_c_string(
    JNIEnv *env,
    jstring j_string,
    const char *c_string
) {
    (*env)->ReleaseStringUTFChars(env, j_string, c_string);
}

// Create a Java string from a C char pointer
jstring to_j_string(
    JNIEnv *env,
    const char *c_string
) {
    return (*env)->NewStringUTF(env, c_string);
}

// Decode an UTF 32 buffer in a Java string
jstring decode_utf_32(
    JNIEnv *env,
    size_t length,
    uint32_t *to_decode
) {
    // Create a byte array from the buffer to decode
    const jbyte *byte_buffer = (jbyte *) to_decode;
    jsize byte_length = (jsize) (length * 4);
    jbyteArray byte_array = (*env)->NewByteArray(
        env,
        byte_length
    );
    (*env)->SetByteArrayRegion(
        env,
        byte_array,
        0,
        byte_length,
        byte_buffer
    );

    // Call the Java method and return the result
    return (jstring) (*env)->CallStaticObjectMethod(
        env,
        main_class_ref,
        decodeUTF32_method_id,
        byte_array
    );
}

// Encode a Java string in a native buffer
void encode_utf_32(
    JNIEnv *env,
    jstring string,
    size_t *length_ref,
    uint32_t **buffer_ref
) {
    // Call the Java method to get the byte array
    jbyteArray byte_array = (jbyteArray) (*env)->CallStaticObjectMethod(
        env,
        main_class_ref,
        encodeUTF32_method_id,
        string
    );
    size_t byte_length = (size_t) (*env)->GetArrayLength(
        env,
        byte_array
    );

    // Allocate the memory for the native buffer
    *buffer_ref = (uint32_t *) malloc(byte_length);

    // Write the native buffer and the length
    *length_ref = byte_length / 4;
    (*env)->GetByteArrayRegion(
        env,
        byte_array,
        0,
        byte_length,
        (jbyte *) *buffer_ref
    );
}

void check_exception(JNIEnv *env) {
    // Call the Java checking exception function
    (*env)->CallStaticVoidMethod(
        env,
        main_class_ref,
        check_exception_method_id
    );
}

// Util function to wrap a native entity into a Java node class
jobject node_from_entity(JNIEnv *env, jobject entity) {
    return (*env)->CallStaticObjectMethod(
        env,
        ${root_node_type}_class_ref,
        ${root_node_type}_from_entity_id,
        entity
    );
}

// Util function to get the entity Java object from a wrapped node
jobject get_node_entity(JNIEnv *env, jobject node) {
    return (*env)->GetObjectField(
        env,
        node,
        ${root_node_type}_entity_field_id
    );
}

% if ctx.generate_unparser:
// Function to get a rewriting node array from a Java one
${node_rewriting_handle_type} *rewriting_nodes_to_native(
    JNIEnv *env,
    jobjectArray rewriting_nodes
) {
    int size = (*env)->GetArrayLength(env, rewriting_nodes);
    ${node_rewriting_handle_type} *res =
        (${node_rewriting_handle_type} *) malloc(
            size * sizeof(${node_rewriting_handle_type})
        );
    for(int i = 0; i < size; i++) {
        res[i] = RewritingNode_unwrap(
            env,
            (*env)->GetObjectArrayElement(
                env,
                rewriting_nodes,
                (jsize) i
            )
        );
    }
    return res;
}
% endif

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
    // Create the new custom pointer
    return (*env)->NewObject(
        env,
        PointerWrapper_class_ref,
        PointerWrapper_constructor_id,
        (jlong) pointer
    );
}

// Get the C pointer from the Java custom pointer
void * PointerWrapper_unwrap(
    JNIEnv *env,
    jobject custom_pointer
) {
    // Return the C pointer
    return (void *) (*env)->CallLongMethod(
        env,
        custom_pointer,
        PointerWrapper_getter_id
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
    // Return the new exception instance
    return (jthrowable) (*env)->NewObject(
        env,
        LangkitException_class_ref,
        LangkitException_constructor_id,
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
    // Call the static method
    return (*env)->CallStaticObjectMethod(
        env,
        TokenKind_class_ref,
        TokenKind_from_c_method_id,
        (jint) enum_value_native
    );
}

// Unwrap a Java token kind as a native one
${token_kind} TokenKind_unwrap(
    JNIEnv *env,
    jobject enum_value
) {
    // Call the Java method
    return (${token_kind}) (*env)->CallIntMethod(
        env,
        enum_value,
        TokenKind_to_c_method_id
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
    uint32_t char_native
) {
    // Return the new object
    return (*env)->NewObject(
        env,
        Char_class_ref,
        Char_constructor_id,
        char_native
    );
}

// Get a native character from a Java wrapping instance
uint32_t Char_unwrap(
    JNIEnv *env,
    jobject character
) {
    // Return the result field
    return (uint32_t) (*env)->GetIntField(
        env,
        character,
        Char_value_field_id
    );
}

// ==========
// Big integer functions
// ==========

// Create a new value for a big integer
${big_integer_type} BigIntegerWrapper_new_value() {
    return NULL;
}

// Wrap a native big integer in the Java class
jobject BigIntegerWrapper_wrap(
    JNIEnv *env,
    ${big_integer_type} big_int_native
) {
    // Get the representation of the big integer
    ${text_type} representation_native = Text_new_value();
    ${nat("big_integer_text")}(
        big_int_native,
        &representation_native
    );
    jobject representation_text = Text_wrap(env, representation_native);
    jstring representation = get_text_content(env, representation_text);

    // Destroy the representation text
    ${nat("destroy_text")}(
        &representation_native
    );

    // Return the new big integer
    return (*env)->NewObject(
        env,
        BigInteger_class_ref,
        BigInteger_constructor_id,
        representation
    );
}

// Get the native big integer from the Java wrapping instance
${big_integer_type} BigIntegerWrapper_unwrap(
    JNIEnv *env,
    jobject big_integer
) {
    // Get the representation of the big integer
    jstring representation = (*env)->CallObjectMethod(
        env,
        big_integer,
        BigInteger_to_string_method_id
    );

    // Create a text from the representations
    jobject representation_text = (*env)->CallStaticObjectMethod(
        env,
        Text_class_ref,
        Text_create_method_id,
        representation
    );
    ${text_type} representation_native = Text_unwrap(env, representation_text);

    // Create a bit integer from the text
    ${big_integer_type} res = ${nat("create_big_integer")}(
        &representation_native
    );

    // Destroy the text
    free(representation_native.chars);

    // Return the result
    return res;
}

// Release the given native big integer
void BigIntegerWrapper_release(
    ${big_integer_type} big_int_native
) {
    ${nat("big_integer_decref")}(big_int_native);
}

// ==========
// Symbol functions
// ==========

// Create a new value for a symbol
${symbol_type} Symbol_new_value() {
    ${symbol_type} res = {
        0,
        NULL
    };
    return res;
}

// Wrap a native symbol in the Java class
jobject Symbol_wrap(
    JNIEnv *env,
    ${symbol_type} symbol_native
) {
    // Get the text of the symbol
    ${text_type} text_native = Text_new_value();
    ${nat("symbol_text")}(
        &symbol_native,
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
        Symbol_class_ref,
        Symbol_constructor_id,
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

    // Get the fields value
    jstring str = (jstring) (*env)->GetObjectField(
        env,
        symbol,
        Symbol_text_field_id
    );

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
    // Return the exception
    return (*env)->NewObject(
        env,
        SymbolException_class_ref,
        SymbolException_constructor_id,
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

// Wrap a native langkit string in the Java class
jstring StringWrapper_wrap(
    JNIEnv *env,
    ${string_type} string_native
) {
    return decode_utf_32(
        env,
        (size_t) string_native->length,
        string_native->content
    );
}

// Get the native langkit string from a Java wrapping instance
${string_type} StringWrapper_unwrap(
    JNIEnv *env,
    jstring string
) {
    // Encode the Java string
    size_t length;
    uint32_t *buffer;
    encode_utf_32(
        env,
        string,
        &length,
        &buffer
    );

    // Create a new native string
    ${string_type} res = ${nat("create_string")}(
        buffer,
        (int) length
    );

    // Free the buffer
    free(buffer);

    // Return the result
    return res;
}

// Release the given native string
void StringWrapper_release(
    ${string_type} string_native
) {
    ${nat("string_dec_ref")}(string_native);
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
    // Get the int array from the structure and translate it into Java array
    jbyteArray content = (*env)->NewByteArray(
        env,
        (jsize) text_native.length * 4
    );
    (*env)->SetByteArrayRegion(
        env,
        content,
        0,
        (jsize) text_native.length * 4,
        (jbyte *) text_native.chars
    );

    // Return the new text
    return (*env)->NewObject(
        env,
        Text_class_ref,
        Text_constructor_id,
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

    // Get the values
    jobject chars = (*env)->GetObjectField(
        env,
        text,
        Text_chars_field_id
    );
    jlong length = (*env)->GetLongField(
        env,
        text,
        Text_length_field_id
    );
    jboolean is_allocated = (*env)->GetBooleanField(
        env,
        text,
        Text_is_allocated_field_id
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
    return (jstring) (*env)->CallObjectMethod(
        env,
        text,
        Text_get_content_method_id
    );
}

// Create a text object from its string content
jobject text_from_content(
    JNIEnv *env,
    jstring content
) {
    // Call the creating method
    return (*env)->CallStaticObjectMethod(
        env,
        Text_class_ref,
        Text_create_method_id,
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

    // Return the new text
    return (*env)->NewObject(
        env,
        Text_class_ref,
        Text_extended_constructor_id,
        PointerWrapper_wrap(env, content_native),
        (jlong) length,
        (jboolean) 0,
        (jboolean) 1,
        content_utf32
    );
}

// Destroy a text object
${api.jni_func_sig("destroy_text", "void")} (
    JNIEnv *env,
    jclass jni_lib,
    jobject text
) {
    // Get if the text is the owner of its buffer
    jboolean is_owner = (*env)->GetBooleanField(
        env,
        text,
        Text_is_owner_field_id
    );

    // Unwrap the text
    ${text_type} text_native = Text_unwrap(env, text);

    // If the object is the buffer owner just free the chars
    if(is_owner) {
        free((void *) text_native.chars);
    } else {
        ${nat("destroy_text")}(&text_native);
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
    // Return the new source location
    return (*env)->NewObject(
        env,
        SourceLocation_class_ref,
        SourceLocation_constructor_id,
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

    // Get the fields value
    jint line = (*env)->GetIntField(
        env,
        sloc,
        SourceLocation_line_field_id
    );
    jshort column = (*env)->GetShortField(
        env,
        sloc,
        SourceLocation_column_field_id
    );

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
    ${sloc_range_type} slocr_native
) {
    // Return the new source location range
    return (*env)->NewObject(
        env,
        SourceLocationRange_class_ref,
        SourceLocationRange_constructor_id,
        SourceLocation_wrap(env, slocr_native.start),
        SourceLocation_wrap(env, slocr_native.end)
    );
}

// Get a native source location range from a Java wrapping instance
${sloc_range_type} SourceLocationRange_unwrap(
    JNIEnv *env,
    jobject slocr
) {
    // Create the result structure
    ${sloc_range_type} res = SourceLocationRange_new_value();

    // Get the fields value
    jobject start = (*env)->GetObjectField(
        env,
        slocr,
        SourceLocationRange_start_field_id
    );
    jobject end = (*env)->GetObjectField(
        env,
        slocr,
        SourceLocationRange_end_field_id
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
    ${diagnostic_type} diag_native
) {
    // Return the new diagnostic
    return (*env)->NewObject(
        env,
        Diagnostic_class_ref,
        Diagnostic_constructor_id,
        SourceLocationRange_wrap(env, diag_native.sloc_range),
        Text_wrap(env, diag_native.message)
    );
}

// Get a native diagnostic from a Java wrapping instance
${diagnostic_type} Diagnostic_unwrap(
    JNIEnv *env,
    jobject diagnostic
) {
    // Create the result structure
    ${diagnostic_type} res = Diagnostic_new_value();

    // Get the fields value
    jobject slocr = (*env)->GetObjectField(
        env,
        diagnostic,
        Diagnostic_sloc_range_field_id
    );
    jobject text = (*env)->GetObjectField(
        env,
        diagnostic,
        Diagnostic_text_field_id
    );

    // Fill the result structure
    res.sloc_range = SourceLocationRange_unwrap(env, slocr);
    res.message = Text_unwrap(env, text);

    // Return the result
    return res;
}

% if ctx.generate_unparser:
// ==========
// Rewriting apply result functions
// ==========

// Create a new vakye for an apply result
${rewriting_apply_result_type} RewritingApplyResult_new_value() {
    ${rewriting_apply_result_type} res = {
        0,
        NULL,
        0,
        NULL
    };
    return res;
}

// Wrap a native rewriting apply result
jobject RewritingApplyResult_wrap(
    JNIEnv *env,
    ${rewriting_apply_result_type} result_native
) {
    if(result_native.success > 0) {
        return (*env)->CallStaticObjectMethod(
            env,
            RewritingApplyResult_class_ref,
            RewritingApplyResult_success_id
        );
    }
    return (*env)->NewObject(
        env,
        RewritingApplyResult_class_ref,
        RewritingApplyResult_contructor_id,
        (jboolean) 0,
        AnalysisUnit_wrap(env, result_native.unit),
        result_native.diagnostics_count,
        PointerWrapper_wrap(env, (void *) result_native.diagnostics)
    );
}

${rewriting_apply_result_type} RewritingApplyResult_unwrap(
    JNIEnv *env,
    jobject rewriting_apply_result
) {
    ${rewriting_apply_result_type} res = RewritingApplyResult_new_value();
    res.success = (int) (*env)->GetBooleanField(
        env,
        rewriting_apply_result,
        RewritingApplyResult_success_field_id
    );
    res.unit = AnalysisUnit_unwrap(
        env,
        (*env)->GetObjectField(
            env,
            rewriting_apply_result,
            RewritingApplyResult_unit_field_id
        )
    );
    res.diagnostics_count = (int) (*env)->GetIntField(
        env,
        rewriting_apply_result,
        RewritingApplyResult_diagnostics_count_field_id
    );
    res.diagnostics = (${diagnostic_type} *) PointerWrapper_unwrap(
        env,
        (*env)->GetObjectField(
            env,
            rewriting_apply_result,
            RewritingApplyResult_diagnostics_reference_field_id
        )
    );
    return res;
}

// Function to get the wrapped diagnostics inside a rewriting apply result
${api.jni_func_sig("rewriting_get_result_diagnostics", "jobjectArray")}(
    JNIEnv *env,
    jclass jni_lib,
    jint diagnostics_count,
    jlong diagnostics_reference
) {
    // Create the result array
    jobjectArray res = (*env)->NewObjectArray(
        env,
        (jsize) diagnostics_count,
        Diagnostic_class_ref,
        NULL
    );

    // Fill the result array by wrapping all diagnostics
    ${diagnostic_type} *diagnostics_native =
        (${diagnostic_type} *) diagnostics_reference;
    for(int i = 0; i < (int) diagnostics_count; i++) {
        (*env)->SetObjectArrayElement(
            env,
            res,
            (jsize) i,
            Diagnostic_wrap(env, diagnostics_native[i])
        );
    }

    // Return the filled array
    return res;
}

// Function to free the rewriting apply result
${api.jni_func_sig("rewriting_free_apply_result", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_apply_result
) {
    ${rewriting_apply_result_type} result_native =
        RewritingApplyResult_unwrap(
            env,
            rewriting_apply_result
        );
    ${nat("rewriting_free_apply_result")}(&result_native);
}
% endif

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
    ${file_reader_type} file_reader_native
) {
    // Return the new file reader
    return (*env)->NewObject(
        env,
        FileReader_class_ref,
        FileReader_constructor_id,
        PointerWrapper_wrap(env, (void *) file_reader_native)
    );
}

// Get a native file reader from a Java wrapping instance
${file_reader_type} FileReader_unwrap(
    JNIEnv *env,
    jobject file_reader
) {
    return (${file_reader_type}) PointerWrapper_unwrap(
        env,
        (*env)->GetObjectField(
            env,
            file_reader,
            FileReader_reference_field_id
        )
    );
}

// Decrease the reference counter of the given file reader
${api.jni_func_sig("dec_ref_file_reader", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject file_reader
) {
    ${nat("dec_ref_file_reader")}(FileReader_unwrap(env, file_reader));
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
    ${unit_provider_type} unit_prov_native
) {
    // Return the new unit provider
    return (*env)->NewObject(
        env,
        UnitProvider_class_ref,
        UnitProvider_constructor_id,
        PointerWrapper_wrap(env, (void *) unit_prov_native)
    );
}

// Get a native unit provider from a Java wrapping instance
${unit_provider_type} UnitProvider_unwrap(
    JNIEnv *env,
    jobject unit_provider
) {
    return (${unit_provider_type}) PointerWrapper_unwrap(
        env,
        (*env)->GetObjectField(
            env,
            unit_provider,
            UnitProvider_reference_field_id
        )
    );
}

${api.jni_func_sig("dec_ref_unit_provider", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject unit_provider
) {
    ${nat("dec_ref_unit_provider")}(UnitProvider_unwrap(env, unit_provider));
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
    ${event_handler_type} event_handler_native
) {
    // Return the new event handler
    return (*env)->CallStaticObjectMethod(
        env,
        EventHandler_class_ref,
        EventHandler_from_reference_id,
        PointerWrapper_wrap(env, (void *) event_handler_native)
    );
}

// Get the native event handler from a Java wrapping instance
${event_handler_type} EventHandler_unwrap(
    JNIEnv *env,
    jobject event_handler
) {
    return (${event_handler_type}) PointerWrapper_unwrap(
        env,
        (*env)->GetObjectField(
            env,
            event_handler,
            EventHandler_reference_field_id
        )
    );
}

// Util function to destroy the native event handler
void event_handler_destroy(
    void *data
) {
    event_handler_data *eh_data = (event_handler_data *) data;
    JNIEnv *env = eh_data->env;
    if(jvm_running) {
        (*env)->DeleteGlobalRef(env, eh_data->unit_requested_callback);
        (*env)->DeleteGlobalRef(env, eh_data->unit_parsed_callback);
    }
    free(data);
}

// Util function called when a unit is requested
void event_handler_unit_requested(
    void *data,
    ${analysis_context_type} context,
    ${text_type} *name,
    ${analysis_unit_type} from,
    ${bool_type} found,
    ${bool_type} is_not_found_error
) {
    // Get the event handler data
    event_handler_data *eh_data = (event_handler_data *) data;
    JNIEnv *env = eh_data->env;

    // Verify that the callback is not null
    if(eh_data->unit_requested_callback != NULL) {
        // Wrap the callback arguments
        jobject analysis_context = AnalysisContext_wrap(env, context);
        jobject text = Text_wrap(env, *name);
        jobject analysis_unit = AnalysisUnit_wrap(env, from);

        // Call the unit requested callback
        (*env)->CallVoidMethod(
            env,
            eh_data->unit_requested_callback,
            UnitRequestedCallback_invoke_id,
            analysis_context,
            get_text_content(env, text),
            analysis_unit,
            (jboolean) found,
            (jboolean) is_not_found_error
        );

        // Remove the intermediary values
        ${nat("context_decref")}(context);
        ${nat("destroy_text")}(name);
    }
}

// Util function called when a unit is parsed
void event_handler_unit_parsed(
    void *data,
    ${analysis_context_type} context,
    ${analysis_unit_type} unit,
    ${bool_type} reparsed
) {
    // Get the event handler data
    event_handler_data *eh_data = (event_handler_data *) data;
    JNIEnv *env = eh_data->env;

    // Verify that the callback is not null
    if(eh_data->unit_parsed_callback != NULL) {
        // Wrap the callback arguments
        jobject analysis_context = AnalysisContext_wrap(env, context);
        jobject analysis_unit = AnalysisUnit_wrap(env, unit);

        // Call the unit parsed callback
        (*env)->CallVoidMethod(
            env,
            eh_data->unit_parsed_callback,
            UnitParsedCallback_invoke_id,
            analysis_context,
            analysis_unit,
            (jboolean) reparsed
        );

        // Remove the intermediary values
        ${nat("context_decref")}(context);
    }
}

// Create a new event handler
${api.jni_func_sig("create_event_handler", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject unit_requested_callback,
    jobject unit_parsed_callback
) {
    // Create the structure to store the event handler information
    event_handler_data *data = (event_handler_data *) malloc(
        sizeof(event_handler_data)
    );
    data->env = env;
    data->unit_requested_callback = (*env)->NewGlobalRef(
        env,
        unit_requested_callback
    );
    data->unit_parsed_callback = (*env)->NewGlobalRef(
        env,
        unit_parsed_callback
    );

    // Call the native function
    ${event_handler_type} res_native = ${nat("create_event_handler")}(
        (void *) data,
        &event_handler_destroy,
        &event_handler_unit_requested,
        &event_handler_unit_parsed
    );

    // Return the wrapped pointer
    return PointerWrapper_wrap(env, (void *) res_native);
}

// Decrease the reference counter of an event handler
${api.jni_func_sig("dec_ref_event_handler", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject event_handler
) {
    ${nat("dec_ref_event_handler")}(EventHandler_unwrap(env, event_handler));
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
        0
    };
    return res;
}

// Wrap a native token in the Java wrapping class
jobject Token_wrap(
    JNIEnv *env,
    ${token_type} token_native,
    jobject analysis_unit
) {
    ${token_kind} tok_kind;
    ${text_type} tok_text;
    ${sloc_range_type} tok_sloc_range;
    jobject text;
    jstring text_string;

    // Handle the no tokens
    if(token_native.token_data == NULL) {
        return NoToken_wrap(env, analysis_unit);
    }

    // Fetch extra token data requires to build a Token instance, but not
    // present in the C-level token data type: kind, text and sloc_range.
    tok_kind = ${nat("token_get_kind")} (&token_native);
    ${nat("token_sloc_range")} (&token_native, &tok_sloc_range);

    ${nat("token_range_text")} (&token_native, &token_native, &tok_text);
    text = Text_wrap(env, tok_text);
    text_string = get_text_content(env, text);
    ${nat("destroy_text")} (&tok_text);

    return (*env)->NewObject(
        env,
        Token_class_ref,
        Token_constructor_id,
        PointerWrapper_wrap(env, token_native.context),
        analysis_unit,
        PointerWrapper_wrap(env, token_native.token_data),
        (jint) token_native.token_index,
        (jint) token_native.trivia_index,
        TokenKind_wrap(env, tok_kind),
        text_string,
        SourceLocationRange_wrap(env, tok_sloc_range)
    );
}

// Get a native token from the Java wrapping instance
${token_type} Token_unwrap(
    JNIEnv *env,
    jobject token
) {
    // Prepare the result structure
    ${token_type} res = Token_new_value();

    // Check if the token is instance of no token
    if((*env)->IsInstanceOf(env, token, NoToken_class_ref)) {
        // Get the value from the object
        jobject context_value = (*env)->GetObjectField(
            env,
            token,
            Token_context_field_id
        );
        jobject tdh_value = (*env)->GetObjectField(
            env,
            token,
            Token_tdh_field_id
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

        // Return the result
        return res;

    } else {
        // Get the value from the object
        jobject context_value = (*env)->GetObjectField(
            env,
            token,
            Token_context_field_id
        );
        jobject tdh_value = (*env)->GetObjectField(
            env,
            token,
            Token_tdh_field_id
        );
        jint token_index_value = (*env)->GetIntField(
            env,
            token,
            Token_token_index_field_id
        );
        jint trivia_index_value = (*env)->GetIntField(
            env,
            token,
            Token_trivia_index_field_id
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

        // Return the result
        return res;

    }
}

// Get the analysis unit from a token
jobject Token_get_unit(
    JNIEnv *env,
    jobject token
) {
    // Return the field
    return (*env)->GetObjectField(
        env,
        token,
        Token_unit_field_id
    );
}

// Get the no token instance for the given analysis unit
jobject NoToken_wrap(
    JNIEnv *env,
    jobject analysis_unit
) {
    // Call the instance getter and return the result
    return (*env)->CallStaticObjectMethod(
        env,
        Token_class_ref,
        Token_none_getter_method_id,
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
    ${token_type} token_native = Token_unwrap(env, token);

    // Prepare the result
    ${token_type} res = Token_new_value();

    // Call the native function
    ${nat("token_next")}(
        &token_native,
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
    ${token_type} token_native = Token_unwrap(env, token);

    // Prepare the result
    ${token_type} res = Token_new_value();

    // Call the native function
    ${nat("token_previous")}(
        &token_native,
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
    ${token_type} left_native = Token_unwrap(env, left);
    ${token_type} right_native = Token_unwrap(env, right);

    // Return the result of the native call
    return (jboolean) ${nat("token_is_equivalent")}(
        &left_native,
        &right_native
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
    ${token_type} start_native = Token_unwrap(env, start);
    ${token_type} end_native = Token_unwrap(env, end);

    // Prepare the result
    ${text_type} res = Text_new_value();

    // Call the native function
    ${nat("token_range_text")}(
        &start_native,
        &end_native,
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
    ${analysis_context_type} context_native
) {
    // Return the new analysis context
    return (*env)->CallStaticObjectMethod(
        env,
        AnalysisContext_class_ref,
        AnalysisContext_from_reference_id,
        PointerWrapper_wrap(env, (void *) context_native)
    );
}

// Get a native analysis context from a Java wrapping instance
${analysis_context_type} AnalysisContext_unwrap(
    JNIEnv *env,
    jobject analysis_context
) {
    return (${analysis_context_type}) PointerWrapper_unwrap(
        env,
        (*env)->GetObjectField(
            env,
            analysis_context,
            AnalysisContext_reference_field_id
        )
    );
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
    const char* charset_native = NULL;
    if(charset != NULL) charset_native = to_c_string(env, charset);

    // Allocate the analysis context
    ${analysis_context_type} res = ${nat("allocate_analysis_context")}();

    // Call the native function
    ${nat("initialize_analysis_context")}(
        res,
        charset_native,
        FileReader_unwrap(env, file_reader),
        UnitProvider_unwrap(env, unit_provider),
        EventHandler_unwrap(env, event_handler),
        (int) with_trivia,
        (int) tab_stop
    );

    // Release the allocated string
    if(charset != NULL)
        release_c_string(env, charset, charset_native);

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
    ${analysis_unit_type} unit_native
) {
    // Return the new analysis unit
    return (*env)->NewObject(
        env,
        AnalysisUnit_class_ref,
        AnalysisUnit_constructor_id,
        PointerWrapper_wrap(env, (void *) unit_native)
    );
}

// Get a native analysis unit from a Java wrapping instance
${analysis_unit_type} AnalysisUnit_unwrap(
    JNIEnv *env,
    jobject analysis_unit
) {
    return (${analysis_unit_type}) PointerWrapper_unwrap(
        env,
        (*env)->GetObjectField(
            env,
            analysis_unit,
            AnalysisUnit_reference_field_id
        )
    );
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
    const char *filename_native = to_c_string(env, filename);
    const char *charset_native = NULL;
    if(charset != NULL) charset_native = to_c_string(env, charset);

    // Call the native function
    ${analysis_unit_type} res = ${nat("get_analysis_unit_from_file")}(
        AnalysisContext_unwrap(env, analysis_context),
        filename_native,
        charset_native,
        (int) reparse,
        (int) grammar_rule
    );

    // Release the strings
    release_c_string(env, filename, filename_native);
    if(charset != NULL)
        release_c_string(env, charset, charset_native);

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
    const char *filename_native = to_c_string(env, filename);
    const char *buffer_native = to_c_string(env, buffer);
    const char *charset_native = NULL;
    if(charset != NULL) charset_native = to_c_string(env, charset);

    // Call the native function
    ${analysis_unit_type} res = ${nat("get_analysis_unit_from_buffer")}(
        AnalysisContext_unwrap(env, analysis_context),
        filename_native,
        charset_native,
        buffer_native,
        (long) buffer_size,
        (int) grammar_rule
    );

    // Release the strings
    release_c_string(env, filename, filename_native);
    release_c_string(env, buffer, buffer_native);
    if(charset != NULL)
        release_c_string(env, charset, charset_native);

    // Return the new analysis unit
    return AnalysisUnit_wrap(env, res);
}

% if ctx.default_unit_provider:
// Create an analysis unit from the unit provider.
${api.jni_func_sig("get_analysis_unit_from_provider", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject context,
    jobject name,
    jint kind,
    jstring charset,
    jboolean reparse
) {
    ${text_type} name_c = Text_unwrap (env, name);
    const char *charset_c
      = (charset == NULL) ? NULL : to_c_string (env, charset);

    ${analysis_unit_type} result = ${nat("get_analysis_unit_from_provider")}(
        AnalysisContext_unwrap(env, context),
        &name_c,
        kind,
        charset_c,
        reparse
    );

    if (charset != NULL)
      release_c_string (env, charset, charset_c);
    ${nat("destroy_text")} (&name_c);

    return AnalysisUnit_wrap(env, result);
}
% endif

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
    ${nat("free")}((void *) res_native);

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

% if ctx.generate_unparser:
// ==========
// Rewriting context functions
// ==========

// Create a new value for a rewriting context
${rewriting_handle_type} RewritingContext_new_value() {
    return NULL;
}

// Wrap a native rewriting context in its Java class
jobject RewritingContext_wrap(
    JNIEnv *env,
    ${rewriting_handle_type} rewriting_context_native
) {
    return (*env)->CallStaticObjectMethod(
        env,
        RewritingContext_class_ref,
        RewritingContext_from_reference_id,
        PointerWrapper_wrap(env, (void *) rewriting_context_native)
    );
}

// Unwrap a Java rewriting context to its native value
${rewriting_handle_type} RewritingContext_unwrap(
    JNIEnv *env,
    jobject rewriting_context
) {
    return (${rewriting_handle_type}) PointerWrapper_unwrap(
        env,
        (*env)->GetObjectField(
            env,
            rewriting_context,
            RewritingContext_reference_field_id
        )
    );
}

// Start a rewriting session on the given analysis context
${api.jni_func_sig("rewriting_start_rewriting", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject analysis_context
) {
    // Call the native function to create a rewriting context and return the
    // wrapped result.
    return RewritingContext_wrap(
        env,
        ${nat("rewriting_start_rewriting")}(AnalysisContext_unwrap(
            env,
            analysis_context
        ))
    );
}

// Get the analysis context
${api.jni_func_sig("rewriting_handle_to_context", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_context
) {
    return AnalysisContext_wrap(
        env,
        ${nat("rewriting_handle_to_context")}(
            RewritingContext_unwrap(env, rewriting_context)
        )
    );
}

// Get the rewriting units of the context
${api.jni_func_sig("rewriting_unit_handles", "jobjectArray")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_context
) {
    // Call the native function to get the native rewriting units
    ${unit_rewriting_handle_type} *unit_array_native =
        ${nat("rewriting_unit_handles")}(
            RewritingContext_unwrap(env, rewriting_context)
        );

    // Count the number of units in the result
    unsigned int unit_count = 0;
    ${unit_rewriting_handle_type} *cursor = unit_array_native;
    while(*cursor != NULL) {
        cursor = &cursor[1];
        unit_count++;
    }

    // Create the result array and fill it
    jobjectArray res = (*env)->NewObjectArray(
        env,
        (jsize) unit_count,
        RewritingUnit_class_ref,
        NULL
    );
    for(unsigned int i = 0; i < unit_count; i++) {
        (*env)->SetObjectArrayElement(
            env,
            res,
            (jsize) i,
            RewritingUnit_wrap(env, unit_array_native[i])
        );
    }
    free(unit_array_native);
    return res;
}

// Create a new rewriting node in the given context
${api.jni_func_sig("rewriting_create_node", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_context,
    jint node_kind
) {
    return RewritingNode_wrap(
        env,
        ${nat("rewriting_create_node")}(
            RewritingContext_unwrap(env, rewriting_context),
            (${node_kind_type}) node_kind
        )
    );
}

// Create a new rewriting node with its children
${api.jni_func_sig("rewriting_create_regular_node", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_context,
    jint node_kind,
    jobjectArray children
) {
    // Unwrap the children
    ${node_rewriting_handle_type} *children_native =
        rewriting_nodes_to_native(env, children);

    // Call the native function and wrap the result
    jobject res = RewritingNode_wrap(
        env,
        ${nat("rewriting_create_regular_node")}(
            RewritingContext_unwrap(env, rewriting_context),
            (${node_kind_type}) node_kind,
            children_native,
            (*env)->GetArrayLength(env, children)
        )
    );

    // Free the native children array then return the result
    free(children_native);
    return res;
}

// Create a new rewriting token node in the given context
${api.jni_func_sig("rewriting_create_token_node", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_context,
    jint node_kind,
    jobject node_text
) {
    ${text_type} node_text_native = Text_unwrap(env, node_text);
    return RewritingNode_wrap(
        env,
        ${nat("rewriting_create_token_node")}(
            RewritingContext_unwrap(env, rewriting_context),
            (${node_kind_type}) node_kind,
            &node_text_native
        )
    );
}

// Create a new node tree from a template
${api.jni_func_sig("rewriting_create_from_template", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_context,
    jobject template_text,
    jobjectArray arguments,
    jint rule
) {
    // Unwrap the arguments and template text
    int count = (*env)->GetArrayLength(env, arguments);
    ${node_rewriting_handle_type} *arguments_native =
        rewriting_nodes_to_native(env, arguments);
    ${text_type} template_text_native = Text_unwrap(env, template_text);

    // Call the native function
    jobject res = RewritingNode_wrap(
        env,
        ${nat("rewriting_create_from_template")}(
            RewritingContext_unwrap(env, rewriting_context),
            &template_text_native,
            arguments_native,
            count,
            (${grammar_rule_type}) rule
        )
    );

    // Free the arguments native array the return the result
    free(arguments_native);
    return res;
}

// Apply the rewriting session and close it if success
${api.jni_func_sig("rewriting_apply", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_context
) {
    ${rewriting_apply_result_type} res_native =
        RewritingApplyResult_new_value();
    ${nat("rewriting_apply")}(
        RewritingContext_unwrap(env, rewriting_context),
        &res_native
    );
    return RewritingApplyResult_wrap(env, res_native);
}

// Abort the rewriting session
${api.jni_func_sig("rewriting_abort_rewriting", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_context
) {
    // Just call the native function
    ${nat("rewriting_abort_rewriting")}(
        RewritingContext_unwrap(env, rewriting_context)
    );
}

// ==========
// Rewriting unit functions
// ==========

// Create a new value for a native rewriting unit
${unit_rewriting_handle_type} RewritingUnit_new_value() {
    return NULL;
}

// Wrap the native rewrite unit inside the Java class
jobject RewritingUnit_wrap(
    JNIEnv *env,
    ${unit_rewriting_handle_type} rewriting_unit_native
) {
    return (*env)->NewObject(
        env,
        RewritingUnit_class_ref,
        RewritingUnit_constructor_id,
        PointerWrapper_wrap(env, (void *) rewriting_unit_native)
    );
}

// Unwrap the Java rewrite unit into a native value
${unit_rewriting_handle_type} RewritingUnit_unwrap(
    JNIEnv *env,
    jobject rewriting_unit
) {
    return (${unit_rewriting_handle_type}) PointerWrapper_unwrap(
        env,
        (*env)->GetObjectField(
            env,
            rewriting_unit,
            RewritingUnit_reference_field_id
        )
    );
}

// Get or create the rewriting unit from an analysis unit
${api.jni_func_sig("rewriting_unit_to_handle", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject analysis_unit
) {
    return RewritingUnit_wrap(
        env,
        ${nat("rewriting_unit_to_handle")}(
            AnalysisUnit_unwrap(env, analysis_unit)
        )
    );
}

// Get the analysis unit from the rewriting unit
${api.jni_func_sig("rewriting_handle_to_unit", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_unit
) {
    return AnalysisUnit_wrap(
        env,
        ${nat("rewriting_handle_to_unit")}(
            RewritingUnit_unwrap(env, rewriting_unit)
        )
    );
}

// Get the root rewriting node of the rewriting unit
${api.jni_func_sig("rewriting_unit_root", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_unit
) {
    return RewritingNode_wrap(
        env,
        ${nat("rewriting_unit_root")}(
            RewritingUnit_unwrap(env, rewriting_unit)
        )
    );
}

// Set the root rewriting node of the rewriting unit
${api.jni_func_sig("rewriting_unit_set_root", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_unit,
    jobject rewriting_node
) {
    ${nat("rewriting_unit_set_root")}(
        RewritingUnit_unwrap(env, rewriting_unit),
        RewritingNode_unwrap(env, rewriting_node)
    );
}

// Unparse the given rewriting unit
${api.jni_func_sig("rewriting_unit_unparse", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_unit
) {
    ${text_type} res_native = Text_new_value();
    ${nat("rewriting_unit_unparse")}(
        RewritingUnit_unwrap(env, rewriting_unit),
        &res_native
    );
    return Text_wrap(env, res_native);
}

// ==========
// Rewriting node functions
// ==========

// Create a new rewriting node native value
${node_rewriting_handle_type} RewritingNode_new_value() {
    return NULL;
}

// Wrap the native rewriting node
jobject RewritingNode_wrap(
    JNIEnv *env,
    ${node_rewriting_handle_type} rewriting_node_native
) {
    return (*env)->NewObject(
        env,
        RewritingNode_class_ref,
        RewritingNode_constructor_id,
        PointerWrapper_wrap(env, (void *) rewriting_node_native)
    );
}

// Unwrap the Java rewriting node
${node_rewriting_handle_type} RewritingNode_unwrap(
    JNIEnv *env,
    jobject rewriting_node
) {
    return (${node_rewriting_handle_type}) PointerWrapper_unwrap(
        env,
        (*env)->GetObjectField(
            env,
            rewriting_node,
            RewritingNode_reference_field_id
        )
    );
}

// Get the rewriting node from the parsed node
${api.jni_func_sig("rewriting_node_to_handle", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject entity
 ) {
    return RewritingNode_wrap(
        env,
        ${nat("rewriting_node_to_handle")}(
            Entity_unwrap(env, entity).node
        )
    );
}

// Get the parsed node from the given rewriting node
${api.jni_func_sig("rewriting_handle_to_node", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node
) {
    ${node_type} bare_node = ${nat("rewriting_handle_to_node")}(
        RewritingNode_unwrap(
            env,
            rewriting_node
        )
    );
    ${entity_type} res_native = Entity_new_value();
    ${nat("create_bare_entity")}(
        bare_node,
        &res_native
    );
    return Entity_wrap(env, res_native);
}

// Get the rewriting context of the given node
${api.jni_func_sig("rewriting_node_to_context", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node
) {
    return RewritingContext_wrap(
        env,
        ${nat("rewriting_node_to_context")}(
            RewritingNode_unwrap(env, rewriting_node)
        )
    );
}

// Clone the given rewriting node and return the result
${api.jni_func_sig("rewriting_clone", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject to_clone
) {
    return RewritingNode_wrap(
        env,
        ${nat("rewriting_clone")}(
            RewritingNode_unwrap(env, to_clone)
        )
    );
}

// Unparse the given rewriting node
${api.jni_func_sig("rewriting_node_unparse", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node
) {
    ${text_type} res_native = Text_new_value();
    ${nat("rewriting_node_unparse")}(
        RewritingNode_unwrap(env, rewriting_node),
        &res_native
    );
    return Text_wrap(
        env,
        res_native
    );
}

// Get the kind index of the rewriting node
${api.jni_func_sig("rewriting_kind", "jint")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node
) {
    return (jint) ${nat("rewriting_kind")}(
        RewritingNode_unwrap(env, rewriting_node)
    );
}

// Get the image of the rewriting node
${api.jni_func_sig("rewriting_node_image", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node
) {
    ${text_type} res_native = Text_new_value();
    ${nat("rewriting_node_image")}(
        RewritingNode_unwrap(env, rewriting_node),
        &res_native
    );
    return Text_wrap(env, res_native);
}

// Get whether the node is tied
${api.jni_func_sig("rewriting_tied", "jboolean")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node
) {
    return (jboolean) ${nat("rewriting_tied")}(
        RewritingNode_unwrap(env, rewriting_node)
    );
}

// Get the parent of the rewriting node
${api.jni_func_sig("rewriting_parent", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node
) {
    return RewritingNode_wrap(
        env,
        ${nat("rewriting_parent")}(
            RewritingNode_unwrap(env, rewriting_node)
        )
    );
}

// Get the rewriting node children
${api.jni_func_sig("rewriting_children", "jobjectArray")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node
) {
    // Call the native method
    ${node_rewriting_handle_type} *children = NULL;
    int count = 0;
    ${nat("rewriting_children")}(
        RewritingNode_unwrap(env, rewriting_node),
        &children,
        &count
    );

    // Create the new java array adn fill it
    jobjectArray res = (*env)->NewObjectArray(
        env,
        (jsize) count,
        RewritingNode_class_ref,
        NULL
    );
    for(int i = 0; i < count; i++) {
        (*env)->SetObjectArrayElement(
            env,
            res,
            (jsize) i,
            RewritingNode_wrap(env, children[i])
        );
    }

    // Free the native children
    free(children);

    // Return the result
    return res;
}

// Get the child of the rewriting node by its member reference
${api.jni_func_sig("rewriting_child", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_parent,
    jint child_member_reference
) {
    return RewritingNode_wrap(
        env,
        ${nat("rewriting_child")}(
            RewritingNode_unwrap(env, rewriting_parent),
            (${introspection_member_ref_type}) child_member_reference
        )
    );
}

// Set the given child at the given member reference
${api.jni_func_sig("rewriting_set_child", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject parent,
    jint child_member_reference,
    jobject new_child
) {
    ${nat("rewriting_set_child")}(
        RewritingNode_unwrap(env, parent),
        (${introspection_member_ref_type}) child_member_reference,
        RewritingNode_unwrap(env, new_child)
    );
}

// Replace the rewriting node with the new one.
${api.jni_func_sig("rewriting_replace", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node,
    jobject new_node
) {
    ${nat("rewriting_replace")}(
        RewritingNode_unwrap(env, rewriting_node),
        RewritingNode_unwrap(env, new_node)
    );
}

// Get the first child of the given parent
${api.jni_func_sig("rewriting_first_child", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_parent
) {
    return RewritingNode_wrap(
        env,
        ${nat("rewriting_first_child")}(
            RewritingNode_unwrap(env, rewriting_parent)
        )
    );
}

// Get the last child of the given parent
${api.jni_func_sig("rewriting_last_child", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_parent
) {
    return RewritingNode_wrap(
        env,
        ${nat("rewriting_last_child")}(
            RewritingNode_unwrap(env, rewriting_parent)
        )
    );
}

// Get the next child from the given rewriting node
${api.jni_func_sig("rewriting_next_child", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node
) {
    return RewritingNode_wrap(
        env,
        ${nat("rewriting_next_child")}(
            RewritingNode_unwrap(env, rewriting_node)
        )
    );
}

// Get the previous child from the given rewriting node
${api.jni_func_sig("rewriting_previous_child", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node
) {
    return RewritingNode_wrap(
        env,
        ${nat("rewriting_previous_child")}(
            RewritingNode_unwrap(env, rewriting_node)
        )
    );
}

// Insert the rewriting node before the other
${api.jni_func_sig("rewriting_insert_before", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node,
    jobject to_insert
) {
    ${nat("rewriting_insert_before")}(
        RewritingNode_unwrap(env, rewriting_node),
        RewritingNode_unwrap(env, to_insert)
    );
}

// Insert the rewriting node after the other
${api.jni_func_sig("rewriting_insert_after", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node,
    jobject to_insert
) {
    ${nat("rewriting_insert_after")}(
        RewritingNode_unwrap(env, rewriting_node),
        RewritingNode_unwrap(env, to_insert)
    );
}

// Insert the rewriting node at the beginning of children
${api.jni_func_sig("rewriting_insert_first", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node,
    jobject to_insert
) {
    ${nat("rewriting_insert_first")}(
        RewritingNode_unwrap(env, rewriting_node),
        RewritingNode_unwrap(env, to_insert)
    );
}

// Insert the rewriting node at the end of children
${api.jni_func_sig("rewriting_insert_last", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node,
    jobject to_insert
) {
    ${nat("rewriting_insert_last")}(
        RewritingNode_unwrap(env, rewriting_node),
        RewritingNode_unwrap(env, to_insert)
    );
}

// Remove the given node from its list parent
${api.jni_func_sig("rewriting_remove_child", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject to_remove
) {
    ${nat("rewriting_remove_child")}(
        RewritingNode_unwrap(env, to_remove)
    );
}

// Get the text of the given rewriting token node
${api.jni_func_sig("rewriting_text", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node
) {
    ${text_type} res_native = Text_new_value();
    ${nat("rewriting_text")}(
        RewritingNode_unwrap(env, rewriting_node),
        &res_native
    );
    return Text_wrap(env, res_native);
}

// Set the text of the given rewriting token node
${api.jni_func_sig("rewriting_set_text", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject rewriting_node,
    jobject text
) {
    ${text_type} text_native = Text_unwrap(env, text);
    ${nat("rewriting_set_text")}(
        RewritingNode_unwrap(env, rewriting_node),
        &text_native
    );
}
% endif

// ==========
// Generated structure functions
// ==========

% for struct_type in ctx.struct_types:
    % if api.should_emit_struct(struct_type):
${struct.jni_c_impl(struct_type)}
    % endif
% endfor

// ==========
// Generated array functions
// ==========

% for array_type in ctx.array_types:
    % if array_type.exposed:
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
// Node functions
// ==========

// Return whether the two given entities are equal
${api.jni_func_sig("node_is_equivalent", "jint")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject entity_left,
    jobject entity_right
) {
    // Unwrap the nodes
    ${entity_type} entity_left_native = Entity_unwrap(env, entity_left);
    ${entity_type} entity_right_native = Entity_unwrap(env, entity_right);

    // Call the native function
    return (jint) ${nat("node_is_equivalent")}(
        &entity_left_native,
        &entity_right_native
    );
}

// Get the hash of a node
${api.jni_func_sig("node_hash", "jint")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject entity
) {
    // Unwrap the node
    ${entity_type} entity_native = Entity_unwrap(env, entity);

    // Call the native function
    return (jint) ${nat("node_hash")}(
        &entity_native
    );
}

// Get the kind of a node
${api.jni_func_sig("node_kind", "jint")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject entity
) {
    // Unwrap the node
    ${entity_type} entity_native = Entity_unwrap(env, entity);

    // Call the native function
    return (jint) ${nat("node_kind")}(
        &entity_native
    );
}

// Get the text of a node
${api.jni_func_sig("node_text", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject entity
) {
    // Unwrap the node
    ${entity_type} entity_native = Entity_unwrap(env, entity);

    // Prepare the result
    ${text_type} res = Text_new_value();

    // Call the native function
    ${nat("node_text")}(
        &entity_native,
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
    ${entity_type} entity_native = Entity_unwrap(env, entity);

    // Prepare the result
    ${sloc_range_type} res = SourceLocationRange_new_value();

    // Call the native function
    ${nat("node_sloc_range")}(
        &entity_native,
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
    ${entity_type} entity_native = Entity_unwrap(env, entity);

    // Call the native function
    return (jint) ${nat("node_children_count")}(
        &entity_native
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
    ${entity_type} entity_native = Entity_unwrap(env, entity);

    // Preapre the result
    ${entity_type} res = Entity_new_value();

    // Call the native function
    ${nat("node_child")}(
        &entity_native,
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
    ${entity_type} entity_native = Entity_unwrap(env, entity);

    // Call the native function
    return (jboolean) ${nat("node_is_token_node")}(
        &entity_native
    );
}

// Get the text image of a node
${api.jni_func_sig("node_unit", "jobject")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject entity
) {
    // Unwrap the node
    ${entity_type} entity_native = Entity_unwrap(env, entity);

    // Call the native function
    ${analysis_unit_type} res_native = ${nat("node_unit")}(
        &entity_native
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
    ${entity_type} entity_native = Entity_unwrap(env, entity);

    // Prepare the result
    ${text_type} res = Text_new_value();

    // Call the native function
    ${nat("node_image")}(
        &entity_native,
        &res
    );

    // Return the image text
    return Text_wrap(env, res);
}

// ==========
// Node field accessors
// ==========

% for astnode in ctx.astnode_types:
    % for field in astnode.fields_with_accessors():
${ast_node.jni_field_accessor(field)}
    % endfor
% endfor
