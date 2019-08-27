## vim: filetype=makocpp

<%namespace name="array_types"   file="array_types_c.mako" />
<%namespace name="struct_types"  file="struct_types_c.mako" />
<%namespace name="astnode_types" file="astnode_types_c.mako" />
<%namespace name="exts"          file="../extensions.mako" />

<% entity_type = root_entity.c_type(capi).name %>

#ifndef ${capi.header_guard_id}
#define ${capi.header_guard_id}

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

${c_doc('langkit.initialize')}
void ${capi.lib_name}_initialize(void);

${c_doc('langkit.analysis_context_type')}
typedef struct
{
   uint64_t serial_number;
} *${analysis_context_type};

${c_doc('langkit.analysis_unit_type')}
typedef struct
{
   uint64_t version_number;
} *${analysis_unit_type};

${c_doc('langkit.node_type')}
typedef void* ${node_type};

${c_doc('langkit.node_kind_type')}
typedef enum {
% for astnode in ctx.astnode_types:
    <% name = astnode.kwless_raw_name %>
    % if astnode.abstract:

        /* ${name} (abstract)  */
        ${c_doc(astnode, 8)}
    % else:

        ${c_doc(astnode, 8)}
        ${capi.get_name(name)} = ${ctx.node_kind_constants[astnode]},
    % endif
% endfor
} ${node_kind_type};

${c_doc('langkit.symbol_type')}
typedef struct {
   void *data;
   void *bounds;
} ${symbol_type};

${c_doc('langkit.env_rebindings_type')}
typedef void *${env_rebindings_type};

typedef uint8_t ${bool_type};

/* Helper data structures for source location handling.  */

${c_doc('langkit.sloc_type')}
typedef struct {
    uint32_t line;
    uint16_t column;
} ${sloc_type};

${c_doc('langkit.sloc_range_type')}
typedef struct {
    ${sloc_type} start;
    ${sloc_type} end;
} ${sloc_range_type};


${c_doc('langkit.text_type')}
typedef struct {
   ${c_doc('langkit.text_type.chars')}
    uint32_t *chars;
   ${c_doc('langkit.text_type.length')}
    size_t length;

    int is_allocated;
} ${text_type};

${c_doc('langkit.big_integer_type')}
typedef void *${big_integer_type};

${c_doc('langkit.token_kind')}
typedef enum {
   <% lexer = ctx.lexer %>
   % for i, t in enumerate(lexer.sorted_tokens):
      ${',' if i > 0 else ''}
      ${t.c_name} = ${t.value}
   % endfor
} ${token_kind};

${c_doc('langkit.token_reference_type')}
typedef struct {
    /* Private data associated to this token or NULL if this designates no
       token.  */
    void *token_data;

    /* Internal identifiers for this token.  */
    int token_index, trivia_index;

    ${token_kind} kind;
    ${text_type} text;
    ${sloc_range_type} sloc_range;
} ${token_type};


${c_doc('langkit.diagnostic_type')}
typedef struct {
    ${sloc_range_type} sloc_range;
    ${text_type} message;
} ${diagnostic_type};

% for enum_type in ctx.enum_types:
   typedef enum {
      ${', '.join(v.c_name(capi) for v in enum_type.values)}
   } ${enum_type.c_type(capi).name};
   ${c_doc(enum_type, 3)}
% endfor

const ${grammar_rule_type} ${default_grammar_rule} = ${
    T.GrammarRule.values_dict[ctx.main_rule_api_name].c_name(capi)
};

${c_doc('langkit.exception_kind_type')}
typedef enum {
   % for _, exc in ctx.sorted_exception_types:
      ${ctx.exception_kind_name(exc).upper},
   % endfor
} ${exception_kind_type};

${c_doc('langkit.exception_type')}
typedef struct {
   ${c_doc('langkit.exception_type.kind')}
   ${exception_kind_type} kind;

   ${c_doc('langkit.exception_type.information')}
   const char *information;
} ${exception_type};

/*
 * Array types incomplete declarations
 */

${array_types.incomplete_decl(T.root_node.array)}
${array_types.incomplete_decl(T.entity.array)}

% for array_type in ctx.array_types:
    % if array_type.element_type.should_emit_array_type and \
            array_type.exposed and \
            array_type.emit_c_type:
        ${array_types.incomplete_decl(array_type)}
    % endif
% endfor

/*
 * Struct types declarations
 */

## Even when metadata and entity structures are not exposed, we need to
## emit their type definition them for low-level interfacing.
% for struct_type in ctx.struct_types:
    % if struct_type in (T.env_md, T.entity_info) or ( \
         struct_type.exposed and \
         struct_type.emit_c_type \
    ):
        ${struct_types.decl(struct_type)}
    % endif
% endfor

/*
 * Types for unit providers
 */

${c_doc('langkit.unit_provider_type')}
typedef void *${unit_provider_type};

${c_doc('langkit.unit_provider_destroy_type')}
typedef void (*${unit_provider_destroy_type})(void *data);

${c_doc('langkit.unit_provider_get_unit_filename_type')}
typedef char *(*${unit_provider_get_unit_filename_type})(
   void *data,
   ${text_type} *name,
   ${unit_kind_type} kind
);

${c_doc('langkit.unit_provider_get_unit_from_name_type')}
typedef ${analysis_unit_type} (*${unit_provider_get_unit_from_name_type})(
   void *data,
   ${analysis_context_type} context,
   ${text_type} *name,
   ${unit_kind_type} kind,
   const char *charset,
   int reparse
);

/* All the functions below can potentially raise an exception, so
   ${capi.get_name("get_last_exception")} must be checked after them even
   before trying to use the returned value.  */


/*
 * Array types declarations
 */

${array_types.decl(T.root_node.array)}
${array_types.decl(T.entity.array)}

% for array_type in ctx.array_types:
    % if array_type.element_type.should_emit_array_type and \
            array_type.exposed and \
            array_type.emit_c_type:
        ${array_types.decl(array_type)}
    % endif
% endfor

/*
 * Analysis primitives
 */

${c_doc('langkit.create_context')}
extern ${analysis_context_type}
${capi.get_name("create_analysis_context")}(
   const char *charset,
   ${unit_provider_type} unit_provider,
   int with_trivia,
   int tab_stop
);

${c_doc('langkit.context_incref')}
extern ${analysis_context_type}
${capi.get_name("context_incref")}(${analysis_context_type} context);

${c_doc('langkit.context_decref')}
extern void
${capi.get_name("context_decref")}(${analysis_context_type} context);

${c_doc('langkit.context_symbol')}
extern int
${capi.get_name("context_symbol")}(${analysis_context_type} context,
                                   ${text_type} *text,
                                   ${symbol_type} *symbol);

${c_doc('langkit.context_discard_errors_in_populate_lexical_env')}
extern void
${capi.get_name("context_discard_errors_in_populate_lexical_env")}(
        ${analysis_context_type} context,
        int discard);

${c_doc('langkit.get_unit_from_file')}
extern ${analysis_unit_type}
${capi.get_name("get_analysis_unit_from_file")}(
        ${analysis_context_type} context,
        const char *filename,
        const char *charset,
        int reparse,
        ${grammar_rule_type} rule);

${c_doc('langkit.get_unit_from_buffer')}
extern ${analysis_unit_type}
${capi.get_name("get_analysis_unit_from_buffer")}(
        ${analysis_context_type} context,
        const char *filename,
        const char *charset,
        const char *buffer,
        size_t buffer_size,
        ${grammar_rule_type} rule);

% if ctx.default_unit_provider:
${c_doc('langkit.get_unit_from_provider')}
extern ${analysis_unit_type}
${capi.get_name("get_analysis_unit_from_provider")}(
        ${analysis_context_type} context,
        ${text_type} *name,
        ${unit_kind_type} kind,
        const char *charset,
        int reparse);
% endif

${c_doc('langkit.unit_root')}
extern void
${capi.get_name("unit_root")}(${analysis_unit_type} unit,
                              ${entity_type} *result_p);

${c_doc('langkit.unit_first_token')}
extern void
${capi.get_name('unit_first_token')}(${analysis_unit_type} unit,
                                     ${token_type} *token);

${c_doc('langkit.unit_last_token')}
extern void
${capi.get_name('unit_last_token')}(${analysis_unit_type} unit,
                                    ${token_type} *token);

${c_doc('langkit.unit_token_count')}
extern int
${capi.get_name('unit_token_count')}(${analysis_unit_type} unit);

${c_doc('langkit.unit_trivia_count')}
extern int
${capi.get_name('unit_trivia_count')}(${analysis_unit_type} unit);

${c_doc('langkit.unit_filename')}
extern char *
${capi.get_name('unit_filename')}(${analysis_unit_type} unit);

${c_doc('langkit.unit_diagnostic_count')}
extern unsigned
${capi.get_name("unit_diagnostic_count")}(${analysis_unit_type} unit);

${c_doc('langkit.unit_diagnostic')}
extern int
${capi.get_name("unit_diagnostic")}(${analysis_unit_type} unit,
                                    unsigned n,
                                    ${diagnostic_type} *diagnostic_p);

${c_doc('langkit.unit_context')}
extern ${analysis_context_type}
${capi.get_name("unit_context")}(${analysis_unit_type} context);

${c_doc('langkit.unit_reparse_file')}
extern void
${capi.get_name("unit_reparse_from_file")}(${analysis_unit_type} unit,
                                           const char *charset);

${c_doc('langkit.unit_reparse_buffer')}
extern void
${capi.get_name("unit_reparse_from_buffer")} (${analysis_unit_type} unit,
                                              const char *charset,
                                              const char *buffer,
                                              size_t buffer_size);

${c_doc('langkit.unit_populate_lexical_env')}
extern int
${capi.get_name("unit_populate_lexical_env")}(${analysis_unit_type} unit);

/*
 * General AST node primitives
 */

${c_doc('langkit.node_is_null')}
static inline int
${capi.get_name("node_is_null")}(${entity_type} *node) {
    return node->node == NULL;
}

${c_doc('langkit.node_kind')}
extern ${node_kind_type}
${capi.get_name("node_kind")}(${entity_type} *node);

${c_doc('langkit.kind_name')}
extern void
${capi.get_name("kind_name")}(${node_kind_type} kind, ${text_type} *result);

${c_doc('langkit.node_unit')}
extern int
${capi.get_name("node_unit")}(${entity_type} *node,
                              ${analysis_unit_type} *unit_p);

${c_doc('langkit.node_is_token_node')}
extern int
${capi.get_name("node_is_token_node")}(${entity_type} *node);

${c_doc('langkit.node_is_synthetic')}
extern int
${capi.get_name("node_is_synthetic")}(${entity_type} *node);

${c_doc('langkit.node_short_image')}
extern void
${capi.get_name("node_short_image")}(${entity_type} *node,
                                     ${text_type} *result);

${c_doc('langkit.node_text')}
extern void
${capi.get_name("node_text")}(${entity_type} *node,
                              ${text_type} *text);

${c_doc('langkit.node_sloc_range')}
extern void
${capi.get_name("node_sloc_range")}(${entity_type} *node,
                                    ${sloc_range_type} *sloc_range);

${c_doc('langkit.lookup_in_node')}
extern void
${capi.get_name("lookup_in_node")}(${entity_type} *node,
                                   const ${sloc_type} *sloc,
                                   ${entity_type} *result_p);

${c_doc('langkit.node_children_count')}
extern unsigned
${capi.get_name("node_children_count")}(${entity_type} *node);

${c_doc('langkit.node_child')}
extern int
${capi.get_name("node_child")}(${entity_type} *node,
                               unsigned n,
                               ${entity_type}* child_p);

${c_doc('langkit.text_to_locale_string')}
extern char *
${capi.get_name("text_to_locale_string")}(${text_type} *text);

${c_doc('langkit.free')}
extern void
${capi.get_name("free")}(void *address);

${c_doc('langkit.destroy_text')}
extern void
${capi.get_name("destroy_text")}(${text_type} *text);

${c_doc('langkit.symbol_text')}
extern void
${capi.get_name("symbol_text")}(${symbol_type} *symbol,
                                ${text_type} *text);

${c_doc('langkit.create_big_integer')}
extern ${big_integer_type}
${capi.get_name("create_big_integer")}(${text_type} *text);

${c_doc('langkit.big_integer_text')}
extern void
${capi.get_name("big_integer_text")}(${big_integer_type} bigint,
                                     ${text_type} *text);

${c_doc('langkit.big_integer_decref')}
extern void
${capi.get_name("big_integer_decref")}(${big_integer_type} bigint);

/*
 * Kind-specific AST node primitives
 */

/* All these primitives return their result through an OUT parameter.  They
   return a boolean telling whether the operation was successful (it can fail
   if the node does not have the proper type, for instance).  When an AST node
   is returned, its ref-count is left as-is.  */

% for astnode in ctx.astnode_types:
    % for field in astnode.fields_with_accessors():
        ${astnode_types.accessor_decl(field)}
    % endfor
% endfor

/*
 * Unit providers
 */

${c_doc('langkit.create_unit_provider')}
extern ${unit_provider_type}
${capi.get_name('create_unit_provider')}(
   void *data,
   ${unit_provider_destroy_type} destroy_func,
   ${unit_provider_get_unit_filename_type} get_unit_filename_func,
   ${unit_provider_get_unit_from_name_type} get_unit_from_name_func
);

${c_doc('langkit.unit_provider_destroy')}
extern void
${capi.get_name('destroy_unit_provider')}(void *data);

${exts.include_extension(
   ctx.ext('analysis', 'c_api', 'unit_providers', 'header')
)}

/*
 * Misc
 */

${c_doc('langkit.get_last_exception')}
extern const ${exception_type} *
${capi.get_name('get_last_exception')}(void);

${c_doc('langkit.token_kind_name')}
extern char *
${capi.get_name('token_kind_name')}(${token_kind} kind);

${c_doc('langkit.token_next')}
extern void
${capi.get_name('token_next')}(${token_type} *token,
                               ${token_type} *next_token);

${c_doc('langkit.token_previous')}
extern void
${capi.get_name('token_previous')}(${token_type} *token,
                                   ${token_type} *previous_token);

${c_doc('langkit.token_range_text')}
extern int
${capi.get_name('token_range_text')}(${token_type} *first,
                                     ${token_type} *last,
                                     ${text_type} *result);

${c_doc('langkit.token_is_equivalent')}
extern void
${capi.get_name('token_is_equivalent')}(${token_type} *left,
                                        ${token_type} *right);

${c_doc('langkit.entity_image')}
extern void
${capi.get_name('entity_image')}(${entity_type} ent, ${text_type} *result);

#ifdef __cplusplus
}
#endif

#endif
