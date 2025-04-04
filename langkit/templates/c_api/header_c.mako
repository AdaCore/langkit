## vim: filetype=makocpp

<%namespace name="array_types"    file="array_types_c.mako" />
<%namespace name="iterator_types" file="iterator_types_c.mako" />
<%namespace name="struct_types"   file="struct_types_c.mako" />
<%namespace name="astnode_types"  file="astnode_types_c.mako" />
<%namespace name="exts"           file="../extensions.mako" />

<%
    entity_type = root_entity.c_type(capi).name

    def define_opaque_ptr(name):
        """
        Return a type declaration for an opaque pointer with the given name.
        """
        return f"typedef struct {name}__struct *{name};"
%>

#ifndef ${capi.header_guard_id}
#define ${capi.header_guard_id}

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

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
${define_opaque_ptr(node_type)}

${c_doc('langkit.node_kind_type')}
typedef enum {
% for astnode in ctx.node_types:
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
   uint32_t thin_sym;
   void *table;
} ${symbol_type};

${c_doc('langkit.string_type')}
typedef struct {
   int length;
   int ref_count;
   uint32_t content[1];
} *${string_type};

${c_doc('langkit.env_rebindings_type')}
${define_opaque_ptr(env_rebindings_type)}

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
${define_opaque_ptr(big_integer_type)}

${c_doc('langkit.token_kind')}
typedef enum {
   <% lexer = ctx.lexer %>
   % for i, t in enumerate(lexer.sorted_tokens):
      ${',' if i > 0 else ''}
      ${t.c_name} = ${t.value}
   % endfor
} ${token_kind};

typedef struct
{
   uint64_t version;
} *${tdh_ptr_type};

${c_doc('langkit.token_reference_type')}
typedef struct {
    /* Private data associated to this token, including stale reference
       checking data, or NULL if this designates no token.  */
    ${analysis_context_type} context;
    ${tdh_ptr_type} token_data;

    /* Internal identifiers for this token.  */
    int token_index, trivia_index;
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

#define ${default_grammar_rule} ${
    T.GrammarRule.values_dict[ctx.main_rule_api_name].c_name(capi)
}

${c_doc('langkit.exception_kind_type')}
typedef enum {
   % for e in ctx.sorted_exception_types:
      ${e.kind_name.upper},
   % endfor
} ${exception_kind_type};

${c_doc('langkit.stack_trace_type')}
typedef void *${stack_trace_type};

${c_doc('langkit.exception_type')}
typedef struct {
   ${c_doc('langkit.exception_type.kind')}
   ${exception_kind_type} kind;

   ${c_doc('langkit.exception_type.information')}
   const char *information;

   ${c_doc('langkit.exception_type.stack_trace')}
   ${stack_trace_type} stack_trace;
} ${exception_type};

/*
 * Array types incomplete declarations
 */

% for array_type in ctx.array_types:
    % if array_type.exposed and array_type.emit_c_type:
        ${array_types.incomplete_decl(array_type)}
    % endif
% endfor

/*
 * Iterator types incomplete declarations
 */

${c_doc('langkit.iterator_type')}

${iterator_types.incomplete_decl(T.entity.iterator)}

% for iterator_type in ctx.iterator_types:
    % if iterator_type.exposed and iterator_type.emit_c_type:
        ${iterator_types.incomplete_decl(iterator_type)}
    % endif
% endfor

/*
 * Struct types declarations
 */

## Even when metadata and entity structures are not exposed, we need to
## emit their type definition them for low-level interfacing.
% for struct_type in ctx.struct_types:
    % if struct_type in (T.env_md, T.EntityInfo) or ( \
         struct_type.exposed and \
         struct_type.emit_c_type \
    ):
        ${struct_types.decl(struct_type)}
    % endif
% endfor

/*
 * Types for event handler
 */

${c_doc('langkit.event_handler_type')}
${define_opaque_ptr(event_handler_type)}

${c_doc('langkit.event_handler_unit_requested_callback')}
typedef void (*${event_handler_unit_requested_type})(
   void *data,
   ${analysis_context_type} context,
   ${text_type} *name,
   ${analysis_unit_type} from,
   ${bool_type} found,
   ${bool_type} is_not_found_error
);

${c_doc('langkit.event_handler_destroy_callback')}
typedef void (*${event_handler_destroy_type})(void *data);

${c_doc('langkit.event_handler_unit_parsed_callback')}
typedef void (*${event_handler_unit_parsed_type})(
   void *data,
   ${analysis_context_type} context,
   ${analysis_unit_type} unit,
   ${bool_type} reparsed
);

/*
 * Types for file readers
 */

${c_doc('langkit.file_reader_type')}
${define_opaque_ptr(file_reader_type)}

${c_doc('langkit.file_reader_destroy_type')}
typedef void (*${file_reader_destroy_type})(void *data);

${c_doc('langkit.file_reader_read_type')}
typedef void (*${file_reader_read_type})(
   void *data,
   const char *filename,
   const char *charset,
   int read_bom,
   ${text_type} *buffer,
   ${diagnostic_type} *diagnostic
);

/*
 * Types for unit providers
 */

${c_doc('langkit.unit_provider_type')}
${define_opaque_ptr(unit_provider_type)}

/*
 * Types for introspection
 */

/* References to struct/node members.  */
typedef enum {
   % for i, m in enumerate(generic_api.all_members, 1):
      ${capi.symbol_prefix}_member_ref_${generic_api.member_name(m).lower()}
        = ${i},
   % endfor
} ${introspection_member_ref_type};

% if ctx.generate_unparsers:
/*
 * Types for tree rewriting
 */

${c_doc('langkit.rewriting.rewriting_handle_type')}
${define_opaque_ptr(rewriting_handle_type)}

${c_doc('langkit.rewriting.unit_rewriting_handle_type')}
${define_opaque_ptr(unit_rewriting_handle_type)}

${c_doc('langkit.rewriting.node_rewriting_handle_type')}
${define_opaque_ptr(node_rewriting_handle_type)}

${c_doc('langkit.rewriting.apply_result_type')}
typedef struct {
    int success;
    ${analysis_unit_type} unit;
    int diagnostics_count;
    ${diagnostic_type} *diagnostics;
} ${rewriting_apply_result_type};
% endif

/* All the functions below can potentially raise an exception, so
   ${capi.get_name("get_last_exception")} must be checked after them even
   before trying to use the returned value.  */


/*
 * Array types declarations
 */

% for array_type in ctx.array_types:
    % if array_type.exposed and array_type.emit_c_type:
        ${array_types.decl(array_type)}
    % endif
% endfor

/*
 * Iterator types declarations
 */

${iterator_types.decl(T.entity.iterator)}

% for iterator_type in ctx.iterator_types:
    % if iterator_type.exposed and iterator_type.emit_c_type:
        ${iterator_types.decl(iterator_type)}
    % endif
% endfor

/*
 * Analysis primitives
 */

${c_doc('langkit.allocate_context')}
extern ${analysis_context_type}
${capi.get_name("allocate_analysis_context")} (void);

${c_doc('langkit.initialize_context')}
extern void
${capi.get_name("initialize_analysis_context")}(
   ${analysis_context_type} context,
   const char *charset,
   ${file_reader_type} file_reader,
   ${unit_provider_type} unit_provider,
   ${event_handler_type} event_handler,
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

% if cfg.library.defaults.unit_provider:
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

${c_doc('langkit.unit_dump_lexical_env')}
extern void
${capi.get_name('unit_dump_lexical_env')}(${analysis_unit_type} unit);

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
${capi.get_name("unit_populate_lexical_env")}(
    ${analysis_unit_type} unit
    % if ctx.ple_unit_root:
       , int ple_root_index
    % endif
);

/*
 * General AST node primitives
 */

${c_doc('langkit.create_bare_entity')}
extern void
${capi.get_name("create_bare_entity")}(
    ${node_type} node,
    ${entity_type} *entity
);

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
extern ${analysis_unit_type}
${capi.get_name("node_unit")}(${entity_type} *node);

${c_doc('langkit.node_hash')}
extern uint32_t
${capi.get_name("node_hash")}(${entity_type} *node);

${c_doc('langkit.node_is_equivalent')}
extern ${bool_type}
${capi.get_name("node_is_equivalent")}(${entity_type} *l, ${entity_type} *r);

${c_doc('langkit.node_is_token_node')}
extern int
${capi.get_name("node_is_token_node")}(${entity_type} *node);

${c_doc('langkit.node_is_synthetic')}
extern int
${capi.get_name("node_is_synthetic")}(${entity_type} *node);

${c_doc('langkit.node_image')}
extern void
${capi.get_name("node_image")}(${entity_type} *node,
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

${c_doc('langkit.text_to_utf8')}
extern void
${capi.get_name("text_to_utf8")}(${text_type} *text,
                                 char **bytes,
                                 size_t *length);

${c_doc('langkit.text_from_utf8')}
extern void
${capi.get_name("text_from_utf8")}(const char *bytes,
                                   size_t length,
                                   ${text_type} *text);

${c_doc('langkit.char_to_utf8')}
extern void
${capi.get_name("char_to_utf8")}(uint32_t codepoint,
                                 char **bytes,
                                 size_t *length);

${c_doc('langkit.char_from_utf8')}
extern void
${capi.get_name("char_from_utf8")}(const char *bytes,
                                   size_t length,
                                   uint32_t *codepoint);

${c_doc('langkit.string_to_utf8')}
extern void
${capi.get_name("string_to_utf8")}(${string_type} string,
                                   char **bytes,
                                   size_t *length);

${c_doc('langkit.string_from_utf8')}
extern void
${capi.get_name("string_from_utf8")}(const char *bytes,
                                     size_t length,
                                     ${string_type} *string);

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

${c_doc('langkit.get_versions')}
extern void
${capi.get_name("get_versions")}(char **version, char **build_date);

${c_doc('langkit.create_string')}
extern ${string_type}
${capi.get_name("create_string")}(uint32_t *content, int length);

${c_doc('langkit.string_dec_ref')}
extern void
${capi.get_name("string_dec_ref")}(${string_type} self);

/*
 * Kind-specific AST node primitives
 */

/* All these primitives return their result through an OUT parameter.  They
   return a boolean telling whether the operation was successful (it can fail
   if the node does not have the proper type, for instance).  When an AST node
   is returned, its ref-count is left as-is.  */

% for astnode in ctx.node_types:
    % for field in astnode.fields_with_accessors():
        ${astnode_types.accessor_decl(field)}
    % endfor
% endfor

/*
 * Event handlers
 */

${c_doc('langkit.create_event_handler')}
extern ${event_handler_type}
${capi.get_name('create_event_handler')}(
   void *data,
   ${event_handler_destroy_type} destroy_func,
   ${event_handler_unit_requested_type} unit_requested_func,
   ${event_handler_unit_parsed_type} unit_parsed_func
);

${c_doc('langkit.event_handler_dec_ref')}
extern void
${capi.get_name('dec_ref_event_handler')}(${event_handler_type} self);

/*
 * File readers
 */

${c_doc('langkit.create_file_reader')}
extern ${file_reader_type}
${capi.get_name('create_file_reader')}(
   void *data,
   ${file_reader_destroy_type} destroy_func,
   ${file_reader_read_type} read_func
);

${c_doc('langkit.file_reader_dec_ref')}
extern void
${capi.get_name('dec_ref_file_reader')}(${file_reader_type} self);

${exts.include_extension(
   ctx.ext('analysis', 'c_api', 'file_readers', 'header')
)}

/*
 * Unit providers
 */

${c_doc('langkit.unit_provider_dec_ref')}
extern void
${capi.get_name('dec_ref_unit_provider')}(void *data);

${exts.include_extension(
   ctx.ext('analysis', 'c_api', 'unit_providers', 'header')
)}

/*
 * Stack traces
 */

${c_doc('langkit.stack_trace_size')}
extern int
${capi.get_name('stack_trace_size')}(${stack_trace_type} trace);

${c_doc('langkit.stack_trace_element')}
extern void *
${capi.get_name('stack_trace_element')}(${stack_trace_type} trace, int index);

${c_doc('langkit.create_stack_trace')}
extern ${stack_trace_type}
${capi.get_name('create_stack_trace')}(int size, void **elements);

${c_doc('langkit.destroy_stack_trace')}
extern void
${capi.get_name('destroy_stack_trace')}(${stack_trace_type} trace);

${c_doc('langkit.symbolize_stack_trace')}
extern char *
${capi.get_name('symbolize_stack_trace')}(${stack_trace_type} trace);

/*
 * Misc
 */

${c_doc('langkit.get_last_exception')}
extern const ${exception_type} *
${capi.get_name('get_last_exception')}(void);

${c_doc('langkit.exception_name')}
extern char *
${capi.get_name('exception_name')}(${exception_kind_type} kind);

${c_doc('langkit.token_kind')}
extern int
${capi.get_name('token_get_kind')}(${token_type} *token);

${c_doc('langkit.token_kind_name')}
extern char *
${capi.get_name('token_kind_name')}(${token_kind} kind);

${c_doc('langkit.token_sloc_range')}
extern void
${capi.get_name('token_sloc_range')}(${token_type} *token,
                                     ${sloc_range_type} *result);

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
extern ${bool_type}
${capi.get_name('token_is_equivalent')}(${token_type} *left,
                                        ${token_type} *right);

% if ctx.generate_unparsers:
/*
 * Tree rewriting
 */

/* ... context rewriting... */

${c_doc('langkit.rewriting.context_handle')}
extern ${rewriting_handle_type}
${capi.get_name('rewriting_context_to_handle')}(
    ${analysis_context_type} context
);

${c_doc('langkit.rewriting.handle_context')}
extern ${analysis_context_type}
${capi.get_name('rewriting_handle_to_context')}(
    ${rewriting_handle_type} handle
);

${c_doc('langkit.rewriting.start_rewriting')}
extern ${rewriting_handle_type}
${capi.get_name('rewriting_start_rewriting')}(
    ${analysis_context_type} context
);

${c_doc('langkit.rewriting.abort_rewriting')}
extern void
${capi.get_name('rewriting_abort_rewriting')}(
    ${rewriting_handle_type} context
);

${c_doc('langkit.rewriting.apply')}
extern void
${capi.get_name('rewriting_apply')}(
    ${rewriting_handle_type} context,
    ${rewriting_apply_result_type} *result
);

${c_doc('langkit.rewriting.free_apply_result')}
extern void
${capi.get_name('rewriting_free_apply_result')}(
    ${rewriting_apply_result_type} *result
);

${c_doc('langkit.rewriting.unit_handles')}
extern ${unit_rewriting_handle_type} *
${capi.get_name('rewriting_unit_handles')}(
    ${rewriting_handle_type} handle
);

/* ... unit rewriting... */

${c_doc('langkit.rewriting.unit_handle')}
extern ${unit_rewriting_handle_type}
${capi.get_name('rewriting_unit_to_handle')}(${analysis_unit_type} context);

${c_doc('langkit.rewriting.handle_unit')}
extern ${analysis_unit_type}
${capi.get_name('rewriting_handle_to_unit')}(
    ${unit_rewriting_handle_type} handle
);

${c_doc('langkit.rewriting.root')}
extern ${node_rewriting_handle_type}
${capi.get_name('rewriting_unit_root')}(
    ${unit_rewriting_handle_type} handle
);

${c_doc('langkit.rewriting.set_root')}
extern void
${capi.get_name('rewriting_unit_set_root')}(
    ${unit_rewriting_handle_type} handle,
    ${node_rewriting_handle_type} root
);

${c_doc('langkit.rewriting.unit_unparse')}
extern void
${capi.get_name('rewriting_unit_unparse')}(
    ${unit_rewriting_handle_type} handle,
    ${text_type} *result
);

/* ... node rewriting... */

${c_doc('langkit.rewriting.node_handle')}
extern ${node_rewriting_handle_type}
${capi.get_name('rewriting_node_to_handle')}(${node_type} context);

${c_doc('langkit.rewriting.handle_node')}
extern ${node_type}
${capi.get_name('rewriting_handle_to_node')}(
    ${node_rewriting_handle_type} handle
);

${c_doc('langkit.rewriting.node_context')}
extern ${rewriting_handle_type}
${capi.get_name('rewriting_node_to_context')}(
    ${node_rewriting_handle_type} handle
);

${c_doc('langkit.rewriting.unparse')}
extern void
${capi.get_name('rewriting_node_unparse')}(
    ${node_rewriting_handle_type} handle,
    ${text_type} *result
);

${c_doc('langkit.rewriting.kind')}
extern ${node_kind_type}
${capi.get_name('rewriting_kind')}(${node_rewriting_handle_type} handle);

${c_doc('langkit.rewriting.node_image')}
extern void
${capi.get_name('rewriting_node_image')}(
    ${node_rewriting_handle_type} handle,
    ${text_type} *result
);

${c_doc('langkit.rewriting.tied')}
extern int
${capi.get_name('rewriting_tied')}(${node_rewriting_handle_type} handle);

${c_doc('langkit.rewriting.parent')}
extern ${node_rewriting_handle_type}
${capi.get_name('rewriting_parent')}(${node_rewriting_handle_type} handle);

${c_doc('langkit.rewriting.children_count')}
extern int
${capi.get_name('rewriting_children_count')}(
    ${node_rewriting_handle_type} handle
);

${c_doc('langkit.rewriting.child_by_ref')}
extern ${node_rewriting_handle_type}
${capi.get_name('rewriting_child')}(
    ${node_rewriting_handle_type} handle,
    ${introspection_member_ref_type} field
);

${c_doc('langkit.rewriting.children')}
extern void
${capi.get_name('rewriting_children')}(
    ${node_rewriting_handle_type} handle,
    ${node_rewriting_handle_type} **children,
    int *count
);

${c_doc('langkit.rewriting.set_child_by_ref')}
extern void
${capi.get_name('rewriting_set_child')}(
    ${node_rewriting_handle_type} handle,
    ${introspection_member_ref_type} field,
    ${node_rewriting_handle_type} child
);

${c_doc('langkit.rewriting.text')}
extern void
${capi.get_name('rewriting_text')}(
    ${node_rewriting_handle_type} handle,
    ${text_type} *result
);

${c_doc('langkit.rewriting.set_text')}
extern void
${capi.get_name('rewriting_set_text')}(
    ${node_rewriting_handle_type} handle,
    ${text_type} *text
);

${c_doc('langkit.rewriting.replace')}
extern void
${capi.get_name('rewriting_replace')}(
    ${node_rewriting_handle_type} handle,
    ${node_rewriting_handle_type} new_node
);

${c_doc('langkit.rewriting.rotate')}
extern void
${capi.get_name('rewriting_rotate')}(
    ${node_rewriting_handle_type} *handles,
    int count
);

/* ... list node rewriting... */

${c_doc('langkit.rewriting.first_child')}
extern ${node_rewriting_handle_type}
${capi.get_name('rewriting_first_child')}(
    ${node_rewriting_handle_type} handle
);

${c_doc('langkit.rewriting.last_child')}
extern ${node_rewriting_handle_type}
${capi.get_name('rewriting_last_child')}(
    ${node_rewriting_handle_type} handle
);

${c_doc('langkit.rewriting.next_child')}
extern ${node_rewriting_handle_type}
${capi.get_name('rewriting_next_child')}(
    ${node_rewriting_handle_type} handle
);

${c_doc('langkit.rewriting.previous_child')}
extern ${node_rewriting_handle_type}
${capi.get_name('rewriting_previous_child')}(
    ${node_rewriting_handle_type} handle
);

${c_doc('langkit.rewriting.insert_before')}
extern void
${capi.get_name('rewriting_insert_before')}(
    ${node_rewriting_handle_type} handle,
    ${node_rewriting_handle_type} new_sibling
);

${c_doc('langkit.rewriting.insert_after')}
extern void
${capi.get_name('rewriting_insert_after')}(
    ${node_rewriting_handle_type} handle,
    ${node_rewriting_handle_type} new_sibling
);

${c_doc('langkit.rewriting.insert_first')}
extern void
${capi.get_name('rewriting_insert_first')}(
    ${node_rewriting_handle_type} handle,
    ${node_rewriting_handle_type} new_sibling
);

${c_doc('langkit.rewriting.insert_last')}
extern void
${capi.get_name('rewriting_insert_last')}(
    ${node_rewriting_handle_type} handle,
    ${node_rewriting_handle_type} new_sibling
);

${c_doc('langkit.rewriting.remove_child')}
extern void
${capi.get_name('rewriting_remove_child')}(
    ${node_rewriting_handle_type} handle
);

/* ... node creation... */

${c_doc('langkit.rewriting.clone')}
extern ${node_rewriting_handle_type}
${capi.get_name('rewriting_clone')}(${node_rewriting_handle_type} handle);

${c_doc('langkit.rewriting.create_node')}
extern ${node_rewriting_handle_type}
${capi.get_name('rewriting_create_node')}(
    ${rewriting_handle_type} handle,
    ${node_kind_type} kind
);

${c_doc('langkit.rewriting.create_token_node')}
extern ${node_rewriting_handle_type}
${capi.get_name('rewriting_create_token_node')}(
    ${rewriting_handle_type} handle,
    ${node_kind_type} kind,
    ${text_type} *text
);

${c_doc('langkit.rewriting.create_regular_node')}
extern ${node_rewriting_handle_type}
${capi.get_name('rewriting_create_regular_node')}(
    ${rewriting_handle_type} handle,
    ${node_kind_type} kind,
    ${node_rewriting_handle_type} *children,
    int count
);

${c_doc('langkit.rewriting.create_from_template')}
extern ${node_rewriting_handle_type}
${capi.get_name('rewriting_create_from_template')}(
    ${rewriting_handle_type} handle,
    ${text_type} *src_template,
    ${node_rewriting_handle_type} *arguments,
    int count,
    ${grammar_rule_type} rule
);
% endif

${exts.include_extension(ctx.ext('analysis', 'c_api', 'header'))}

#ifdef __cplusplus
}
#endif

#endif
