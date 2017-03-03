## vim: filetype=makocpp

<%namespace name="array_types"   file="array_types_c.mako" />
<%namespace name="struct_types"  file="struct_types_c.mako" />
<%namespace name="astnode_types" file="astnode_types_c.mako" />
<%namespace name="enum_types"    file="enum_types_c.mako" />
<%namespace name="exts" file="../extensions.mako" />

#ifndef ${capi.header_guard_id}
#define ${capi.header_guard_id}

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

${c_doc('langkit.initialize')}
void ${capi.lib_name}_initialize(void);

${c_doc('langkit.analysis_context_type')}
typedef void* ${analysis_context_type};

${c_doc('langkit.analysis_unit_type')}
typedef void* ${analysis_unit_type};

${c_doc('langkit.node_type')}
typedef void* ${node_type};

${c_doc('langkit.node_kind_type')}
typedef enum {
% for astnode in ctx.astnode_types:
    % if astnode.abstract:

        /* ${astnode.name()} (abstract)  */
        ${c_doc(astnode, 8)}
    % else:

        ${c_doc(astnode, 8)}
        ${capi.get_name(astnode.name())}
          = ${ctx.node_kind_constants[astnode]},
    % endif
% endfor
} ${node_kind_type};

% if ctx.library_fields_all_public:
${c_doc('langkit.lexical_env_type')}
typedef void *${lexical_env_type};

${c_doc('langkit.logic_var_type')}
typedef void *${logic_var_type};

${c_doc('langkit.equation_type')}
typedef void *${equation_type};
% endif

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

${c_doc('langkit.token_kind')}
typedef enum {
   <% lexer = ctx.lexer %>
   % for i, t in enumerate(lexer.sorted_tokens):
      ${',' if i > 0 else ''}
      ${lexer.c_token_name(t)} = ${t.value}
   % endfor
} ${token_kind};

${c_doc('langkit.token_type')}
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


${c_doc('langkit.exception_type')}
typedef struct {
   ${c_doc('langkit.exception_type.is_fatal')}
   int is_fatal;

   ${c_doc('langkit.exception_type.information')}
   const char *information;
} ${exception_type};

% for struct_type in ctx.struct_types:
   ${struct_types.decl(struct_type)}
% endfor

% if ctx.default_unit_file_provider:
/*
 * Types for unit file providers
 */

${c_doc('langkit.unit_kind_type')}
typedef enum {
   ${capi.get_name('unit_kind_specification')},
   ${capi.get_name('unit_kind_body')}
} ${unit_kind_type};

${c_doc('langkit.unit_file_provider_type')}
typedef void *${unit_file_provider_type};

${c_doc('langkit.unit_file_provider_destroy_type')}
typedef void (*${unit_file_provider_destroy_type})(void *data);

${c_doc('langkit.unit_file_provider_get_file_from_node_type')}
typedef char *(*${unit_file_provider_get_file_from_node_type})(
   void *data,
   ${node_type} node,
   ${unit_kind_type} kind
);

${c_doc('langkit.unit_file_provider_get_file_from_name_type')}
typedef char *(*${unit_file_provider_get_file_from_name_type})(
   void *data,
   ${text_type} name,
   ${unit_kind_type} kind
);
% endif

/* All the functions below can potentially raise an exception, so
   ${capi.get_name("get_last_exception")} must be checked after them even
   before trying to use the returned value.  */


/*
 * Data structures held in AST nodes
 */


% for enum_type in ctx.sorted_types(ctx.enum_types):
    ${enum_types.decl(enum_type)}
% endfor

% for array_type in ctx.sorted_types(ctx.array_types):
    % if array_type.element_type()._exposed or ctx.library_fields_all_public:
        ${array_types.decl(array_type)}
    % endif
% endfor


/*
 * Analysis primitives
 */

${c_doc('langkit.create_context')}
extern ${analysis_context_type}
${capi.get_name("create_analysis_context")}(
   const char *charset
   % if ctx.default_unit_file_provider:
   , ${unit_file_provider_type} unit_file_provider
   % endif
);

${c_doc('langkit.context_incref')}
extern ${analysis_context_type}
${capi.get_name("context_incref")}(${analysis_context_type} context);

${c_doc('langkit.context_decref')}
extern void
${capi.get_name("context_decref")}(${analysis_context_type} context);

${c_doc('langkit.destroy_context')}
extern void
${capi.get_name("destroy_analysis_context")}(
        ${analysis_context_type} context);

${c_doc('langkit.get_unit_from_file')}
extern ${analysis_unit_type}
${capi.get_name("get_analysis_unit_from_file")}(
        ${analysis_context_type} context,
        const char *filename,
        const char *charset,
        int reparse,
        int with_trivia);

${c_doc('langkit.get_unit_from_buffer')}
extern ${analysis_unit_type}
${capi.get_name("get_analysis_unit_from_buffer")}(
        ${analysis_context_type} context,
        const char *filename,
        const char *charset,
        const char *buffer,
        size_t buffer_size,
        int with_trivia);

% if ctx.default_unit_file_provider:
${c_doc('langkit.get_unit_from_provider')}
extern ${analysis_unit_type}
${capi.get_name("get_analysis_unit_from_provider")}(
        ${analysis_context_type} context,
        ${text_type} name,
        ${unit_kind_type} kind,
        const char *charset,
        int reparse,
        int with_trivia);
% endif

${c_doc('langkit.remove_unit')}
extern int
${capi.get_name("remove_analysis_unit")}(${analysis_context_type} context,
                                         const char *filename);

${c_doc('langkit.unit_root')}
extern ${node_type}
${capi.get_name("unit_root")}(${analysis_unit_type} unit);

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

${c_doc('langkit.node_unit')}
extern ${analysis_unit_type}
${capi.get_name("node_unit")}(${node_type} node);

${c_doc('langkit.unit_incref')}
extern ${analysis_unit_type}
${capi.get_name("unit_incref")}(${analysis_unit_type} unit);

${c_doc('langkit.unit_decref')}
extern void
${capi.get_name("unit_decref")}(${analysis_unit_type} unit);

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

${c_doc('langkit.node_kind')}
extern ${node_kind_type}
${capi.get_name("node_kind")}(${node_type} node);

${c_doc('langkit.kind_name')}
extern ${text_type}
${capi.get_name("kind_name")}(${node_kind_type} kind);

${c_doc('langkit.node_is_ghost')}
extern int
${capi.get_name("node_is_ghost")}(${node_type} node);

${c_doc('langkit.node_short_image')}
extern ${text_type}
${capi.get_name("node_short_image")}(${node_type} node);

${c_doc('langkit.node_sloc_range')}
extern void
${capi.get_name("node_sloc_range")}(${node_type} node,
                                    ${sloc_range_type} *sloc_range);

${c_doc('langkit.lookup_in_node')}
extern ${node_type}
${capi.get_name("lookup_in_node")}(${node_type} node,
                                   const ${sloc_type} *sloc);

${c_doc('langkit.node_child_count')}
extern unsigned
${capi.get_name("node_child_count")}(${node_type} node);

${c_doc('langkit.node_child')}
extern int
${capi.get_name("node_child")}(${node_type} node,
                               unsigned n,
                               ${node_type}* child_p);

${c_doc('langkit.text_to_locale_string')}
extern char *
${capi.get_name("text_to_locale_string")}(${text_type} text);

${c_doc('langkit.free')}
extern void
${capi.get_name("free")}(void *address);

${c_doc('langkit.destroy_text')}
extern void
${capi.get_name("destroy_text")}(${text_type} *text);

% if ctx.library_fields_all_public:
/* Lexical environment primitives */

${c_doc('langkit.lexical_env_parent')}
extern ${lexical_env_type}
${capi.get_name('lexical_env_parent')}(${lexical_env_type} env);

${c_doc('langkit.lexical_env_node')}
extern ${node_type}
${capi.get_name('lexical_env_node')}(${lexical_env_type} env);

${c_doc('langkit.lexical_env_get')}
extern ${T.root_node.env_el().array_type().c_type(capi).name}
${capi.get_name('lexical_env_get')}(${lexical_env_type} env,
                                    ${text_type} name);

/* Decrement the ref-count for "env". This deallocates it if the ref-count
   drops to 0.  */
extern void
${capi.get_name('lexical_env_dec_ref')}(${lexical_env_type} env);
% endif

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
 * Extensions handling
 */

${c_doc('langkit.extensions_handling')}

${c_doc('langkit.node_extension_destructor')}
typedef void (*${capi.get_name("node_extension_destructor")})(${node_type} node,
                                                              void *extension);

${c_doc('langkit.register_extension')}
extern unsigned
${capi.get_name("register_extension")}(const char *name);

${c_doc('langkit.node_extension')}
extern void **
${capi.get_name("node_extension")}(
    ${node_type} node,
    unsigned ext_id,
    ${capi.get_name("node_extension_destructor")} dtor
);

% if ctx.default_unit_file_provider:
/*
 * Unit file providers
 */

extern ${unit_file_provider_type}
${capi.get_name('create_unit_file_provider')}(
   void *data,
   ${unit_file_provider_destroy_type} destroy_func,
   ${unit_file_provider_get_file_from_node_type} get_file_from_node_func,
   ${unit_file_provider_get_file_from_name_type} get_file_from_name_func
);

extern void
${capi.get_name('destroy_unit_file_provider')}(void *data);

${exts.include_extension(
   ctx.ext('analysis', 'c_api', 'unit_file_providers', 'header')
)}
% endif

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

#ifdef __cplusplus
}
#endif

#endif
