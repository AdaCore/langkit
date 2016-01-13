## vim: filetype=makocpp

<%namespace name="array_types"   file="array_types_c.mako" />
<%namespace name="astnode_types" file="astnode_types_c.mako" />
<%namespace name="enum_types"    file="enum_types_c.mako" />

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
    /* TODO: do we keep a single node kind for all lists or should we
       specialize them?  */
    ${capi.get_name(Name("List"))} = 1,
% for astnode in _self.astnode_types:
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

${c_doc('langkit.token_type')}
typedef void* ${token_type};

% for rec in _self.struct_types:
   typedef struct {
      % for f in rec.get_fields():
         ${f.type.c_type(capi).name} ${f.name};
      % endfor
   } ${rec.c_type(capi).name};
% endfor

/* Helper data structures for source location handling.  */

typedef struct {
    uint32_t line;
    uint16_t column;
} ${sloc_type};

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
} ${text_type};


${c_doc('langkit.diagnostic_type')}
typedef struct {
    ${sloc_range_type} sloc_range;
    ${text_type} message;
} ${diagnostic_type};


${c_doc('langkit.exception_type')}
typedef struct {
   ${c_doc('langkit.exception_type.information')}
   const char *information;
} ${exception_type};

/* All the functions below can potentially raise an exception, so
   ${capi.get_name("get_last_exception")} must be checked after them even
   before trying to use the returned value.  */


/*
 * Data structures held in AST nodes
 */


% for enum_type in _self.sorted_types(_self.enum_types):
    ${enum_types.decl(enum_type)}
% endfor

% for array_type in _self.sorted_types(_self.array_types):
    ${array_types.decl(array_type)}
% endfor


/*
 * Analysis primitives
 */

${c_doc('langkit.create_context')}
extern ${analysis_context_type}
${capi.get_name("create_analysis_context")}(const char *charset);

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
        int reparse);

${c_doc('langkit.get_unit_from_buffer')}
extern ${analysis_unit_type}
${capi.get_name("get_analysis_unit_from_buffer")}(
        ${analysis_context_type} context,
        const char *filename,
        const char *charset,
        const char *buffer,
        size_t buffer_size);

${c_doc('langkit.remove_unit')}
extern int
${capi.get_name("remove_analysis_unit")}(${analysis_context_type} context,
                                         const char *filename);

${c_doc('langkit.unit_root')}
extern ${node_type}
${capi.get_name("unit_root")}(${analysis_unit_type} unit);

${c_doc('langkit.unit_diagnostic_count')}
extern unsigned
${capi.get_name("unit_diagnostic_count")}(${analysis_unit_type} unit);

${c_doc('langkit.unit_diagnostic')}
extern int
${capi.get_name("unit_diagnostic")}(${analysis_unit_type} unit,
                                    unsigned n,
                                    ${diagnostic_type} *diagnostic_p);

${c_doc('langkit.unit_incref')}
extern ${analysis_unit_type}
${capi.get_name("unit_incref")}(${analysis_unit_type} unit);

${c_doc('langkit.unit_decref')}
extern void
${capi.get_name("unit_decref")}(${analysis_unit_type} unit);

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
extern void
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

${c_doc('langkit.node_child')}
extern ${text_type}
${capi.get_name("token_text")}(${token_type} token);

${c_doc('langkit.text_to_locale_string')}
extern char *
${capi.get_name("text_to_locale_string")}(${text_type} text);

${c_doc('langkit.free')}
extern void
${capi.get_name("free")}(void *address);


/*
 * Kind-specific AST node primitives
 */

/* All these primitives return their result through an OUT parameter.  They
   return a boolean telling whether the operation was successful (it can fail
   if the node does not have the proper type, for instance).  When an AST node
   is returned, its ref-count is left as-is.  */

% for astnode in _self.astnode_types:
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

/*
 * Misc
 */

${c_doc('langkit.get_last_exception')}
extern const ${exception_type} *
${capi.get_name('get_last_exception')}(void);

#ifdef __cplusplus
}
#endif

#endif
