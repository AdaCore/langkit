## vim: filetype=makocpp

<%def name="decl(cls)">

<% type_name = cls.c_type(capi).name %>

${c_doc(cls)}
typedef struct {
   int n;
   int ref_count;
   ${cls.element_type().c_type(capi).name} items[1];
} *${type_name};

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
${cls.c_dec_ref(capi)}(${type_name} a);

</%def>
