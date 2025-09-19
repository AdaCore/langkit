## vim: filetype=makocpp

<%def name="incomplete_decl(cls)">
<% type_name = cls.c_type(capi).name %>
typedef struct ${type_name}_record *${type_name};
</%def>

<%def name="decl(cls)">

<% type_name = cls.c_type(capi).name %>

${c_doc(cls)}
struct ${type_name}_record {
   int n;
   int ref_count;
   ${cls.element_type.c_type(capi).name} items[1];
};

/* Create a length-sized array.  */
extern ${type_name}
${cls.c_create(capi)}(int length);

/* Increment the ref-count for "a".  */
extern void
${cls.c_inc_ref(capi)}(${type_name} a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
${cls.c_dec_ref(capi)}(${type_name} a);

</%def>
