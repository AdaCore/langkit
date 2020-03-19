## vim: filetype=makocpp

<%def name="incomplete_decl(cls)">
<% type_name = cls.c_type(capi).name %>
${c_doc(cls)}
typedef void* ${type_name};
</%def>

<%def name="decl(cls)">

<% type_name = cls.c_type(capi).name %>

/* Set the next value of the iterator in the given element pointer.
   Return 1 if successful, otherwise 0.  */
extern int
${cls.c_next(capi)}(${type_name} i, ${cls.element_type.c_type(capi).name}* e);

/* Increment the ref-count for "i".  */
extern void
${cls.c_inc_ref(capi)}(${type_name} i);

/* Decrement the ref-count for "i". This deallocates it if the ref-count drops
   to 0.  */
extern void
${cls.c_dec_ref(capi)}(${type_name} i);

</%def>
