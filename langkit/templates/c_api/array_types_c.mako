## vim: filetype=makocpp

<%def name="decl(cls)">

${c_doc(cls)}
typedef struct {
   int n;
   ${cls.element_type().c_type(capi).name} items[1];
} *${cls.c_type(capi).name};

</%def>
