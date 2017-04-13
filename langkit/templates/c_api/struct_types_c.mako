## vim: filetype=makocpp

<%def name="decl(cls)">

<%
   type_name = cls.c_type(capi).name
%>

typedef struct {
    % for f in cls.get_fields():
        ${f.type.c_type(capi).name} ${f.name};
    % endfor
} ${type_name};

/* Increment the ref-count of all components in "r".  */
extern void
${cls.c_inc_ref(capi)}(${type_name} *r);

/* Decrement the ref-count of all components in "r".  */
extern void
${cls.c_dec_ref(capi)}(${type_name} *r);

</%def>
