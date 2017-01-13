## vim: filetype=makocpp

<%def name="decl(cls)">

<%
   type_name = cls.c_type(capi).name
%>

typedef struct {
    % for f in cls.get_fields():
        ${f.type.c_type(capi).name} ${f.name};
    % endfor
   char is_null;
} ${type_name};

/* Decrement the ref-count of all components in "r".  */
extern void
${cls.c_dec_ref(capi)}(${type_name} *r);

</%def>
