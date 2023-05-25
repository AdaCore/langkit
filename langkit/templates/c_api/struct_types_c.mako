## vim: filetype=makocpp

<%def name="decl(cls)">

<% type_name = cls.c_type(capi).name %>

% if cls.is_empty:
    typedef struct {char dummy;} ${type_name};
% else:
    typedef struct {
        % for f in cls.get_fields():
            ${f.type.c_type(capi).name} ${f.name};
        % endfor
    } ${type_name};
% endif

% if cls.is_refcounted:
    /* Increment the ref-count of all components in "r".  */
    extern void
    ${cls.c_inc_ref(capi)}(${type_name} *r);

    /* Decrement the ref-count of all components in "r".  */
    extern void
    ${cls.c_dec_ref(capi)}(${type_name} *r);
% endif

</%def>
