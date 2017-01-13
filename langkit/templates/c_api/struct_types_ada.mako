## vim: filetype=makoada

<%def name="decl(cls)">

<%
   ada_type_name = cls.name().camel_with_underscores
   c_type_name = cls.c_type(capi).name
   dec_ref = cls.c_dec_ref(capi)
%>

type ${c_type_name}_Ptr is access ${ada_type_name};

% if cls.is_refcounted():
procedure ${dec_ref} (R : ${c_type_name}_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "${dec_ref}";
% endif

</%def>

<%def name="body(cls)">

<%
   ada_type_name = cls.name().camel_with_underscores
   c_type_name = cls.c_type(capi).name
   dec_ref = cls.c_dec_ref(capi)
%>

% if cls.is_refcounted():
procedure ${dec_ref} (R : ${c_type_name}_Ptr) is
begin
   Dec_Ref (R.all);
end ${dec_ref};
% endif

</%def>
