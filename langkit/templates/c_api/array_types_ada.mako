## vim: filetype=makoada

<%def name="decl(cls)">

<%
   ada_type_name = cls.name().camel_with_underscores
   c_type_name = cls.c_type(capi).name
   dec_ref = cls.c_dec_ref(capi)
%>

type ${c_type_name}_Ptr is access ${ada_type_name};

procedure ${dec_ref} (A : ${ada_type_name})
   with Export        => True,
        Convention    => C,
        External_name => "${dec_ref}";

</%def>

<%def name="body(cls)">

<%
   ada_type_name = cls.name().camel_with_underscores
   c_type_name = cls.c_type(capi).name
   dec_ref = cls.c_dec_ref(capi)
%>

procedure ${dec_ref} (A : ${ada_type_name}) is
   A_Var : ${ada_type_name} := A;
begin
   Dec_Ref (A_Var);
end ${dec_ref};

</%def>
