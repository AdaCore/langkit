## vim: filetype=makoada

<%def name="decl(cls)">

<%
   ada_type_name = cls.name.camel_with_underscores
   c_type_name = cls.c_type(capi).name
   inc_ref = cls.c_inc_ref(capi)
   dec_ref = cls.c_dec_ref(capi)
%>

## Define the C type name as an alias for the Ada one for convenience in code
## generation.
subtype ${c_type_name} is ${ada_type_name};
type ${c_type_name}_Ptr is access ${ada_type_name};

% if cls.is_refcounted():
procedure ${inc_ref} (R : ${c_type_name}_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "${inc_ref}";
procedure ${dec_ref} (R : ${c_type_name}_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "${dec_ref}";
% endif

</%def>

<%def name="body(cls)">

<%
   ada_type_name = cls.name.camel_with_underscores
   c_type_name = cls.c_type(capi).name
   inc_ref = cls.c_inc_ref(capi)
   dec_ref = cls.c_dec_ref(capi)
%>

% if cls.is_refcounted():
procedure ${inc_ref} (R : ${c_type_name}_Ptr) is
begin
   Clear_Last_Exception;
   Inc_Ref (R.all);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end ${inc_ref};

procedure ${dec_ref} (R : ${c_type_name}_Ptr) is
begin
   Clear_Last_Exception;
   Dec_Ref (R.all);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end ${dec_ref};
% endif

</%def>
