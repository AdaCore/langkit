## vim: filetype=makoada

<%def name="decl(cls)">

<%
   ada_type_name = cls.name().camel_with_underscores
   c_type_name = cls.c_type(capi).name
   inc_ref = cls.c_inc_ref(capi)
   dec_ref = cls.c_dec_ref(capi)
%>

subtype ${c_type_name} is ${ada_type_name};
type ${c_type_name}_Ptr is access ${ada_type_name};

procedure ${inc_ref} (A : ${ada_type_name})
   with Export        => True,
        Convention    => C,
        External_name => "${inc_ref}";

procedure ${dec_ref} (A : ${ada_type_name})
   with Export        => True,
        Convention    => C,
        External_name => "${dec_ref}";

</%def>

<%def name="body(cls)">

<%
   ada_type_name = cls.name().camel_with_underscores
   c_type_name = cls.c_type(capi).name
   inc_ref = cls.c_inc_ref(capi)
   dec_ref = cls.c_dec_ref(capi)
%>

procedure ${inc_ref} (A : ${ada_type_name}) is
begin
   Clear_Last_Exception;
   Inc_Ref (A);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end;

procedure ${dec_ref} (A : ${ada_type_name}) is
begin
   Clear_Last_Exception;
   declare
      A_Var : ${ada_type_name} := A;
   begin
      Dec_Ref (A_Var);
   end;
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end;

</%def>
