## vim: filetype=makoada

<%def name="decl(cls)">

<%
   ada_type_name = cls.name.camel_with_underscores
   c_type_name = cls.c_type(capi).name
   ada_elt_type_name = cls.element_type.name
   get_next = cls.c_next(capi)
   inc_ref = cls.c_inc_ref(capi)
   dec_ref = cls.c_dec_ref(capi)
%>

subtype ${c_type_name} is ${ada_type_name};

function ${get_next}
  (I : ${ada_type_name};
   E : access ${ada_elt_type_name}) return int
   with Export        => True,
        Convention    => C,
        External_name => "${get_next}";

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
   ada_type_name = cls.name.camel_with_underscores
   c_type_name = cls.c_type(capi).name
   ada_elt_type_name = cls.element_type.name
   get_next = cls.c_next(capi)
   inc_ref = cls.c_inc_ref(capi)
   dec_ref = cls.c_dec_ref(capi)
%>

function ${get_next}
  (I : ${ada_type_name};
   E : access ${ada_elt_type_name}) return int is
begin
   Clear_Last_Exception;
   if Next (I, E.all) then
      return 1;
   else
      return 0;
   end if;
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
      return 0;
end ${get_next};

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
