## vim: filetype=makoada

function ${property.name}
  (E : Entity) return ${property.untyped_wrapper_rtype.name}
is
   <% uses_einfo = property.uses_entity_info %>

   % if uses_einfo:
      E_Info : Entity_Info := Shed_Rebindings (E.Info, E.El.Node_Env);
   % endif

   Result : constant ${property.untyped_wrapper_rtype.name} :=
      ${Self.type.name} (E.El).${property.name}
         ${'(E_Info)' if uses_einfo else ''};
begin
   % if uses_einfo:
      Dec_Ref (E_Info);
   % endif
   return Result;
end;
