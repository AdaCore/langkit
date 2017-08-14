## vim: filetype=makoada

function ${property.name}
  (E : Entity
   % for arg in property.natural_arguments:
      ; ${arg.name} : ${arg.type.name}
   % endfor
  ) return ${property.untyped_wrapper_rtype.name}
is
   <%
      uses_einfo = property.uses_entity_info
      args = [str(arg.name) for arg in property.natural_arguments]
   %>

   % if uses_einfo:
      E_Info : Entity_Info := Shed_Rebindings (E.Info, E.El.Node_Env);
      <% args.append('E_Info') %>
   % endif

   Result : constant ${property.untyped_wrapper_rtype.name} :=
      ${Self.type.name} (E.El).${property.name}
         ${'({})'.format(', '.join(args)) if args else ''};
begin
   % if uses_einfo:
      Dec_Ref (E_Info);
   % endif
   return Result;
end;
