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
      args = ([Self.type.internal_conversion(T.root_node, 'E.Node')] +
              [str(arg.name) for arg in property.natural_arguments])
   %>

   % if uses_einfo:
      E_Info : ${T.entity_info.name} :=
         Shed_Rebindings (E.Info, Node_Env (E.Node));
      <% args.append('E_Info') %>
   % endif

   <%
      property_call = '{} ({})'.format(property.name, ', '.join(args))
      rtype = property.untyped_wrapper_rtype
   %>
   Result : constant ${rtype.name} :=
      % if rtype.is_ast_node:
         ${T.root_node.internal_conversion(Self.type, property_call)}
      % else:
         ${property_call}
      % endif
   ;
begin
   return Result;
end;
