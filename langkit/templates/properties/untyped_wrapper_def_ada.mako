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
      args = (['E.Node'] +
              [str(arg.name) for arg in property.natural_arguments])
   %>

   % if uses_einfo:
      E_Info : ${T.entity_info.name} :=
         Shed_Rebindings (E.Info, Node_Env (E.Node));
      <% args.append('E_Info') %>
   % endif

   Result : constant ${property.type.name} :=
      ${property.name} (${', '.join(args)});
begin
   % if property.type.is_entity_type:
      return (Node => Result.Node, Info => Result.Info);
   % else:
      return Result;
   % endif
end;
