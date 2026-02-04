## vim: filetype=makoada

function ${property.names.codegen}
  (E : Internal_Entity
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
      E_Info : ${T.EntityInfo.name} :=
         AST_Envs.Shed_Rebindings (E.Info, Children_Env (E.Node));
      <% args.append('E_Info') %>
   % endif
begin
   ## If it is possible for the caller to pass an entity whose type mismatches
   ## what this property excepts (i.e. if we accept less than the root node and
   ## if there is more than one concrete node), reject it explicitly.
   <% expected_type = property.owner %>
   % if not expected_type.is_root_node and len(ctx.kind_constant_to_node) > 1:
      if E.Node /= null
         and then E.Node.Kind not in ${property.owner.ada_kind_range_name}
      then
         Raise_Property_Exception
           (E.Node,
            Property_Error'Identity,
            "mismatching node type");
      end if;
   % endif

   declare
      Result : constant ${property.type.name} :=
         ${property.names.codegen} (${', '.join(args)});
   begin
      % if property.type.is_entity_type:
         return (Node => Result.Node, Info => Result.Info);
      % else:
         return Result;
      % endif
   end;
end;
