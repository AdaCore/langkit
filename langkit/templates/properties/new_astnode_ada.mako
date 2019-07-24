## vim: filetype=makoada

<% result = expr.result_var.name %>

${result} := new ${expr.static_type.value_type_name()};
declare
   Result_As_Root_Node : constant ${root_node_type_name} :=
      ${T.root_node.internal_conversion(expr.static_type, result)};
begin
   Initialize
     (Self => Result_As_Root_Node,
      Kind => ${expr.static_type.ada_kind_name},
      Unit => Self_As_Root_Node.Unit,

      ## Keep the token start/end null, as expected for a synthetized node
      Token_Start_Index => No_Token_Index,
      Token_End_Index   => No_Token_Index,

      ## We consider the creator of a synthetized nodes as its parent even
      ## though the latter is not a regular child.
      Parent => Self_As_Root_Node,

      ## The node's env is the same as the parent
      Self_Env => Self_As_Root_Node.Self_Env);
   Register_Destroyable (Self_As_Root_Node.Unit, Result_As_Root_Node);
end;

<%
   parse_field_assocs = []
   user_field_assocs = []

   for f, e in expr._iter_ordered():
      dest = (user_field_assocs
              if f.is_user_field else
              parse_field_assocs)
      dest.append((f, e))
%>

## Initialize parse fields using the standard initialize procedure
% if parse_field_assocs:
   Initialize_Fields_For_${expr.type.kwless_raw_name}
     (Self => ${result},
      ${', '.join('{} => {}'.format(field.name, field_expr.render_expr())
                  for field, field_expr in parse_field_assocs)});
% endif

## Then initialize user fields individually
% if user_field_assocs:
   % for field, field_expr in user_field_assocs:
      ${field.struct.internal_conversion(expr.type, result)}.${field.name} :=
         ${field.type.convert_to_storage_expr(result,
                                              field_expr.render_expr())};
   % endfor
% endif
