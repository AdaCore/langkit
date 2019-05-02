## vim: filetype=makoada

<% result = expr.result_var.name %>

${result} := new ${expr.static_type.value_type_name()};
Initialize
  (Self => ${result},
   Kind => ${expr.static_type.ada_kind_name},
   Unit => Self.Unit,

   ## Keep the token start/end null, as expected for a synthetized node
   Token_Start_Index => No_Token_Index,
   Token_End_Index   => No_Token_Index,

   ## We consider the creator of a synthetized nodes as its parent even though
   ## the latter is not a regular child.
   Parent => ${root_node_type_name} (Self),

   ## The node's env is the same as the parent
   Self_Env => Self.Self_Env);
Register_Destroyable (Self.Unit, ${root_node_type_name} (${result}));

% if expr.assocs:
   ## Initialize parse fields using the standard initialize procedure
   Initialize_Fields_For_${expr.type.kwless_raw_name}
     (Self => ${result},
      ${', '.join('{} => {}'.format(field.name, field_expr.render_expr())
                  for field, field_expr in expr._iter_ordered()
                  if not field.is_user_field)});

   ## Then initialize user fields individually
   % for field, field_expr in expr._iter_ordered():
      % if field.is_user_field:
         ${result}.${field.name} := ${field.type.convert_to_storage_expr(
            result, field_expr.render_expr())};
      % endif
   % endfor
% endif
