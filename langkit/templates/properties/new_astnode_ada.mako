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

% for name, fld_expr in expr._iter_ordered():
   ${result}.${name} :=
      ${fld_expr.type.convert_to_storage_expr(result, fld_expr.render_expr())};
% endfor
