## vim: filetype=makoada

${expr.render_pre()}
${result_var.name} := ${expr.render_expr()};
% if expr.type.is_ptr:
   if ${result_var.name} = null then
      raise Property_Error with "dereferencing a null access";
   end if;
% endif
