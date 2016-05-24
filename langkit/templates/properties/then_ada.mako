## vim: filetype=makoada

<%namespace name="helpers" file="helpers.mako" />

${then.expr.render_pre()}
${then.var_expr.name} := ${then.expr.render_expr()};
if
   <%
      cond_expr = then.var_expr.render_expr()
      cond_type = then.var_expr.type
   %>
   % if is_struct_type(cond_type) and not cond_type.is_ptr:
      not ${cond_expr}.Is_Null
   % else:
      ${cond_expr} /= ${cond_type.nullexpr()}
   % endif
then
   ${then.then_expr.render_pre()}
   ${then.result_var.name} := ${then.then_expr.render_expr()};
else
   ${then.default_expr.render_pre()}
   ${then.result_var.name} := ${then.default_expr.render_expr()};
end if;
${helpers.inc_ref(then.result_var)}
