## vim: filetype=makoada

<%namespace name="helpers" file="helpers.mako" />

${then.expr.render_pre()}
${then.var_expr.name} := ${then.expr.render_expr()};
${helpers.inc_ref(then.var_expr)}

<%
cond_expr = then.var_expr.render_expr()
cond_type = then.var_expr.type
%>

if ${cond_expr} /= ${cond_type.nullexpr} then
   ${then.then_expr.render_pre()}
   ${then.result_var.codegen_name} := ${then.then_expr.render_expr()};
else
   ${then.default_expr.render_pre()}
   ${then.result_var.codegen_name} := ${then.default_expr.render_expr()};
end if;
${helpers.inc_ref(then.result_var.ref_expr)}
