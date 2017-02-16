## vim: filetype=makoada

<%namespace name="helpers" file="helpers.mako" />

${expr.cond.render_pre()}
if ${expr.cond.render_expr()} then
   ${expr.then.render_pre()}
   ${expr._result_var.name} := ${expr.then.render_expr()};
else
   ${expr.else_then.render_pre()}
   ${expr._result_var.name} := ${expr.else_then.render_expr()};
end if;
${helpers.inc_ref(expr._result_var)}
