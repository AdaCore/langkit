## vim: filetype=makoada

${expr.cond.render_pre()}
if ${expr.cond.render_expr()} then
   ${expr.then.render_pre()}
   ${expr.result_var.name} := ${expr.then.render_expr()};
else
   ${expr.else_then.render_pre()}
   ${expr.result_var.name} := ${expr.else_then.render_expr()};
end if;
