## vim: filetype=makoada

${then.expr.render_pre()}
${then.var_expr.name} := ${then.expr.render_expr()};
if ${then.var_expr.render_expr()} /= null then
   ${then.then_expr.render_pre()}
   ${then.result.name} := ${then.then_expr.render_expr()};
else
   ${then.default_expr.render_pre()}
   ${then.result.name} := ${then.default_expr.render_expr()};
end if;
