## vim: filetype=makoada

${expr.expr.render_pre()}
${expr.expr_var.name} := ${expr.expr.render_expr()};
if ${expr.expr_var.name} = null
     or else
   ${expr.expr_var.name}.all in ${expr.astnode.name()}_Type'Class
then
   ${expr.result_var.name} := ${expr.astnode.name()} (${expr.expr_var.name});
else
   % if expr.do_raise:
   raise Property_Error;
   % else:
   ${expr.result_var.name} := null;
   % endif
end if;
