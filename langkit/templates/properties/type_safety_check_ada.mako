## vim: filetype=makoada

${expr.render_pre()}
${result_var.name} := ${expr.render_expr()};
if ${result_var.name}.all not in ${astnode.name()}_Type'Class then
   raise Property_Error;
end if;
