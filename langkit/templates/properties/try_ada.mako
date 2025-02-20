## vim: filetype=makoada

<%namespace name="helpers" file="helpers.mako" />

begin
   ${expr.try_expr.render_pre()}
   ${expr.result_var.codegen_name} := ${expr.try_expr.render_expr()};
exception
   when Property_Error =>
      ${expr.else_expr.render_pre()}
      ${expr.result_var.codegen_name} := ${expr.else_expr.render_expr()};
end;
${helpers.inc_ref(expr.result_var.ref_expr)}
