## vim: filetype=makoada

<%namespace name="scopes" file="scopes_ada.mako" />

${scopes.start_scope(expr.scope)}

% for binding in expr.expr_bindings:
   ${gdb_bind_var(binding)}
% endfor

${expr.expr.render_pre()}
${expr.result_var.name} := ${expr.expr.render_expr()};
% if expr.type.is_refcounted:
   Inc_Ref (${expr.result_var.name});
% endif

${scopes.finalize_scope(expr.scope)}
