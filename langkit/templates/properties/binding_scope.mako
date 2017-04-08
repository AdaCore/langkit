## vim: filetype=makoada

<%namespace name="scopes" file="scopes_ada.mako" />

% if expr.scope:
${scopes.start_scope(expr.scope)}
% endif

% for binding in expr.expr_bindings:
   ${gdb_bind_var(binding)}
% endfor

${expr.expr.render_pre()}
${expr.scope_result_var.name} := ${expr.expr.render_expr()};
% if expr.type.is_refcounted():
   Inc_Ref (${expr.scope_result_var.name});
% endif

% if expr.scope:
${scopes.finalize_scope(expr.scope)}
% endif
