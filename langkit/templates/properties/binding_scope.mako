## vim: filetype=makoada

% for binding in expr.expr_bindings:
   ${gdb_bind_var(binding)}
% endfor
${expr.expr.render_pre()}
