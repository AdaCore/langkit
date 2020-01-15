## vim: filetype=makoada

${expr.expr.render_pre()}

## Fill fields to update, one by one
% for field, field_expr in expr.assocs.items():
   <% field_ref = '{}.{}'.format(expr.result_var.name, field.name) %>
   ${field_expr.render_pre()}
   ${field_ref} := ${field_expr.render_expr()};

   ## Do not forget to create a new refcount share for each field, as the above
   ## does a copy.
   % if field.type.is_refcounted:
      Inc_Ref (${field_ref});
   % endif
% endfor
