## vim: filetype=makoada

${expr.expr.render_pre()}

<%def name="assign_field(field, field_expr)">
   <% field_ref = f"{expr.result_var.codegen_name}.{field.names.codegen}" %>
   ${field_ref} := ${field_expr};

   ## Do not forget to create a new refcount share for each field, as the above
   ## does a copy.
   % if field.type.is_refcounted:
      Inc_Ref (${field_ref});
   % endif
</%def>

## Fill fields to copy, one by one
% for _, field in sorted(expr.static_type.required_fields_in_exprs.items()):
   % if field not in expr.assocs:
      ${assign_field(field, '{}.{}'.format(expr.expr.render_expr(),
                                           field.names.codegen))}
   % endif
% endfor

## Likewise, fill fields to update
% for field, field_expr in expr.assocs.items():
   ${field_expr.render_pre()}
   ${assign_field(field, field_expr.render_expr())}
% endfor
