## vim: filetype=makoada

<% operand = expr.expr %>

${operand.render_pre()}

% if not ctx.no_property_checks:

   ## Output a null check only if the expression can be null
   <%
      if operand.type.is_ptr and operand.type.null_allowed:
         operand_expr = operand.render_expr()
      elif expr.implicit_deref:
         operand_expr = '{}.El'.format(operand.render_expr())
      else:
         operand_expr = None
   %>

   % if operand_expr:
      if ${operand_expr} = null then
         raise Property_Error with "dereferencing a null access";
      end if;
   % endif

% endif

## The laws of ref-counting tell us to create an ownership share for our
## result. If we are storing our result into a result variable, we need to
## inc-ref.
% if expr.type.is_refcounted and expr.result_var:
   ${expr.result_var.name} := ${operand.render_expr()};
   Inc_Ref (${expr.result_var.name});
% endif
