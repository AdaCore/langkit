## vim: filetype=makoada

% if not ctx.no_property_checks:

   ## Output a null check only if the expression can be null
   <%
      if expr.type.is_ptr and expr.type.null_allowed:
         operand = expr.render_expr()
      elif implicit_deref:
         operand = '{}.El'.format(expr.render_expr())
      else:
         operand = None
   %>

   % if operand:
      if ${operand} = null then
         raise Property_Error with "dereferencing a null access";
      end if;
   % endif

% endif

## The laws of ref-counting tells us to create an ownership share for our
## result.
% if expr.type.is_refcounted():
   Inc_Ref (${expr.render_expr()});
% endif
